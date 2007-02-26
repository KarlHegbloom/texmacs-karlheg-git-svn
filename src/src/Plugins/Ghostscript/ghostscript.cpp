
/******************************************************************************
* MODULE     : ghostscript.cpp
* DESCRIPTION: interface with ghostscript
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "Ghostscript/ghostscript.hpp"
#include "file.hpp"
#include "image_files.hpp"

/******************************************************************************
* Special hack for a bug in Ghostscript 6.50, 6.51 and 6.52
******************************************************************************/

static int gs_type= -1;
  // -1: uninitialized
  //  0: OK
  //  1: with pixmap bug

bool
ghostscript_bugged () {
#ifdef OS_WIN32
  return false;
#else
  if (gs_type == -1) {
    string gs_version= var_eval_system ("gs --version");
    gs_type= 0;
    if ((gs_version == "6.50") ||
	(gs_version == "6.51") ||
	(gs_version == "6.52"))
      gs_type= 1;
  }
  return (gs_type == 1);
#endif
}

/******************************************************************************
* Drawing a postscript image file in a pixmap
******************************************************************************/

static string
encapsulate_postscript (string s) {
  int i, n= N(s);
  int last_begin= 0;
  string r;
  for (i=0; i<n; i++) {
    if ((s[i] != 's') || (i>(n-8)) || (s(i,i+8) != "showpage")) continue;
    if (i > last_begin) r << s (last_begin, i);
    i += 8;
    last_begin= i;
  }
  r << s (last_begin, i);
  return r;
}

void
ghostscript_run (Display* dpy, Window gs_win, Pixmap pm,
		 url image, SI w, SI h,
		 double cx1, double cy1, double cx2, double cy2)
{
  if (DEBUG_VERBOSE)
    cout << "TeXmacs] Running ghostscript " << image << "\n";
  //cout << "WH : " << w << ", " << h << "\n";

  int bx1, by1, bx2, by2;
  ps_bounding_box (image, bx1, by1, bx2, by2);
  //cout << "BB: " << bx1 << ", " << by1 << ", " << bx2 << ", " << by2 << "\n";
  int x1= bx1 + as_int (cx1 * (bx2 - bx1));
  int y1= by1 + as_int (cy1 * (by2 - by1));
  int x2= bx1 + as_int (cx2 * (bx2 - bx1));
  int y2= by1 + as_int (cy2 * (by2 - by1));
  if (x1+1 < x2) x1++;
  if (y1+1 < y2) y1++;
  //cout << "SB: " << x1 << ", " << y1 << ", " << x2 << ", " << y2 << "\n";

  if (ghostscript_bugged ()) {
    int scr  = DefaultScreen (dpy);
    int max_w= 2 * DisplayWidth (dpy, scr);
    int max_h= 2 * DisplayHeight (dpy, scr);
    w= min (w, max_w);
    h= min (h, max_h);
  }

#ifndef OS_WIN32
  int win_id= (int) gs_win;
  int pix_id= (int) pm;
  if (ghostscript_bugged ()) set_env ("GHOSTVIEW", as_string (win_id));
  else set_env ("GHOSTVIEW", as_string (win_id) * " " * as_string (pix_id));
  Atom gh= XInternAtom (dpy, "GHOSTVIEW", false);
  Atom st= XA_STRING;
  double dpi_x= ((double) (w*72))/((double) (x2-x1));
  double dpi_y= ((double) (h*72))/((double) (y2-y1));
  //cout << "DPI: " << dpi_x << ", " << dpi_y << "\n";
  string data=
    (ghostscript_bugged ()? as_string (pix_id): string ("0")) * " 0 " *
    as_string (x1) * " " * as_string (y1) * " " *
    as_string (x2) * " " * as_string (y2) * " " *
    as_string (dpi_x) * " " * as_string (dpi_y) * " " *
    "0 0 0 0";
  unsigned char* _data= (unsigned char*) as_charp (data);
  int _n= N(data);
  XChangeProperty (dpy, gs_win, gh, st, 8, PropModeReplace, _data, _n);
  delete[] _data;
  XSync(dpy, false);
#endif

  string raw_ps, nice_ps;
  raw_ps= ps_load (image);
  nice_ps= encapsulate_postscript (raw_ps);
  url temp_name= url_temp ();
  save_string (temp_name, nice_ps, true);
  system ("tm_gs", temp_name);
  remove (temp_name);
}
