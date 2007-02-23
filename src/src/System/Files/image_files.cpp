
/******************************************************************************
* MODULE     : image_files.cpp
* DESCRIPTION: image file handling
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "file.hpp"
#include "image_files.hpp"
#include "web_files.hpp"
#include "sys_utils.hpp"
#include "analyze.hpp"
#include "hashmap.hpp"
#include "scheme.hpp"
#include "../Plugins/Imlib2/imlib2.hpp"

#ifdef OS_WIN32
#include <x11/xlib.h>
#endif

hashmap<tree,string> ps_bbox ("");

/******************************************************************************
* Loading xpm pixmaps
******************************************************************************/

tree
xpm_load (url u) {
  string s;
  load_string ("$TEXMACS_PIXMAP_PATH" * u, s);
  if (s == "") load_string ("$TEXMACS_PATH/misc/pixmaps/TeXmacs.xpm", s);

  int i, j;
  tree t (TUPLE);
  for (i=0; i<N(s); i++)
    if (s[i]=='\x22') {
      i++;
      j=i;
      while ((i<N(s)) && (s[i]!='\x22')) i++;
      t << s (j, i);
    }
  if (N(t)==0) return xpm_load ("$TEXMACS_PATH/misc/pixmaps/TeXmacs.xpm");
  return t;
}

void
xpm_size (url u, int& w, int& h) {
  static hashmap<string,string> xpm_size_table ("");
  string file_name= as_string (u);
  if (!xpm_size_table->contains (file_name)) {
    tree t= xpm_load (u);
    xpm_size_table (file_name)= t[0]->label;
  }

  int i= 0;
  bool ok;
  string s= xpm_size_table[file_name];
  skip_spaces (s, i);
  ok= read_int (s, i, w);
  skip_spaces (s, i);
  ok= read_int (s, i, h) && ok;
  if (!ok) fatal_error ("Invalid xpm (" * file_name * ")", "xpm_size");
}

array<string>
xpm_colors (tree t) {
  array<string> res(0);
  string s= t[0]->label;
  int ok, i=0, j, k, w, h, c, b;
  skip_spaces (s, i);
  ok= read_int (s, i, w);
  skip_spaces (s, i);
  ok= read_int (s, i, h) && ok;
  skip_spaces (s, i);
  ok= read_int (s, i, c) && ok;
  skip_spaces (s, i);
  ok= read_int (s, i, b) && ok;
  if ((!ok) || (N(t)<(c+1)) || (c<=0))
    fatal_error ("Invalid xpm tree", "x_drawable_rep::xpm_colors");

  for (k=0; k<c; k++) {
    string s   = as_string (t[k+1]);
    string def = "none";
    if (N(s)<b) i=N(s); else i=b;

    skip_spaces (s, i);
    if ((i<N(s)) && (s[i]=='s')) {
      i++;
      skip_spaces (s, i);
      while ((i<N(s)) && (s[i]!=' ') && (s[i]!='\t')) i++;
      skip_spaces (s, i);
    }
    if ((i<N(s)) && (s[i]=='c')) {
      i++;
      skip_spaces (s, i);
      j=i;
      while ((i<N(s)) && (s[i]!=' ') && (s[i]!='\t')) i++;
      def= locase_all (s (j, i));
    }
    res<<def;
  }
  return res;
}

array<SI>
xpm_hotspot (tree t) {
  array<SI> res(0);
  string s= t[0]->label;
  int ok, i=0, w, h, c, b, x, y;
  skip_spaces (s, i);
  ok= read_int (s, i, w);
  skip_spaces (s, i);
  ok= read_int (s, i, h) && ok;
  skip_spaces (s, i);
  ok= read_int (s, i, c) && ok;
  skip_spaces (s, i);
  ok= read_int (s, i, b) && ok;
  if ((!ok) || (N(t)<(c+1)) || (c<=0))
    fatal_error ("Invalid xpm tree", "x_drawable_rep::xpm_hotspot");

  skip_spaces (s, i);
  ok= read_int (s, i, x) && ok;
  skip_spaces (s, i);
  ok= read_int (s, i, y) && ok;
  if (ok) {
    res<<x;
    res<<y;
  }
  return res;
}

/******************************************************************************
* Loading postscript files or conversion to postscript
******************************************************************************/

string
ps_load (url image) {
  url name= resolve (image);
  if (is_none (name))
    name= "$TEXMACS_PATH/misc/pixmaps/unknown.ps";

#ifdef OS_WIN32
  if (is_ramdisc (name)) name= get_from_ramdisc (name);
#endif

  string s, suf= suffix (name);
  if (suf == "ps" || suf == "eps") load_string (name, s);
  else s= as_string (call ("image->postscript", object (name)));

#ifdef OS_WIN32
  if (s == "") {
    char *data;
    char *path= as_charp (as_string (name));
    data= XLoadImageAsPS (path);
    delete[] path;
    if (!data) s= "";
    else {
      s= string (data);
      free (data);
    }
  }
#endif

  if (s == "") load_string ("$TEXMACS_PATH/misc/pixmaps/unknown.ps", s);
  return s;
}

void
ps_bounding_box (url image, int& x1, int& y1, int& x2, int& y2) {
  tree lookup= image->t;
  if (!ps_bbox->contains (image->t)) {
    int i;
    string s= ps_load (image);
    string r= "0 0 32 32";
    for (i=0; i<N(s); i++)
      if (read (s, i, "%%BoundingBox: ")) {
	double tmp;
	int j = i;
	read_line (s, i, r);
	// Check whether we really have a bounding box line with numbers
	skip_spaces (s, j);
	if(read_double (s, j, tmp))
	  break;
      }
    ps_bbox (image->t)= r;
  }

  int i= 0;
  bool ok;
  string s= ps_bbox [image->t];
  double X1=0.0, Y1=0.0, X2=0.0, Y2=0.0;
  skip_spaces (s, i);
  ok= read_double (s, i, X1);
  skip_spaces (s, i);
  ok= read_double (s, i, Y1) && ok;
  skip_spaces (s, i);
  ok= read_double (s, i, X2) && ok;
  skip_spaces (s, i);
  ok= read_double (s, i, Y2) && ok;
  x1= (int) X1; y1= (int) Y1;
  x2= (int) X2; y2= (int) Y2;
  if (ok) return;
  x1= y1= 0; x2= 596; y2= 842;
}

/******************************************************************************
* Getting the size of an image, using internal plug-ins if possible
******************************************************************************/

void
image_size (url image, int& w, int& h) {
  if (imlib2_supports (image))
    imlib2_image_size (image, w, h);
  else {
    int x1, y1, x2, y2;
    ps_bounding_box (image, x1, y1, x2, y2);
    w= x2 - x1;
    h= y2 - y1;
  }
}
