
/******************************************************************************
* MODULE     : load_tex.cpp
* DESCRIPTION: simultaneously load pk and tfm file and
*              generate them if they can't be found.
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "load_tex.hpp"
#include "path.hpp"
#include "boot.hpp"
#include "Freetype/tt_file.hpp"
#include "Freetype/tt_face.hpp"
#include "timer.hpp"

#ifdef OS_WIN32
#include <x11/xlib.h>
#endif

static int
mag (int dpi, int size, int dsize) {
  if ((size>=100) && (dsize<100)) dsize *= 100;
  if ((dsize>=100) && (size<100))  size *= 100;
  return (size*((unsigned long int) dpi))/dsize;
}

/******************************************************************************
* Loading tfm files
******************************************************************************/

bool
try_tfm (string family, int size, int osize, tex_font_metric& tfm, bool make) {
  // cout << "Try tfm " << family << size << " (" << osize << ")\n";
  make= make && get_setting ("MAKETFM") != "false";
  string name_tfm = family * as_string (osize) * ".tfm";
  if (tex_font_metric::instances -> contains (name_tfm)) {
    tfm= tex_font_metric (name_tfm);
    return true;
  }
  string name= family * (size==0? string (""): as_string (size)) * ".tfm";
  if (DEBUG_STD) cout << "TeXmacs] Try tfm " << name << "\n";
  url u= resolve_tex (name);
  if (is_none (u)) {
    if (exists (url ("$TEXMACS_HOME_PATH/fonts/error", name)))
      return false;
    if (make) {
      system_wait ("Generating font file", name);
      make_tex_tfm (name);
      system_wait ("");
      u= resolve_tex (name);
      if (is_none (u)) {
	reset_tfm_path ();
	u= resolve_tex (name);
	save_string (url ("$TEXMACS_HOME_PATH/fonts/error", name), "");
      }
    }
    if (is_none (u)) return false;
  }
  // cout << "Tfm " << family << osize << " -> " << family << size << "\n";
  tfm= load_tfm (u, family, osize);
  if (size == 0) {
    size= tfm->size;
    if (DEBUG_STD) cout << "TeXmacs] Design size = " << size << "\n";
  }
  if (size != osize)
    tfm->header[1]= mag (tfm->header[1], osize, size);
  return true;
}

bool
load_tex_tfm (string family, int size, int dsize, tex_font_metric& tfm,
	      bool make)
{
  //cout << "Load TeX tfm " << family << size << " (dsize= " << dsize << ")\n";
  if (try_tfm (family, size, size, tfm, make))
    return true;
  if (size > 333)
    return load_tex_tfm (family, (size+50)/100, dsize, tfm, make);
  if (get_font_type () == 3) {
    if ((size > 14) && try_tfm (family, 17, size, tfm, make)) return true;
    if ((size > 12) && try_tfm (family, 12, size, tfm, make)) return true;
    if ((size > 10) && try_tfm (family, 10, size, tfm, make)) return true;
    if ((size <  5) && try_tfm (family,  5, size, tfm, make)) return true;
    if ((size <  6) && try_tfm (family,  6, size, tfm, make)) return true;
    if ((size <  7) && try_tfm (family,  7, size, tfm, make)) return true;
    if ((size <  8) && try_tfm (family,  8, size, tfm, make)) return true;
    if ((size <  9) && try_tfm (family,  9, size, tfm, make)) return true;
    if ((size <  9) && try_tfm (family,  7, size, tfm, make)) return true;
    if (try_tfm (family, 10, size, tfm, make)) return true;
    if ((size > 14) && try_tfm (family, 1700, size, tfm, make)) return true;
    if ((size > 12) && try_tfm (family, 1200, size, tfm, make)) return true;
    if ((size < 5) && try_tfm (family, 500, size, tfm, make)) return true;
    if ((size < 9) && try_tfm (family, 700, size, tfm, make)) return true;
    if (try_tfm (family, 1000, size, tfm, make)) return true;
  }
  if (get_font_type () == 2) {
    SI delta= (size<10? 1: -1);
    if (try_tfm (family, size + delta, size, tfm, make)) return true;
    if (try_tfm (family, size - delta, size, tfm, make)) return true;
    if (try_tfm (family, size + 2*delta, size, tfm, make)) return true;
    if (try_tfm (family, size - 2*delta, size, tfm, make)) return true;
    if (try_tfm (family, 100 * size, size, tfm, make)) return true;
    if (try_tfm (family, 100 * (size + delta), size, tfm, make)) return true;
    if (try_tfm (family, 100 * (size - delta), size, tfm, make)) return true;
    if (try_tfm (family, 100 * (size + 2*delta), size, tfm, make)) return true;
    if (try_tfm (family, 100 * (size - 2*delta), size, tfm, make)) return true;
  }
  if (dsize != size)
    if (try_tfm (family, dsize, size, tfm, make))
      return true;
  if ((dsize != 10) && (size != 10))
    if (try_tfm (family, 10, size, tfm, make))
      return true;
  return false;
}

bool
load_tex_tfm (string family, int size, int dsize, tex_font_metric& tfm) {
  if (get_font_type () >= 2 && get_setting ("MAKETFM") != "false")
    if (load_tex_tfm (family, size ,dsize, tfm, false))
      return true;
  return load_tex_tfm (family, size ,dsize, tfm, true);
}

/******************************************************************************
* PK font glyphs with lazy parsing
******************************************************************************/

static glyph error_glyph;

struct pk_font_glyphs_rep: public font_glyphs_rep {
  pk_loader* pkl;
  int bc, ec;
  glyph* fng; // definitions of the characters

  pk_font_glyphs_rep (string name, pk_loader*);
  glyph& get (int char_code);
};

pk_font_glyphs_rep::pk_font_glyphs_rep(string name, pk_loader* pkl2)
  :font_glyphs_rep (name), pkl (pkl2)
{
  if (pkl) {
    fng = pkl->load_pk ();
    bc = pkl->tfm->bc;
    ec = pkl->tfm->ec;
  }
  else {
    fng = 0;
    bc = 0;
    ec = -1;
  }
}

glyph&
pk_font_glyphs_rep::get(int c)
{
  if ((c<bc) || (c>ec)) return error_glyph;
  if (pkl && !pkl->unpacked[c-bc]) {
    pkl->input_pos = pkl->char_pos[c-bc];
    pkl->flagbyte  = pkl->char_flag[c-bc];
    pkl->unpack(fng[c-bc]);

    pkl->unpacked[c-bc] = true;
  }
  return fng [c-bc];
}

/******************************************************************************
* Loading pk files
******************************************************************************/

bool
try_pk (string family, int size, int dpi, int dsize,
	tex_font_metric& tfm, font_glyphs& pk)
{
  // cout << "Try pk " << family << size << " at " << dpi << " dpi\n";
#ifdef USE_FREETYPE
  if (get_font_type () > 0) {
    // Substitute by True Type font ?
    int tt_size= size<333? size: (size+50)/100;
    int tt_dpi = size<333? dpi : (size * dpi) / (100 * tt_size);
    string tt_name= tt_find_name (family, tt_size);
    if (tt_name != "") {
      if (font_glyphs::instances -> contains (tt_name))
	pk= font_glyphs (tt_name);
      else pk= tt_font_glyphs (tt_name, tt_size, tt_dpi);
      return true;
    }
  }
#endif // USE_FREETYPE

  // Open regular pk font
  string name_pk= family * as_string (size) * "." * as_string (dpi) * "pk";
  if (font_glyphs::instances -> contains (name_pk)) {
    pk = font_glyphs (name_pk);
    return true;
  }
  if (dsize == 0) {
    int old_size= size;
    size= tfm->size;
    dpi = mag (dpi, old_size, size);
  }
  string size_name (dsize==0? string (""): as_string (size));
  string name (family * size_name * "." * as_string (dpi) * "pk");
  if (DEBUG_STD) cout << "TeXmacs] Open pk " << name << "\n";
  url u= resolve_tex (name);
  if (is_none (u)) {
    if (exists (url ("$TEXMACS_HOME_PATH/fonts/error", name)))
      return false;
    if (get_setting ("MAKEPK") != "false") {
      system_wait ("Generating font file", name);
      make_tex_pk (family * size_name, dpi, as_int (get_setting ("DPI")));
      system_wait ("");
      u= resolve_tex (name);
      if (is_none (u)) {
	reset_pk_path ();
	u= resolve_tex (name);
      }
    }
    if (is_none (u)) {
      save_string (url ("$TEXMACS_HOME_PATH/fonts/error", name), "");
      return false;
    }
  }
  pk = font_glyphs (new pk_font_glyphs_rep (name_pk,
					    new pk_loader(u, tfm, dpi)));
  return true;
}

bool
load_tex_pk (string family, int size, int dpi, int dsize,
	     tex_font_metric& tfm, font_glyphs& pk) {
  if (try_pk (family, size, dpi, dsize, tfm, pk)) return true;
  if ((dsize != size) && (dsize != 0))
    if (try_pk (family, dsize, mag (dpi, size, dsize), dsize, tfm, pk))
      return true;
  if ((dsize != 10) && (size != 10))
    if (try_pk (family, 10, mag (dpi, size, 10), dsize, tfm, pk))
      return true;
  if (size > 333) {
    int sz= (size+50)/100;
    return load_tex_pk (family, sz, mag (dpi, size, sz), dsize, tfm, pk);
  }
  return false;
}

/******************************************************************************
* Loading tfm and pk files
******************************************************************************/

void
load_tex (string family, int size, int dpi, int dsize,
	  tex_font_metric& tfm, font_glyphs& pk)
{
  bench_start ("load tex font");
  if (DEBUG_VERBOSE)
    cout << "TeXmacs] loading " << family << size
	 << " at " << dpi << " dpi\n";
  if (load_tex_tfm (family, size, dsize, tfm) &&
      load_tex_pk (family, size, dpi, dsize, tfm, pk))
    {
      bench_cumul ("load tex font");
      return;
    }
  if (DEBUG_VERBOSE) {
    cout << "TeXmacs] font " << family << size
         << " at " << dpi << " dpi not found\n";
    cout << "TeXmacs] loading cmr" << size
	 << " at " << dpi << " dpi instead\n";
  }
  if (load_tex_tfm ("cmr", size, 10, tfm) &&
      load_tex_pk ("cmr", size, dpi, 10, tfm, pk))
    {
      bench_cumul ("load tex font");
      return;
    }
#ifdef OS_WIN32
  else {
    string name= family * as_string (size) * "@" * as_string (dpi);
    cerr << "\n\nCould not open font " << name << "\nLoading default" << LF;
    cout << "Could not load font...\nLoading default" << LF;
    XNoTexWarn();
    if (load_tex_tfm ("cmr", 10, 10, tfm) &&
	load_tex_pk ("cmr", 10, 600, 10, tfm, pk))
      {
	bench_cumul ("load tex font");
	return;
      }
  }
#endif
  string name= family * as_string (size) * "@" * as_string (dpi);
  cerr << "\n\nI could not open " << name << "\n";
  fatal_error ("Tex seems not to be installed properly",
	       "load_tex", "load_tex.cpp");
  bench_cumul ("load tex font");
}
