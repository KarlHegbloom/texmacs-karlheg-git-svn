
/******************************************************************************
* MODULE     : rubber_unicode_font.cpp
* DESCRIPTION: Assemble rubber characters from pieces in Unicode fonts
* COPYRIGHT  : (C) 2015  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "font.hpp"
#include "converter.hpp"
#include "translator.hpp"

#ifdef USE_FREETYPE

/******************************************************************************
* True Type fonts
******************************************************************************/

struct rubber_assemble_font_rep: font_rep {
  font base;
  array<font> larger;
  translator virt;

  rubber_assemble_font_rep (string name, font base);
  int search_font (string s, string& r);

  bool supports (string c);
  void get_extents (string s, metric& ex);
  void draw_fixed (renderer ren, string s, SI x, SI y);
  void draw_fixed (renderer ren, string s, SI x, SI y, SI xk);
  font magnify (double zoom);
  glyph get_glyph (string s);
};

/******************************************************************************
* Initialization of main font parameters
******************************************************************************/

#define MAGNIFIED_NUMBER 4

rubber_assemble_font_rep::rubber_assemble_font_rep (string name, font base2):
  font_rep (name, base2), base (base2)
{
  this->copy_math_pars (base);
  larger << base;
  double m= sqrt (sqrt (2.0)), p= m;
  for (int i=1; i<=MAGNIFIED_NUMBER; i++) {
    larger << base->magnify (p);
    p *= m;
  }
  int dpi= (72 * base->wpt + (PIXEL/2)) / PIXEL;
  larger << virtual_font (base, "unilong", base->size, dpi);
  virt= load_translator ("unilong");
}

int
rubber_assemble_font_rep::search_font (string s, string& r) {
  if (starts (s, "<mid-")) s= "<left-" * s (5, N(s));
  if (starts (s, "<right-")) s= "<left-" * s (7, N(s));
  if (starts (s, "<large-")) s= "<left-" * s (7, N(s));
  if (starts (s, "<left-")) {
    int pos= search_backwards ("-", N(s), s);
    if (pos > 6) {
      r= s (6, pos);
      int num= as_int (s (pos+1, N(s)-1));
      int nr= num - 5;
      int code;
      if (num <= MAGNIFIED_NUMBER) return num;
      else if (r == "(")
        code= virt->dict ["<rubber-lparenthesis-#>"];
      else if (r == ")")
        code= virt->dict ["<rubber-rparenthesis-#>"];
      else if (r == "[")
        code= virt->dict ["<rubber-lbracket-#>"];
      else if (r == "]")
        code= virt->dict ["<rubber-rbracket-#>"];
      else if (r == "{")
        code= virt->dict ["<rubber-lcurly-#>"];
      else if (r == "}")
        code= virt->dict ["<rubber-rcurly-#>"];
      else
        code= virt->dict ["<rubber-lparenthesis-#>"];
      r= string ((char) code) * as_string (nr) * ">";
      return MAGNIFIED_NUMBER + 1;
    }
  }
  r= s;
  return 0;
}

/******************************************************************************
* Getting extents and drawing strings
******************************************************************************/

bool
rubber_assemble_font_rep::supports (string s) {
  if (starts (s, "<mid-")) s= "<left-" * s (5, N(s));
  if (starts (s, "<right-")) s= "<left-" * s (7, N(s));
  if (starts (s, "<large-")) s= "<left-" * s (7, N(s));
  if (starts (s, "<left-")) {
    int pos= search_backwards ("-", N(s), s);
    if (pos > 6) {
      string r= s (6, pos);
      return r == "(" || r == ")" ||
             r == "[" || r == "]" ||
             r == "{" || r == "}";
    }
  }
  return false;
}

void
rubber_assemble_font_rep::get_extents (string s, metric& ex) {
  string name;
  int num= search_font (s, name);
  larger[num]->get_extents (name, ex);
}

void
rubber_assemble_font_rep::draw_fixed (renderer ren, string s, SI x, SI y) {
  string name;
  int num= search_font (s, name);
  larger[num]->draw (ren, name, x, y);
}

void
rubber_assemble_font_rep::draw_fixed (renderer ren, string s, SI x, SI y, SI xk) {
  string name;
  int num= search_font (s, name);
  larger[num]->draw (ren, name, x, y, xk);
}

font
rubber_assemble_font_rep::magnify (double zoom) {
  return rubber_assemble_font (base->magnify (zoom));
}

glyph
rubber_assemble_font_rep::get_glyph (string s) {
  string name;
  int num= search_font (s, name);
  return larger[num]->get_glyph (name);
}

/******************************************************************************
* Interface
******************************************************************************/

font
rubber_assemble_font (font base) {
  string name= "rubberassemble[" * base->res_name * "]";
  return make (font, name, tm_new<rubber_assemble_font_rep> (name, base));
}

#else

font
rubber_assemble_font (font base) {
  string name= "rubberunicode[" * base->res_name * "]";
  failed_error << "Font name= " << name << "\n";
  FAILED ("true type support was disabled");
  return font ();
}

#endif
