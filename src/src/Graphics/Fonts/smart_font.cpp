
/******************************************************************************
* MODULE     : smart_font.cpp
* DESCRIPTION: smart merging of several fonts for different unicode ranges
* COPYRIGHT  : (C) 2013  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "font.hpp"
#include "convert.hpp"
#include "converter.hpp"
#include "Freetype/tt_tools.hpp"
#include "translator.hpp"

/******************************************************************************
* Efficient computation of the appropriate subfont
******************************************************************************/

RESOURCE(smart_map);

#define SUBFONT_MAIN  0
#define SUBFONT_ERROR 1

#define REWRITE_NONE       0
#define REWRITE_MATH       1
#define REWRITE_CYRILLIC   2
#define REWRITE_LETTERS    3
#define REWRITE_SPECIAL    4

struct smart_map_rep: rep<smart_map> {
  int chv[256];
  hashmap<string,int> cht;
  hashmap<tree,int> fn_nr;
  array<tree> fn_spec;
  array<int> fn_rewr;

public:
  smart_map_rep (string name, tree fn):
    rep<smart_map> (name), cht (-1), fn_nr (-1), fn_spec (2), fn_rewr (2)
  {
    for (int i=0; i<256; i++) chv[i]= -1;
    fn_nr (tuple ("main" ))= SUBFONT_MAIN;
    fn_nr (tuple ("error"))= SUBFONT_ERROR;
    fn_spec[SUBFONT_MAIN ]= tuple ("main");
    fn_spec[SUBFONT_ERROR]= tuple ("error");
    fn_rewr[SUBFONT_MAIN ]= REWRITE_NONE;
    fn_rewr[SUBFONT_ERROR]= REWRITE_NONE;
  }

  int
  add_font (tree fn, int rewr) {
    if (!fn_nr->contains (fn)) {
      int sz= N (fn_spec);
      fn_nr (fn)= sz;
      fn_spec << fn;
      fn_rewr << rewr;
      //cout << "Create " << sz << " -> " << fn << "\n";
    }
    return fn_nr[fn];
  }

  int
  add_char (tree fn, string c) {
    //cout << "Add " << c << " to " << fn << "\n";
    add_font (fn, REWRITE_NONE);
    int nr= fn_nr [fn];
    if (starts (c, "<")) cht (c)= nr;
    else chv [(int) (unsigned char) c[0]]= nr;
    return nr;
  }
};


RESOURCE_CODE(smart_map);

smart_map
get_smart_map (tree fn) {
  string name= recompose (tuple_as_array (fn), "-");
  if (smart_map::instances -> contains (name))
    return smart_map (name);
  return make (smart_map, name, tm_new<smart_map_rep> (name, fn));
}

/******************************************************************************
* Virtual font handling
******************************************************************************/

static bool virt_initialized= false;
static array<string> std_virt;
static array<translator> std_trl;

static void
initialize_virtual () {
  if (virt_initialized) return;
  std_virt << string ("long") << string ("negate") << string ("misc");
  for (int i=0; i<N(std_virt); i++)
    std_trl << load_translator (std_virt[i]);
  virt_initialized= true;
}

static string
find_in_virtual (string c) {
  initialize_virtual ();
  for (int i=0; i<N(std_virt); i++)
    if (std_trl[i]->dict->contains (c))
      return std_virt[i];
  return "";
}

/******************************************************************************
* Special characters in mathematical fonts
******************************************************************************/

static string rewrite_math (string s);

static bool
is_math_family (string f) {
  return
    f == "roman" ||
    f == "concrete" ||
    f == "Euler" ||
    f == "ENR";
}

inline bool
is_letter (char c) {
  return (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z');
}

static bool
is_greek (string c) {
  static hashmap<string,bool> t (false);
  if (N(t) == 0) {
    array<int> a;
    //for (int i= 0x391; i<0x3a9; i++) if (i != 0x3a2) a << i;
    for (int i= 0x3b1; i<0x3c9; i++) a << i;
    for (int i= 0; i<N(a); i++) {
      string s= upcase_all ("<#" * as_hexadecimal (a[i]) * ">");
      t (s)= true;
      t (locase_all (s))= true;
      t (rewrite_math (s))= true;
    }
  }
  return t[c];
}

static hashmap<string,string> special_table ("");

static bool
unicode_provides (string s) {
  return cork_to_utf8 (s) != s;
}

static bool
is_special (string s) {
  if (N (special_table) == 0) {
    special_table ("*")= "";
    special_table ("<noplus>")= "";
    special_table ("<nocomma>")= "";
    special_table ("<nospace>")= "";
    special_table ("<nobracket>")= "";
    special_table ("<nosymbol>")= "";
    special_table ("-")= "<minus>";
    special_table ("|")= "<mid>";
    special_table ("'")= "<#2B9>";
    special_table ("`")= "<backprime>";
  }
  if (starts (s, "<big-."))
    special_table (s)= "";
  if (starts (s, "<big-") && (ends (s, "-1>") || ends (s, "-2>"))) {
    string ss= s (0, N(s)-3) * ">";
    //cout << "Search " << ss << "\n";
    if (unicode_provides (ss))
      special_table (s)= ss;
    ss= "<big" * s (5, N(s)-3) * ">";
    //cout << "Search " << ss << "\n";
    if (unicode_provides (ss))
      special_table (s)= ss;
    ss= "<" * s (5, N(s)-3) * ">";
    if (ends (ss, "lim>")) ss= ss (0, N(ss)-4) * ">";
    //cout << "Search " << ss << "\n";
    if (unicode_provides (ss))
      special_table (s)= ss;
  }
  return special_table->contains (s);
}

/******************************************************************************
* Mathematical letters in Unicode
******************************************************************************/

static hashmap<string,string> substitution_char ("");
static hashmap<string,string> substitution_font ("");

static void
unicode_subst (int src, int dest, int nr, string fn) {
  for (int i=0; i<nr; i++) {
    string csrc = upcase_all ("<#" * as_hexadecimal (src  + i) * ">");
    string cdest= upcase_all ("<#" * as_hexadecimal (dest + i) * ">");
    if (dest + i < 128) cdest= string ((char) (dest + i));
    substitution_char (csrc)= cdest;
    substitution_font (csrc)= fn;
    csrc= locase_all (csrc);
    substitution_char (csrc)= cdest;
    substitution_font (csrc)= fn;
    csrc= rewrite_math (csrc);
    substitution_char (csrc)= cdest;
    substitution_font (csrc)= fn;
  }
}

static void
unicode_letters (int start, string fn) {
  unicode_subst (start, 0x41, 26, fn);
  unicode_subst (start + 26, 0x61, 26, fn);
}

static void
unicode_greek (int start, string fn) {
  unicode_subst (start, 0x391, 25, fn); // FIXME: attention to 0x3a2
  unicode_subst (start + 25, 0x2207, 1, fn);
  unicode_subst (start + 26, 0x3b1, 25, fn);
  unicode_subst (start + 51, 0x2202, 1, fn);
  unicode_subst (start + 52, 0x3f5, 1, fn);
  unicode_subst (start + 53, 0x3d1, 1, fn);
  unicode_subst (start + 54, 0x3f0, 1, fn);
  unicode_subst (start + 55, 0x3d5, 1, fn);
}

static void
unicode_digits (int start, string fn) {
  unicode_subst (start, 0x30, 10, fn);
}

static void
init_unicode_substitution () {
  if (N (substitution_char) != 0) return;
  unicode_letters (0x1d400, "bold-math");
  unicode_letters (0x1d434, "italic-math");
  unicode_letters (0x1d468, "bold-italic-math");
  unicode_letters (0x1d49c, "cal");
  unicode_letters (0x1d4d0, "bold-cal");
  unicode_letters (0x1d504, "frak");
  unicode_letters (0x1d56c, "bold-frak");
  unicode_letters (0x1d538, "bbb");
  unicode_letters (0x1d5a0, "ss");
  unicode_letters (0x1d5d4, "bold-ss");
  unicode_letters (0x1d608, "italic-ss");
  unicode_letters (0x1d63c, "bold-italic-ss");
  unicode_letters (0x1d670, "tt");
  unicode_greek (0x1d6a8, "bold-math");
  unicode_greek (0x1d6e2, "italic-math");
  unicode_greek (0x1d71c, "bold-italic-math");
  unicode_greek (0x1d756, "bold-ss");
  unicode_greek (0x1d790, "bold-italic-ss");
  unicode_digits (0x1d7ce, "bold-math");
  unicode_digits (0x1d7d8, "bbb");
  unicode_digits (0x1d7e2, "ss");
  unicode_digits (0x1d7ec, "bold-ss");
  unicode_digits (0x1d7f6, "tt");
  unicode_subst (0x212c, 0x42, 1, "cal");
  unicode_subst (0x2130, 0x45, 1, "cal");
  unicode_subst (0x2131, 0x46, 1, "cal");
  unicode_subst (0x210b, 0x48, 1, "cal");
  unicode_subst (0x2110, 0x49, 1, "cal");
  unicode_subst (0x2112, 0x4c, 1, "cal");
  unicode_subst (0x2133, 0x4d, 1, "cal");
  unicode_subst (0x211b, 0x52, 1, "cal");
  unicode_subst (0x212f, 0x65, 1, "cal");
  unicode_subst (0x210a, 0x67, 1, "cal");
  unicode_subst (0x2134, 0x6f, 1, "cal");
  unicode_subst (0x212d, 0x43, 1, "frak");
  unicode_subst (0x210c, 0x49, 1, "frak");
  unicode_subst (0x2111, 0x4a, 1, "frak");
  unicode_subst (0x211c, 0x52, 1, "frak");
  unicode_subst (0x2128, 0x5a, 1, "frak");
  unicode_subst (0x2102, 0x43, 1, "bbb");
  unicode_subst (0x210d, 0x48, 1, "bbb");
  unicode_subst (0x2115, 0x4e, 1, "bbb");
  unicode_subst (0x2119, 0x50, 1, "bbb");
  unicode_subst (0x211a, 0x51, 1, "bbb");
  unicode_subst (0x211d, 0x52, 1, "bbb");
  unicode_subst (0x2124, 0x5a, 1, "bbb");
}

int
get_utf8_code (string c) {
  string uc= cork_to_utf8 (c);
  int pos= 0;
  int code= decode_from_utf8 (uc, pos);
  if (pos == N(uc)) return code;
  else return -1;
}

string
substitute_math_letter (string c, int math_kind) {
  if (math_kind == 0) return "";
  int code= get_utf8_code (c);
  if ((code >= 0x1d400 && code <= 0x1d7ff) ||
      (code >= 0x2100 && code <= 0x213f)) {
    init_unicode_substitution ();
    string nc= "<#" * as_hexadecimal (code) * ">";
    string sc= substitution_char [nc];
    string sf= substitution_font [nc];
    //cout << c << " (" << nc << ") -> " << sc << ", " << sf << "\n";
    if (sc != "" && sc != c) {
      bool flag= ends (sf, "cal") || ends (sf, "frak") || ends (sf, "bbb");
      if (!flag || math_kind == 2) return sf;
    }
  }
  return "";
}

/******************************************************************************
* Font sequences
******************************************************************************/

array<string>
trimmed_tokenize (string s, string sep) {
  return trim_spaces (tokenize (s, sep));
}

string
main_family (string f) {
  array<string> a= trimmed_tokenize (f, ",");
  for (int i=0; i<N(a); i++)
    if (N (trimmed_tokenize (a[i], "=")) <= 1)
      return a[i];
  if (N(a) == 0) return f;
  a= trimmed_tokenize (f, "=");
  return a[1];
}

string
get_unicode_range (string c) {
  string uc= cork_to_utf8 (c);
  int pos= 0;
  int code= decode_from_utf8 (uc, pos);
  string range= "";
  if (code <= 0x7f) range= "ascii";
  else if (code >= 0x80 && code <= 0x37f) range= "latin";
  else if (code >= 0x380 && code <= 0x3ff) range= "greek";
  else if (code >= 0x400 && code <= 0x4ff) range= "cyrillic";
  else if (code >= 0x3000 && code <= 0x303f) range= "cjk";
  else if (code >= 0x4e00 && code <= 0x9fcc) range= "cjk";
  else if (code >= 0xff00 && code <= 0xffef) range= "cjk";
  else if (code >= 0xac00 && code <= 0xd7af) range= "hangul";
  else if (code >= 0x2000 && code <= 0x23ff) range= "mathsymbols";
  else if (code >= 0x2900 && code <= 0x2e7f) range= "mathextra";
  else if (code >= 0x1d400 && code <= 0x1d7ff) range= "mathletters";
  if (pos == N(uc)) return range;
  return "";
}

/******************************************************************************
* The smart font class
******************************************************************************/

typedef int int_vector[256];
typedef hashmap<string,int> int_table;

struct smart_font_rep: font_rep {
  string mfam;
  string family;
  string variant;
  string series;
  string shape;
  string rshape;
  int    sz;
  int    dpi;
  int    math_kind;
  int    italic_nr;

  array<font> fn;
  smart_map   sm;

  smart_font_rep (string name, font base_fn, font err_fn,
                  string family, string variant,
                  string series, string shape, int sz, int dpi);
  font   get_math_font (string fam, string var, string ser, string sh);
  font   get_cyrillic_font (string fam, string var, string ser, string sh);

  void   advance (string s, int& pos, string& r, int& nr);
  int    resolve (string c, string fam, int attempt);
  int    resolve (string c);
  void   initialize_font (int nr);
  int    adjusted_dpi (string fam, string var, string ser, string sh, int att);

  bool   supports (string c);
  void   get_extents (string s, metric& ex);
  void   get_xpositions (string s, SI* xpos);
  void   get_xpositions (string s, SI* xpos, SI xk);
  void   draw_fixed (renderer ren, string s, SI x, SI y);
  void   draw_fixed (renderer ren, string s, SI x, SI y, SI xk);
  font   magnify (double zoom);
  glyph  get_glyph (string s);
  double get_left_slope  (string s);
  double get_right_slope (string s);
  SI     get_left_correction  (string s);
  SI     get_right_correction (string s);
};

smart_font_rep::smart_font_rep (
  string name, font base_fn, font err_fn, string family2, string variant2,
  string series2, string shape2, int sz2, int dpi2):
    font_rep (name, base_fn), mfam (main_family (family2)),
    family (family2), variant (variant2),
    series (series2), shape (shape2), rshape (shape2),
    sz (sz2), dpi (dpi2),
    math_kind (0), italic_nr (-1),
    fn (2), sm (get_smart_map (tuple (family2, variant2, series2, shape2)))
{
  fn[SUBFONT_MAIN ]= base_fn;
  fn[SUBFONT_ERROR]= err_fn;
  if (shape == "mathitalic" || shape == "mathupright" || shape == "mathshape") {
    if (is_math_family (mfam)) {
      rshape= "right";
      if (shape == "mathupright")
        this->copy_math_pars (base_fn);
      else {
        tree key= tuple ("math", mfam, variant, series, rshape);
        int nr= sm->add_font (key, REWRITE_MATH);
        initialize_font (nr);
        this->copy_math_pars (fn[nr]);
        fn[SUBFONT_MAIN]= fn[nr];
      }
    } 
    else {
      math_kind= 1;
      if (shape == "mathupright") math_kind= 2;
      if (shape == "mathshape") math_kind= 3;
      rshape= "right";
      if (math_kind == 2)
        this->copy_math_pars (base_fn);
      else {
        italic_nr= sm->add_font (tuple ("fast-italic"), REWRITE_NONE);
        initialize_font (italic_nr);
        this->copy_math_pars (fn[italic_nr]);
      }
      (void) sm->add_font (tuple ("special"), REWRITE_SPECIAL);
      (void) sm->add_font (tuple ("other"), REWRITE_NONE);
      (void) sm->add_font (tuple ("regular"), REWRITE_LETTERS);
      (void) sm->add_font (tuple ("bold-math"), REWRITE_LETTERS);
      (void) sm->add_font (tuple ("italic-math"), REWRITE_LETTERS);
      (void) sm->add_font (tuple ("bold-italic-math"), REWRITE_LETTERS);
      (void) sm->add_font (tuple ("cal"), REWRITE_LETTERS);
      (void) sm->add_font (tuple ("bold-cal"), REWRITE_LETTERS);
      (void) sm->add_font (tuple ("frak"), REWRITE_LETTERS);
      (void) sm->add_font (tuple ("bold-frak"), REWRITE_LETTERS);
      (void) sm->add_font (tuple ("bbb"), REWRITE_LETTERS);
      (void) sm->add_font (tuple ("tt"), REWRITE_LETTERS);
      (void) sm->add_font (tuple ("ss"), REWRITE_LETTERS);
      (void) sm->add_font (tuple ("bold-ss"), REWRITE_LETTERS);
      (void) sm->add_font (tuple ("italic-ss"), REWRITE_LETTERS);
      (void) sm->add_font (tuple ("bold-italic-ss"), REWRITE_LETTERS);
    }
  }
}

/******************************************************************************
* Fonts for backward compatibility
******************************************************************************/

font
smart_font_rep::get_math_font (string fam, string var, string ser, string sh) {
  find_closest (fam, var, ser, sh);
  string mvar= "mr";
  if (var == "ss") mvar= "ms";
  if (var == "tt") mvar= "mt";
  return find_font (fam, mvar, ser, "", sz, dpi);
}

font
smart_font_rep::get_cyrillic_font (string fam, string var, string ser, string sh) {
  find_closest (fam, var, ser, sh);
  return find_font ("cyrillic", var, ser, sh, sz, dpi);
}

static string
rewrite_math (string s) {
  string r;
  int i= 0, n= N(s);
  while (i < n) {
    int start= i;
    tm_char_forwards (s, i);
    if (s[start] == '<' && start+1 < n && s[start+1] == '#' && s[i-1] == '>')
      r << utf8_to_cork (cork_to_utf8 (s (start, i)));
    else r << s (start, i);
  }
  return r;
}

static string
rewrite_letters (string s) {
  init_unicode_substitution ();
  string r;
  int i= 0, n= N(s);
  while (i < n) {
    int start= i;
    tm_char_forwards (s, i);
    string ss= s (start, i);
    if (substitution_char->contains (ss)) r << substitution_char[ss];
    else r << ss;
  }
  return r;
}

static string
rewrite (string s, int kind) {
  switch (kind) {
  case REWRITE_NONE:
    return s;
  case REWRITE_MATH:
    return rewrite_math (s);
  case REWRITE_CYRILLIC:
    return code_point_to_cyrillic_subset_in_t2a (s);
  case REWRITE_LETTERS:
    return rewrite_letters (s);
  case REWRITE_SPECIAL:
    return special_table [s];
  default:
    return s;
  }
}

/******************************************************************************
* Smart font resolution
******************************************************************************/

void
smart_font_rep::advance (string s, int& pos, string& r, int& nr) {
  int* chv= sm->chv;
  hashmap<string,int>& cht (sm->cht);
  int start= pos;
  nr= -1;
  while (pos < N(s)) {
    if (s[pos] != '<') {
      int c= (int) (unsigned char) s[pos];
      int next= chv[c];
      if (math_kind != 0 && math_kind != 2 && is_letter (c) &&
          (pos == 0 || !is_letter (s[pos-1])) &&
          (pos+1 == N(s) || !is_letter (s[pos+1])))
        next= italic_nr;
      else if (chv[c] == -1) next= resolve (s (pos, pos+1));
      if (next == nr) pos++;
      else if (nr == -1) { pos++; nr= next; }
      else break;
    }
    else {
      int end= pos;
      tm_char_forwards (s, end);
      int next= cht[s (pos, end)];
      if (next == -1) next= resolve (s (pos, end));
      if (next == nr) pos= end;
      else if (nr == -1) { pos= end; nr= next; }
      else break;
    }
  }
  r= s (start, pos);
  if (nr < 0) return;
  if (N(fn) <= nr || is_nil (fn[nr])) initialize_font (nr);
  if (sm->fn_rewr[nr] != REWRITE_NONE)
    r= rewrite (r, sm->fn_rewr[nr]);
  //cout << "Got " << r << " in " << fn[nr]->res_name << "\n";
}

int
smart_font_rep::resolve (string c, string fam, int attempt) { 
  //cout << "Resolve " << c << " in " << fam << ", attempt " << attempt << "\n";
  array<string> a= trimmed_tokenize (fam, "=");
  if (N(a) >= 2) {
    array<string> given= logical_font (family, variant, series, rshape);
    fam= a[1];
    array<string> b= tokenize (a[0], " ");
    for (int i=0; i<N(b); i++) {
      if (b[i] == "") continue;
      bool ok= false;
      array<string> v= tokenize (b[i], "|");
      for (int j=0; j<N(v); j++) {
        string wanted= locase_all (v[j]);
        if (wanted == "") ok= true;
        else if (contains (wanted, given)) ok= true;
        else if (wanted == get_unicode_range (c)) ok= true;
        else if (wanted == substitute_math_letter (c, 2)) ok= true;
        else if (wanted == c) ok= true;
        else {
          array<string> w= tokenize (v[j], ":");
          if (N(w) == 1) w << w[0];
          if (N(w) == 2) {
            int code = get_utf8_code (c);
            int start= get_utf8_code (w[0]);
            int end  = get_utf8_code (w[1]);
            if (code != -1 && code >= start && code <= end) ok= true;
          }
        }
      }
      if (!ok) return -1;
    }
  }

  if (attempt == 1) {
    bool ok= true;
    if (fam == "cal" || fam == "cal*" ||
        fam == "Bbb" || fam == "Bbb****")
      ok= ok && is_alpha (c) && upcase_all (c) == c;
    if (fam == "cal**" || fam == "Bbb*")
      ok= ok && is_alpha (c);
    if (!ok) return -1;

    if (fam == mfam) {
      if (fn[SUBFONT_MAIN]->supports (c))
        return sm->add_char (tuple ("main"), c);
    }
    else {
      font cfn= closest_font (fam, variant, series, rshape, sz, dpi, 1);
      if (cfn->supports (c)) {
        tree key= tuple (fam, variant, series, rshape, "1");
        return sm->add_char (key, c);
      }
    }

    if (is_math_family (fam)) {
      tree key= tuple ("math", fam, variant, series, rshape);
      int nr= sm->add_font (key, REWRITE_MATH);
      initialize_font (nr);
      if (fn[nr]->supports (rewrite (c, REWRITE_MATH)))
        return sm->add_char (key, c);
    }
    if (fam == "roman" && N(c) > 1) {
      tree key= tuple ("cyrillic", fam, variant, series, rshape);
      int nr= sm->add_font (key, REWRITE_CYRILLIC);
      initialize_font (nr);
      if (fn[nr]->supports (rewrite (c, REWRITE_CYRILLIC)))
        return sm->add_char (key, c);
    }
  }

  if (attempt > 1) {
    string range= get_unicode_range (c);
    if (range != "") {
      int a= attempt - 1;
      string v= variant;
      if (v == "rm") v= range;
      else v= v * "-" * range;
      font cfn= closest_font (fam, v, series, rshape, sz, dpi, a);
      //cout << "Trying " << c << " in " << cfn->res_name << "\n";
      if (cfn->supports (c)) {
        tree key= tuple (fam, v, series, rshape, as_string (a));
        return sm->add_char (key, c);
      }
    }
  }

  return -1;
}

int
smart_font_rep::resolve (string c) {
  if (math_kind != 0) {
    if (is_greek (c))
      return sm->add_char (tuple ("italic-math"), c);
    if (is_special (c))
      return sm->add_char (tuple ("special"), c);
  }

  array<string> a= trimmed_tokenize (family, ",");
  for (int attempt= 1; attempt <= 20; attempt++) {
    if (attempt > 1 && substitute_math_letter (c, math_kind) != "") break;
    for (int i= 0; i < N(a); i++) {
      int nr= resolve (c, a[i], attempt);
      if (nr >= 0) return nr;
    }
  }

  string sf= substitute_math_letter (c, math_kind);
  if (sf != "") return sm->add_char (tuple (sf), c);

  string virt= find_in_virtual (c);
  if (math_kind != 0 && !unicode_provides (c) && virt == "")
    return sm->add_char (tuple ("other"), c);

  if (virt != "") {
    //cout << "Found " << c << " in " << virt << "\n";
    return sm->add_char (tuple ("virtual", virt), c);
  }

  return sm->add_char (tuple ("error"), c);
}

void
smart_font_rep::initialize_font (int nr) {
  if (N(fn) <= nr) fn->resize (nr+1);
  if (!is_nil (fn[nr])) return;
  array<string> a= tuple_as_array (sm->fn_spec[nr]);
  if (a[0] == "math")
    fn[nr]= get_math_font (a[1], a[2], a[3], a[4]);
  else if (a[0] == "cyrillic")
    fn[nr]= get_cyrillic_font (a[1], a[2], a[3], a[4]);
  else if (a[0] == "special")
    fn[nr]= smart_font (family, variant, series, "right", sz, dpi);
  else if (a[0] == "other") {
    int ndpi= adjusted_dpi ("roman", variant, series, "mathitalic", 1);
    fn[nr]= smart_font ("roman", variant, series, "mathitalic", sz, ndpi);
  }
  else if (a[0] == "bold-math")
    fn[nr]= smart_font (family, variant, "bold", "right", sz, dpi);
  else if (a[0] == "fast-italic")
    fn[nr]= smart_font (family, variant, series, "italic", sz, dpi);
  else if (a[0] == "italic-math")
    fn[nr]= smart_font (family, variant, series, "italic", sz, dpi);
  else if (a[0] == "bold-italic-math")
    fn[nr]= smart_font (family, variant, "bold", "italic", sz, dpi);
  else if (a[0] == "tt")
    fn[nr]= smart_font (family, "tt", series, "right", sz, dpi);
  else if (a[0] == "ss")
    fn[nr]= smart_font (family, "tt", series, "right", sz, dpi);
  else if (a[0] == "bold-ss")
    fn[nr]= smart_font (family, "tt", "bold", "right", sz, dpi);
  else if (a[0] == "italic-ss")
    fn[nr]= smart_font (family, "tt", series, "italic", sz, dpi);
  else if (a[0] == "bold-italic-ss")
    fn[nr]= smart_font (family, "tt", "bold", "italic", sz, dpi);
  else if (a[0] == "cal" && N(a) == 1)
    fn[nr]= smart_font (family, "calligraphic", series, "italic", sz, dpi);
  else if (a[0] == "bold-cal")
    fn[nr]= smart_font (family, "calligraphic", "bold", "italic", sz, dpi);
  else if (a[0] == "frak")
    fn[nr]= smart_font (family, "gothic", series, "right", sz, dpi);
  else if (a[0] == "bold-frak")
    fn[nr]= smart_font (family, "gothic", "bold", "right", sz, dpi);
  else if (a[0] == "bbb" && N(a) == 1)
    fn[nr]= smart_font (family, "outline", series, "right", sz, dpi);
  else if (a[0] == "virtual")
    fn[nr]= virtual_font (this, a[1], sz, dpi);
  else {
    int ndpi= adjusted_dpi (a[0], a[1], a[2], a[3], as_int (a[4]));
    fn[nr]= closest_font (a[0], a[1], a[2], a[3], sz, ndpi, as_int (a[4]));
  }
  //cout << "Font " << nr << ", " << a << " -> " << fn[nr]->res_name << "\n";
  if (fn[nr]->res_name == res_name) {
    failed_error << "Font " << nr << ", " << a
                 << " -> " << fn[nr]->res_name << "\n";
    FAILED ("substitution font loop detected");
  }
}

static int
get_ex (string family, string variant, string series, string shape,
	int attempt) {
  array<string> lfn= logical_font (family, variant, series, shape);
  array<string> pfn= search_font (lfn, attempt);
  array<string> chs= font_database_characteristics (pfn[0], pfn[1]);
  string ex= find_attribute_value (chs, "ex");
  if (ex == "") return 0;
  else return as_int (ex);
}

int
smart_font_rep::adjusted_dpi (string fam, string var, string ser, string sh,
                              int attempt) {
  int ex1= get_ex (mfam, variant, series, rshape, 1);
  int ex2= get_ex (fam, var, ser, sh, attempt);
  double zoom= 1.0;
  if (ex1 != 0 && ex2 != 0) zoom= ((double) ex1) / ((double) ex2);
  if (zoom > 0.975 && zoom < 1.025) zoom= 1;
  //cout << mfam << ", " << fam << " -> "
  //     << ex1 << ", " << ex2 << ", " << zoom << "\n";
  return (int) tm_round (dpi * zoom);
}

/******************************************************************************
* Getting extents and drawing strings
******************************************************************************/

static string empty_string ("");

bool
smart_font_rep::supports (string c) {
  (void) c;
  return true;
}

void
smart_font_rep::get_extents (string s, metric& ex) {
  //cout << "Extents of " << s << " for " << res_name << "\n";
  int i=0, n= N(s);
  if (n == 0) fn[0]->get_extents (empty_string, ex);
  else {
    int nr;
    string r= s;
    metric ey;
    while (true) {
      advance (s, i, r, nr);
      if (nr >= 0) {
        //cout << "From " << nr << " -> " << sm->fn_spec[nr] << "\n";
        fn[nr]->get_extents (r, ex);
        break;
      }
      if (i >= n) {
        fn[0]->get_extents (empty_string, ex);
        break;
      }
    }
    while (i < n) {
      advance (s, i, r, nr);
      if (nr >= 0) {
        //cout << "From " << nr << " -> " << sm->fn_spec[nr] << "\n";
        fn[nr]->get_extents (r, ey);
        ex->y1= min (ex->y1, ey->y1);
        ex->y2= max (ex->y2, ey->y2);
        ex->x3= min (ex->x3, ex->x2 + ey->x3);
        ex->y3= min (ex->y3, ey->y3);
        ex->x4= max (ex->x4, ex->x2 + ey->x4);
        ex->y4= max (ex->y4, ey->y4);
        ex->x2 += ey->x2;
      }
    }
  }
}

void
smart_font_rep::get_xpositions (string s, SI* xpos) {
  SI x= 0;
  int i=0, n= N(s);
  xpos[0]= x;
  while (i < n) {
    int nr;
    string r= s;
    int start= i;
    advance (s, i, r, nr);
    if (nr >= 0) {
      if (r == s (start, i)) {
        fn[nr]->get_xpositions (r, xpos+start);
        for (int j=0; j<=N(r); j++) xpos[start+j] += x;
      }
      else {
        STACK_NEW_ARRAY (tmp, SI, N(r)+1);
        fn[nr]->get_xpositions (r, tmp);
        for (int j=start; j<i; j++) xpos[j]= x;
        xpos[i]= x + tmp[N(r)];
        STACK_DELETE_ARRAY (tmp);
      }
      x= xpos[i];
    }
    else
      for (int j=start; j<=i; j++) xpos[j]= x;
  }
}

void
smart_font_rep::get_xpositions (string s, SI* xpos, SI xk) {
  SI x= 0;
  int i=0, n= N(s);
  xpos[0]= x;
  while (i < n) {
    int nr;
    string r= s;
    int start= i;
    advance (s, i, r, nr);
    if (nr >= 0) {
      if (r == s (start, i)) {
        fn[nr]->get_xpositions (r, xpos+start, xk);
        for (int j=0; j<=N(r); j++) xpos[start+j] += x;
      }
      else {
        STACK_NEW_ARRAY (tmp, SI, N(r)+1);
        fn[nr]->get_xpositions (r, tmp, xk);
        for (int j=start; j<i; j++) xpos[j]= x;
        xpos[i]= x + tmp[N(r)];
        STACK_DELETE_ARRAY (tmp);
      }
      x= xpos[i];
    }
    else
      for (int j=start; j<=i; j++) xpos[j]= x;
  }
}

void
smart_font_rep::draw_fixed (renderer ren, string s, SI x, SI y) {
  int i=0, n= N(s);
  while (i < n) {
    int nr;
    string r= s;
    metric ey;
    advance (s, i, r, nr);
    if (nr >= 0) {
      fn[nr]->draw_fixed (ren, r, x, y);
      if (i < n) {
	fn[nr]->get_extents (r, ey);
	x += ey->x2;
      }
    }
  }
}

void
smart_font_rep::draw_fixed (renderer ren, string s, SI x, SI y, SI xk) {
  int i=0, n= N(s);
  while (i < n) {
    int nr;
    string r= s;
    metric ey;
    advance (s, i, r, nr);
    if (nr >= 0) {
      fn[nr]->draw_fixed (ren, r, x, y, xk);
      if (i < n) {
	fn[nr]->get_extents (r, ey, xk);
	x += ey->x2;
      }
    }
  }
}

font
smart_font_rep::magnify (double zoom) {
  return smart_font (family, variant, series, shape, sz,
                     (int) tm_round (dpi * zoom));
}

/******************************************************************************
* Other routines for fonts
******************************************************************************/

glyph
smart_font_rep::get_glyph (string s) {
  int i=0, n= N(s), nr;
  if (n == 0) return fn[0]->get_glyph (s);
  string r= s;
  advance (s, i, r, nr);
  if (nr<0) return glyph ();
  return fn[nr]->get_glyph (r);
}

double
smart_font_rep::get_left_slope  (string s) {
  int i=0, n= N(s), nr;
  if (n == 0) return fn[0]->get_left_slope (s);
  string r= s;
  advance (s, i, r, nr);
  nr= max (nr, 0);
  return fn[nr]->get_left_slope (r);
}

double
smart_font_rep::get_right_slope (string s) {
  int i=0, n= N(s), nr;
  if (n == 0) return fn[0]->get_right_slope (s);
  string r= s;
  while (i<n) advance (s, i, r, nr);
  nr= max (nr, 0);
  return fn[nr]->get_right_slope (r);
}

SI
smart_font_rep::get_left_correction  (string s) {
  int i=0, n= N(s), nr;
  if (n == 0) return fn[0]->get_left_correction (s);
  string r= s;
  advance (s, i, r, nr);
  nr= max (nr, 0);
  return fn[nr]->get_left_correction (r);
}

SI
smart_font_rep::get_right_correction (string s) {
  int i=0, n= N(s), nr;
  if (n == 0) return fn[0]->get_right_correction (s);
  string r= s;
  while (i<n) advance (s, i, r, nr);
  nr= max (nr, 0);
  return fn[nr]->get_right_correction (r);
}

/******************************************************************************
* User interface
******************************************************************************/

font
smart_font (string family, string variant, string series, string shape,
            int sz, int dpi) {
  if (!new_fonts) return find_font (family, variant, series, shape, sz, dpi);
  if (starts (family, "tc"))
    // FIXME: temporary hack for symbols from std-symbol.ts
    return find_font (family, variant, series, shape, sz, dpi);
  if (starts (family, "sys-")) {
    if (family == "sys-chinese") {
      string name= default_chinese_font_name ();
      family= "cjk=" * name * ",roman";
    }
    if (family == "sys-japanese") {
      string name= default_japanese_font_name ();
      family= "cjk=" * name * ",roman";
    }
    if (family == "sys-korean") {
      string name= default_korean_font_name ();
      family= "cjk=" * name * ",roman";
    }
  }

  string name=
    family * "-" * variant * "-" *
    series * "-" * shape * "-" *
    as_string (sz) * "-" * as_string (dpi) * "-smart";
  if (font::instances->contains (name)) return font (name);
  string sh= shape;
  if (shape == "mathitalic" || shape == "mathshape") sh= "right";
  string mfam= main_family (family);
  font base_fn= closest_font (mfam, variant, series, sh, sz, dpi);
  if (is_nil (base_fn)) return font ();
  font sec_fn= closest_font ("roman", "ss", "medium", "right", sz, dpi);
  font err_fn= error_font (sec_fn);
  return make (font, name,
               tm_new<smart_font_rep> (name, base_fn, err_fn, family, variant,
                                       series, shape, sz, dpi));
}

font
smart_font (string family, string variant, string series, string shape,
            string tfam, string tvar, string tser, string tsh,
            int sz, int dpi) {
  if (!new_fonts) return find_font (family, variant, series, shape, sz, dpi);
  if (tfam == "roman") tfam= family;
  if (variant != "mr") {
    if (variant == "ms") tvar= "ss";
    if (variant == "mt") tvar= "tt";
  }
  if (shape == "right") tsh= "mathupright";
  return smart_font (tfam, tvar, tser, tsh, sz, dpi);
}
