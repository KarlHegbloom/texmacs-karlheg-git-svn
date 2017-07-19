
/******************************************************************************
* MODULE     : font_scripts.cpp
* DESCRIPTION: microtypographic script positioning
* COPYRIGHT  : (C) 2017  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "font.hpp"
#include "analyze.hpp"

/******************************************************************************
* Helper routines for script correction tables
******************************************************************************/

void
adjust_char (hashmap<string,double>& t, string c, double delta) {
  t (c) += delta;
}

void
adjust_pair (hashmap<string,double>& t, string c, double delta) {
  adjust_char (t, c, delta);
  if (N(c) == 1) c= "<b-" * c * ">";
  else c= "<b-" * c (1, N(c));
  adjust_char (t, c, delta);
}

/******************************************************************************
* Standard corrections
******************************************************************************/

void
rsub_adjust_std (hashmap<string,double>& t) {
  adjust_pair (t, "1", -0.02);
  adjust_pair (t, "J", -0.01);
  adjust_pair (t, "P", -0.02);
  adjust_pair (t, "T", -0.03);
  adjust_pair (t, "V", -0.05);
  adjust_pair (t, "W", -0.05);
  adjust_pair (t, "Y", -0.07);
  adjust_pair (t, "<Gamma>", -0.1);
  adjust_pair (t, "<Tau>", -0.05);
  adjust_pair (t, "<Upsilon>", -0.05);
  adjust_pair (t, "<Psi>", -0.03);
  adjust_pair (t, "<gamma>", -0.02);
}

void
rsup_adjust_std (hashmap<string,double>& t) {
  adjust_pair (t, "1", -0.05);
  adjust_pair (t, "A", -0.05);
  adjust_pair (t, "L", -0.05);
  adjust_pair (t, "<Alpha>", -0.1);
  adjust_pair (t, "<Delta>", -0.1);
  adjust_pair (t, "<Lambda>", -0.1);
  adjust_pair (t, "<bbb-A>", -0.05);
  adjust_pair (t, "<bbb-L>", -0.05);
}

/******************************************************************************
* Guessing further adjustments
******************************************************************************/

void
rsub_adjust_guessed (hashmap<string,double>& t) {
  adjust_pair (t, "/", -0.05);
  adjust_pair (t, "7", -0.1);
  adjust_pair (t, "T", -0.05);
  adjust_pair (t, "V", -0.05);
  adjust_pair (t, "W", -0.05);
  adjust_pair (t, "Y", -0.05);
  adjust_pair (t, "<Gamma>", -0.03);
  adjust_pair (t, "<Rho>", -0.05);
  adjust_pair (t, "<Tau>", -0.03);
  adjust_pair (t, "<Upsilon>", -0.03);
  adjust_pair (t, "<Psi>", -0.03);
  adjust_pair (t, "<bbb-F>", -0.05);
  adjust_pair (t, "<bbb-P>", -0.05);
  adjust_pair (t, "<bbb-T>", -0.05);
  adjust_pair (t, "<bbb-V>", -0.05);
  adjust_pair (t, "<bbb-W>", -0.05);
  adjust_pair (t, "<bbb-Y>", -0.05);
}

void
rsup_adjust_guessed (hashmap<string,double>& t) {
  adjust_pair (t, "\\", -0.05);
  adjust_pair (t, "L", -0.05);
  adjust_pair (t, "<bbb-A>", -0.1);
  adjust_pair (t, "<bbb-L>", -0.1);
}

/******************************************************************************
* Interface
******************************************************************************/

static hashmap<string,double> rsub_guessed (0.0);
static hashmap<string,double> rsup_guessed (0.0);

hashmap<string,double>
rsub_guessed_table () {
  if (N (rsub_guessed) == 0) {
    rsub_adjust_std (rsub_guessed);
    rsub_adjust_guessed (rsub_guessed);
  }
  return rsub_guessed;
}

hashmap<string,double>
rsup_guessed_table () {
  if (N (rsup_guessed) == 0) {
    rsup_adjust_std (rsup_guessed);
    rsup_adjust_guessed (rsup_guessed);
  }
  return rsup_guessed;
}
