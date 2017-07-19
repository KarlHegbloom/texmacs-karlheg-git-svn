
/******************************************************************************
* MODULE     : adjust_stix.cpp
* DESCRIPTION: Microtypography for the Stix font
* COPYRIGHT  : (C) 2017  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "font.hpp"

/******************************************************************************
* Table initialization
******************************************************************************/

void
rsub_adjust_stix (hashmap<string,double>& t) {
  adjust_pair (t, "!", 0.05);
  adjust_pair (t, "/", -0.02);
  adjust_pair (t, "B", -0.01);
  adjust_pair (t, "N", -0.02);
  adjust_pair (t, "O", -0.02);
  adjust_pair (t, "P", -0.02);
  adjust_pair (t, "T", -0.02);
  adjust_pair (t, "U", -0.02);
  adjust_pair (t, "<Alpha>", 0.02);
  adjust_pair (t, "<Delta>", 0.01);
  adjust_pair (t, "<Eta>", 0.02);
  adjust_pair (t, "<Theta>", -0.02);
  adjust_pair (t, "<Iota>", 0.02);
  adjust_pair (t, "<Kappa>", 0.02);
  adjust_pair (t, "<Lambda>", 0.02);
  adjust_pair (t, "<Omicron>", -0.02);
  adjust_pair (t, "<Backepsilon>", 0.05);
  adjust_char (t, "<#1D714>", -0.01);
  adjust_char (t, "<b-omega>", -0.01);
  adjust_char (t, "<gamma>", -0.05);
  adjust_char (t, "<b-up-gamma>", -0.05);
  adjust_char (t, "<sigma>", -0.03);
  adjust_char (t, "<b-up-sigma>", -0.03);
  adjust_char (t, "<tau>", -0.03);
  adjust_char (t, "<b-up-tau>", -0.03);
  adjust_char (t, "<varpi>", -0.05);
  adjust_char (t, "<b-up-varpi>", -0.05);
  adjust_pair (t, "<cal-A>", -0.05);
  adjust_pair (t, "<cal-B>", -0.03);
  adjust_pair (t, "<cal-C>", -0.05);
  adjust_pair (t, "<cal-D>", -0.03);
  adjust_pair (t, "<cal-E>", -0.05);
  adjust_pair (t, "<cal-F>", -0.05);
  adjust_pair (t, "<cal-G>", -0.05);
  adjust_pair (t, "<cal-H>", -0.1);
  adjust_pair (t, "<cal-I>", -0.15);
  adjust_pair (t, "<cal-J>", -0.1);
  adjust_pair (t, "<cal-K>", -0.1);
  adjust_pair (t, "<cal-L>", -0.1);
  adjust_pair (t, "<cal-M>", -0.05);
  adjust_pair (t, "<cal-N>", -0.15);
  adjust_pair (t, "<cal-S>", -0.1);
  adjust_pair (t, "<cal-T>", -0.25);
  adjust_pair (t, "<cal-U>", -0.05);
  adjust_pair (t, "<cal-V>", -0.25);
  adjust_pair (t, "<cal-W>", -0.25);
  adjust_pair (t, "<cal-X>", -0.15);
  adjust_pair (t, "<cal-Y>", -0.15);
  adjust_pair (t, "<cal-Z>", -0.1);
  adjust_pair (t, "<cal-a>", -0.05);
  adjust_pair (t, "<cal-c>", -0.05);
  adjust_pair (t, "<cal-d>", -0.05);
  adjust_pair (t, "<cal-e>", -0.05);
  adjust_pair (t, "<cal-f>", -0.1);
  adjust_pair (t, "<cal-g>", -0.05);
  adjust_pair (t, "<cal-h>", -0.05);
  adjust_pair (t, "<cal-i>", -0.03);
  adjust_pair (t, "<cal-j>", -0.05);
  adjust_pair (t, "<cal-k>", -0.03);
  adjust_pair (t, "<cal-l>", -0.08);
  adjust_pair (t, "<cal-p>", -0.03);
  adjust_pair (t, "<cal-q>", -0.02);
  adjust_pair (t, "<cal-s>", -0.03);
  adjust_pair (t, "<cal-t>", -0.07);
  adjust_pair (t, "<cal-u>", -0.03);
  adjust_pair (t, "<cal-x>", -0.05);
  adjust_pair (t, "<cal-y>", -0.05);
  adjust_pair (t, "<cal-z>", -0.07);
  adjust_pair (t, "<bbb-D>", -0.02);
  adjust_pair (t, "<bbb-F>", 0.05);
  adjust_pair (t, "<bbb-I>", 0.03);
  adjust_pair (t, "<bbb-J>", 0.05);
  adjust_pair (t, "<bbb-M>", 0.03);
  adjust_pair (t, "<bbb-N>", 0.02);
  adjust_pair (t, "<bbb-Q>", 0.02);
  adjust_pair (t, "<bbb-T>", 0.05);
  adjust_pair (t, "<bbb-V>", 0.07);
  adjust_pair (t, "<bbb-W>", 0.07);
  adjust_pair (t, "<bbb-Y>", 0.05);
  adjust_pair (t, "<bbb-Z>", -0.03);
}
  
void
rsup_adjust_stix (hashmap<string,double>& t) {
  adjust_pair (t, "!", 0.05);
  adjust_pair (t, "?", 0.05);
  adjust_pair (t, "/", 0.05);
  adjust_pair (t, "B", 0.03);
  adjust_pair (t, "D", 0.01);
  adjust_pair (t, "P", 0.03);
  adjust_pair (t, "Q", 0.01);
  adjust_pair (t, "R", 0.03);
  adjust_pair (t, "W", 0.02);
  adjust_pair (t, "a", 0.01);
  adjust_pair (t, "b", 0.02);
  adjust_pair (t, "c", 0.02);
  adjust_pair (t, "d", 0.01);
  adjust_pair (t, "e", 0.02);
  adjust_pair (t, "i", 0.02);
  adjust_pair (t, "l", 0.02);
  adjust_pair (t, "q", 0.01);
  adjust_pair (t, "r", 0.02);
  adjust_pair (t, "s", 0.02);
  adjust_pair (t, "t", 0.02);
  adjust_pair (t, "x", 0.02);
  adjust_pair (t, "<Gamma>", 0.02);
  adjust_pair (t, "<Theta>", 0.02);
  adjust_pair (t, "<Rho>", 0.02);
  adjust_pair (t, "<Phi>", 0.02);
  adjust_pair (t, "<Psi>", 0.01);
  adjust_pair (t, "<Tau>", 0.02);
  adjust_pair (t, "<Backepsilon>", 0.04);
  adjust_pair (t, "<Mho>", 0.02);
  adjust_char (t, "<#1D6FD>", 0.02);
  adjust_char (t, "<b-beta>", 0.02);
  adjust_char (t, "<#1D701>", 0.04);
  adjust_char (t, "<b-zeta>", 0.03);
  adjust_char (t, "<#1D703>", 0.05);
  adjust_char (t, "<b-theta>", 0.05);
  adjust_char (t, "<#1D709>", 0.04);
  adjust_char (t, "<b-xi>", 0.03);
  adjust_char (t, "<#1D717>", 0.02);
  adjust_char (t, "<b-vartheta>", 0.02);
  adjust_char (t, "<#1D70D>", 0.02);
  adjust_char (t, "<b-varsigma>", 0.02);
  adjust_pair (t, "<mho>", 0.02);
  adjust_char (t, "<zeta>", -0.03);
  adjust_char (t, "<b-up-zeta>", -0.03);
  adjust_char (t, "<xi>", -0.01);
  adjust_char (t, "<b-up-xi>", -0.01);
  adjust_char (t, "<psi>", -0.02);
  adjust_char (t, "<b-up-psi>", -0.02);
  adjust_pair (t, "<bbb-A>", -0.02);
  adjust_pair (t, "<bbb-D>", 0.02);
  adjust_pair (t, "<bbb-H>", 0.02);
  adjust_pair (t, "<bbb-J>", 0.01);
  adjust_pair (t, "<bbb-L>", -0.02);
  adjust_pair (t, "<bbb-M>", 0.01);
  adjust_pair (t, "<bbb-P>", 0.02);
  adjust_pair (t, "<frak-a>", -0.02);
  adjust_pair (t, "<frak-i>", -0.02);
  adjust_pair (t, "<frak-j>", 0.01);
  adjust_pair (t, "<frak-l>", -0.01);
  adjust_pair (t, "<frak-p>", 0.02);
  adjust_pair (t, "<frak-r>", 0.01);
  adjust_pair (t, "<frak-t>", 0.01);
  adjust_pair (t, "<frak-u>", -0.01);
}

/******************************************************************************
* Interface
******************************************************************************/

static hashmap<string,double> rsub_stix (0.0);
static hashmap<string,double> rsup_stix (0.0);

hashmap<string,double>
rsub_stix_table () {
  if (N (rsub_stix) == 0) {
    rsub_adjust_std (rsub_stix);
    rsub_adjust_stix (rsub_stix);
  }
  return rsub_stix;
}

hashmap<string,double>
rsup_stix_table () {
  if (N (rsup_stix) == 0) {
    rsup_adjust_std (rsup_stix);
    rsup_adjust_stix (rsup_stix);
  }
  return rsup_stix;
}
