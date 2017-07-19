
/******************************************************************************
* MODULE     : adjust_termes.cpp
* DESCRIPTION: Microtypography for the TeX Gyre Termes font
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
rsub_adjust_termes (hashmap<string,double>& t) {
  adjust_pair (t, "!", 0.05);
  adjust_pair (t, "/", -0.02);
  adjust_pair (t, "A", 0.03);
  adjust_pair (t, "B", 0.01);
  adjust_pair (t, "D", 0.02);
  adjust_pair (t, "F", -0.05);
  adjust_pair (t, "G", 0.03);
  adjust_pair (t, "I", 0.03);
  adjust_pair (t, "J", 0.02);
  adjust_pair (t, "N", -0.02);
  adjust_pair (t, "O", 0.01);
  adjust_pair (t, "P", -0.02);
  adjust_pair (t, "Q", 0.02);
  adjust_pair (t, "R", 0.02);
  adjust_pair (t, "S", 0.03);
  adjust_pair (t, "T", -0.02);
  adjust_pair (t, "U", -0.02);
  adjust_pair (t, "Z", 0.05);
  for (char c= 'a'; c <= 'z'; c++)
    adjust_pair (t, string (c), 0.02);
  adjust_pair (t, "h", 0.02);
  adjust_pair (t, "l", 0.02);
  adjust_pair (t, "m", 0.02);
  adjust_pair (t, "n", 0.01);
  adjust_pair (t, "x", 0.02);
  adjust_pair (t, "z", 0.02);
  adjust_pair (t, "<Alpha>", 0.02);
  adjust_pair (t, "<Delta>", 0.01);
  adjust_pair (t, "<Gamma>", -0.02);
  adjust_pair (t, "<Theta>", -0.02);
  adjust_pair (t, "<Iota>", 0.02);
  adjust_pair (t, "<Kappa>", 0.02);
  adjust_pair (t, "<Lambda>", 0.02);
  adjust_pair (t, "<Nu>", -0.02);
  adjust_pair (t, "<Omicron>", -0.02);
  adjust_pair (t, "<Pi>", 0.02);
  adjust_pair (t, "<Rho>", -0.02);
  adjust_pair (t, "<Tau>", -0.02);
  adjust_pair (t, "<Upsilon>", -0.05);
  adjust_pair (t, "<Chi>", 0.02);
  adjust_pair (t, "<Backepsilon>", 0.05);
  adjust_char (t, "<#1D6FC>", -0.03);
  adjust_char (t, "<b-alpha>", -0.03);
  adjust_char (t, "<#1D6FD>", -0.03);
  adjust_char (t, "<b-beta>", -0.03);
  adjust_char (t, "<#1D6FE>", -0.1);
  adjust_char (t, "<b-gamma>", -0.1);
  adjust_char (t, "<#1D701>", -0.05);
  adjust_char (t, "<b-zeta>", -0.05);
  adjust_char (t, "<#1D703>", -0.02);
  adjust_char (t, "<b-theta>", -0.02);
  adjust_char (t, "<#1D706>", -0.02);
  adjust_char (t, "<b-kappa>", -0.02);
  adjust_char (t, "<#1D708>", -0.05);
  adjust_char (t, "<b-nu>", -0.05);
  adjust_char (t, "<#1D709>", -0.05);
  adjust_char (t, "<b-xi>", -0.05);
  adjust_char (t, "<#1D70B>", -0.03);
  adjust_char (t, "<b-pi>", -0.03);
  adjust_char (t, "<#1D70C>", -0.03);
  adjust_char (t, "<b-rho>", -0.03);
  adjust_char (t, "<#1D70E>", -0.05);
  adjust_char (t, "<b-sigma>", -0.05);
  adjust_char (t, "<#1D70F>", -0.04);
  adjust_char (t, "<b-tau>", -0.04);
  adjust_char (t, "<#1D712>", -0.07);
  adjust_char (t, "<b-chi>", -0.07);
  adjust_char (t, "<#1D713>", -0.07);
  adjust_char (t, "<b-psi>", -0.07);
  adjust_char (t, "<#1D714>", -0.02);
  adjust_char (t, "<b-omega>", -0.02);
  adjust_char (t, "<#1D716>", -0.05);
  adjust_char (t, "<b-varepsilon>", -0.05);
  adjust_char (t, "<#1D71B>", -0.08);
  adjust_char (t, "<b-varpi>", -0.08);
  adjust_char (t, "<gamma>", -0.03);
  adjust_char (t, "<b-up-gamma>", -0.03);
  adjust_char (t, "<psi>", -0.05);
  adjust_char (t, "<b-up-psi>", -0.05);
  adjust_pair (t, "<cal-A>", 0.03);
  adjust_pair (t, "<cal-C>", -0.1);
  adjust_pair (t, "<cal-F>", -0.2);
  adjust_pair (t, "<cal-G>", -0.1);
  adjust_pair (t, "<cal-H>", 0.05);
  adjust_pair (t, "<cal-I>", -0.12);
  adjust_pair (t, "<cal-J>", -0.05);
  adjust_pair (t, "<cal-K>", -0.05);
  adjust_pair (t, "<cal-M>", 0.03);
  adjust_pair (t, "<cal-N>", -0.3);
  adjust_pair (t, "<cal-P>", -0.03);
  adjust_pair (t, "<cal-S>", -0.1);
  adjust_pair (t, "<cal-T>", -0.3);
  adjust_pair (t, "<cal-V>", -0.3);
  adjust_pair (t, "<cal-W>", -0.3);
  adjust_pair (t, "<cal-X>", -0.1);
  adjust_pair (t, "<cal-Y>", -0.05);
  adjust_pair (t, "<cal-a>", -0.05);
  adjust_pair (t, "<cal-c>", -0.02);
  adjust_pair (t, "<cal-d>", -0.08);
  adjust_pair (t, "<cal-e>", -0.03);
  adjust_pair (t, "<cal-f>", -0.12);
  adjust_pair (t, "<cal-g>", -0.05);
  adjust_pair (t, "<cal-h>", -0.05);
  adjust_pair (t, "<cal-i>", -0.03);
  adjust_pair (t, "<cal-j>", -0.05);
  adjust_pair (t, "<cal-l>", -0.1);
  adjust_pair (t, "<cal-p>", -0.03);
  adjust_pair (t, "<cal-q>", -0.02);
  adjust_pair (t, "<cal-t>", -0.03);
  adjust_pair (t, "<cal-x>", -0.02);
  adjust_pair (t, "<cal-y>", -0.02);
  adjust_pair (t, "<cal-z>", -0.05);
  adjust_pair (t, "<bbb-A>", -0.03);
  adjust_pair (t, "<bbb-D>", -0.02);
  adjust_pair (t, "<bbb-F>", -0.07);
  adjust_pair (t, "<bbb-H>", 0.02);
  adjust_pair (t, "<bbb-I>", 0.03);
  adjust_pair (t, "<bbb-M>", 0.02);
  adjust_pair (t, "<bbb-P>", -0.03);
  adjust_pair (t, "<bbb-Q>", 0.05);
  adjust_pair (t, "<bbb-S>", 0.03);
  adjust_pair (t, "<bbb-T>", -0.05);
  adjust_pair (t, "<bbb-U>", -0.05);
  adjust_pair (t, "<bbb-V>", -0.12);
  adjust_pair (t, "<bbb-W>", -0.12);
  adjust_pair (t, "<bbb-Y>", -0.15);
  adjust_pair (t, "<bbb-Z>", 0.03);
}
  
void
rsup_adjust_termes (hashmap<string,double>& t) {
  adjust_pair (t, "!", 0.05);
  adjust_pair (t, "?", 0.05);
  adjust_pair (t, "/", 0.05);
  adjust_pair (t, "0", 0.03);
  adjust_pair (t, "B", 0.03);
  adjust_pair (t, "D", 0.02);
  adjust_pair (t, "G", 0.02);
  adjust_pair (t, "H", -0.02);
  adjust_pair (t, "I", -0.02);
  adjust_pair (t, "P", 0.03);
  adjust_pair (t, "Q", 0.01);
  adjust_pair (t, "R", 0.03);
  adjust_pair (t, "W", 0.02);
  adjust_pair (t, "Z", 0.02);
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
  adjust_pair (t, "<Alpha>", -0.05);
  adjust_pair (t, "<Gamma>", 0.02);
  adjust_pair (t, "<Delta>", -0.02);
  adjust_pair (t, "<Epsilon>", -0.02);
  adjust_pair (t, "<Lambda>", -0.05);
  adjust_pair (t, "<Rho>", 0.02);
  adjust_pair (t, "<Phi>", 0.02);
  adjust_pair (t, "<Psi>", 0.01);
  adjust_pair (t, "<Tau>", 0.02);
  adjust_pair (t, "<Backepsilon>", 0.04);
  adjust_pair (t, "<Mho>", 0.02);
  adjust_char (t, "<#1D6FD>", 0.02);
  adjust_char (t, "<b-beta>", 0.02);
  adjust_char (t, "<#1D701>", 0.03);
  adjust_char (t, "<b-zeta>", 0.03);
  adjust_char (t, "<#1D703>", 0.03);
  adjust_char (t, "<b-theta>", 0.03);
  adjust_char (t, "<#1D709>", 0.03);
  adjust_char (t, "<b-xi>", 0.03);
  adjust_char (t, "<#1D70D>", 0.02);
  adjust_char (t, "<b-varsigma>", 0.02);
  adjust_pair (t, "<mho>", 0.02);
  adjust_char (t, "<beta>", -0.03);
  adjust_char (t, "<b-up-beta>", -0.03);
  adjust_char (t, "<zeta>", -0.05);
  adjust_char (t, "<b-up-zeta>", -0.05);
  adjust_char (t, "<eta>", -0.03);
  adjust_char (t, "<b-up-eta>", -0.03);
  adjust_char (t, "<theta>", -0.03);
  adjust_char (t, "<b-up-theta>", -0.03);
  adjust_char (t, "<iota>", -0.05);
  adjust_char (t, "<b-up-iota>", -0.05);
  adjust_char (t, "<lambda>", -0.17);
  adjust_char (t, "<b-up-lambda>", -0.17);
  adjust_char (t, "<mu>", -0.07);
  adjust_char (t, "<b-up-mu>", -0.07);
  adjust_char (t, "<xi>", -0.01);
  adjust_char (t, "<b-up-xi>", -0.01);
  adjust_char (t, "<omicron>", -0.03);
  adjust_char (t, "<b-up-omicron>", -0.03);
  adjust_char (t, "<psi>", -0.02);
  adjust_char (t, "<b-up-psi>", -0.02);
  adjust_char (t, "<chi>", -0.05);
  adjust_char (t, "<b-up-chi>", -0.05);
  adjust_char (t, "<omega>", -0.03);
  adjust_char (t, "<b-up-omega>", -0.03);
  adjust_char (t, "<varepsilon>", -0.03);
  adjust_char (t, "<b-up-varepsilon>", -0.03);
  adjust_char (t, "<vartheta>", -0.02);
  adjust_char (t, "<b-up-vartheta>", -0.02);
  adjust_pair (t, "<cal-I>", -0.03);
  adjust_pair (t, "<bbb-A>", -0.05);
  adjust_pair (t, "<bbb-D>", 0.02);
  adjust_pair (t, "<bbb-H>", 0.02);
  adjust_pair (t, "<bbb-J>", 0.01);
  adjust_pair (t, "<bbb-K>", -0.03);
  adjust_pair (t, "<bbb-L>", -0.05);
  adjust_pair (t, "<bbb-M>", 0.01);
  adjust_pair (t, "<bbb-P>", 0.02);
  adjust_pair (t, "<bbb-R>", -0.03);
  adjust_pair (t, "<frak-Q>", -0.02);
  adjust_pair (t, "<frak-U>", -0.03);
  adjust_pair (t, "<frak-a>", -0.03);
  adjust_pair (t, "<frak-b>", -0.01);
  adjust_pair (t, "<frak-i>", -0.02);
  adjust_pair (t, "<frak-j>", 0.01);
  adjust_pair (t, "<frak-l>", -0.01);
  adjust_pair (t, "<frak-p>", -0.01);
  adjust_pair (t, "<frak-u>", -0.03);
  adjust_pair (t, "<frak-v>", -0.02);
  adjust_pair (t, "<frak-w>", -0.02);
  adjust_pair (t, "<frak-y>", -0.01);
}

/******************************************************************************
* Interface
******************************************************************************/

static hashmap<string,double> rsub_termes (0.0);
static hashmap<string,double> rsup_termes (0.0);

hashmap<string,double>
rsub_termes_table () {
  if (N (rsub_termes) == 0) {
    rsub_adjust_std (rsub_termes);
    rsub_adjust_termes (rsub_termes);
  }
  return rsub_termes;
}

hashmap<string,double>
rsup_termes_table () {
  if (N (rsup_termes) == 0) {
    rsup_adjust_std (rsup_termes);
    rsup_adjust_termes (rsup_termes);
  }
  return rsup_termes;
}
