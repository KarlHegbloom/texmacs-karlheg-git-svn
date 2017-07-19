
/******************************************************************************
* MODULE     : adjust_schola.cpp
* DESCRIPTION: Microtypography for the TeX Gyre Schola font
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
rsub_adjust_schola (hashmap<string,double>& t) {
  adjust_pair (t, "!", 0.05);
  adjust_pair (t, "/", -0.05);
  adjust_pair (t, "1", 0.02);
  adjust_pair (t, "4", 0.02);
  adjust_pair (t, "A", 0.03);
  adjust_pair (t, "D", -0.05);
  adjust_pair (t, "F", -0.05);
  adjust_pair (t, "G", 0.02);
  adjust_pair (t, "J", -0.02);
  adjust_pair (t, "K", 0.02);
  adjust_pair (t, "N", -0.05);
  adjust_pair (t, "O", -0.02);
  adjust_pair (t, "P", -0.03);
  adjust_pair (t, "Q", 0.02);
  adjust_pair (t, "U", -0.03);
  adjust_pair (t, "V", -0.03);
  adjust_pair (t, "W", -0.03);
  adjust_pair (t, "X", 0.03);
  adjust_pair (t, "e", 0.01);
  adjust_pair (t, "f", -0.01);
  adjust_pair (t, "l", 0.01);
  adjust_pair (t, "<Gamma>", -0.05);
  adjust_pair (t, "<Theta>", -0.02);
  adjust_pair (t, "<Xi>", 0.03);
  adjust_pair (t, "<Rho>", -0.05);
  adjust_pair (t, "<Tau>", -0.02);
  adjust_pair (t, "<Upsilon>", -0.05);
  adjust_pair (t, "<Psi>", -0.05);
  adjust_pair (t, "<Chi>", 0.02);
  adjust_pair (t, "<Backepsilon>", 0.02);
  adjust_char (t, "<#1D708>", 0.02);
  adjust_char (t, "<b-nu>", 0.02);
  adjust_char (t, "<#1D71B>", -0.02);
  adjust_char (t, "<b-varpi>", -0.02);
  adjust_pair (t, "<cal-C>", -0.1);
  adjust_pair (t, "<cal-F>", -0.15);
  adjust_pair (t, "<cal-G>", -0.15);
  adjust_pair (t, "<cal-I>", -0.15);
  adjust_pair (t, "<cal-K>", -0.05);
  adjust_pair (t, "<cal-N>", -0.2);
  adjust_pair (t, "<cal-O>", -0.05);
  adjust_pair (t, "<cal-P>", -0.05);
  adjust_pair (t, "<cal-S>", -0.15);
  adjust_pair (t, "<cal-T>", -0.3);
  adjust_pair (t, "<cal-V>", -0.25);
  adjust_pair (t, "<cal-W>", -0.25);
  adjust_pair (t, "<cal-X>", -0.05);
  adjust_pair (t, "<cal-Y>", -0.05);
  adjust_pair (t, "<cal-d>", -0.1);
  adjust_pair (t, "<cal-f>", -0.1);
  adjust_pair (t, "<cal-l>", -0.1);
  adjust_pair (t, "<bbb-A>", 0.01);
  adjust_pair (t, "<bbb-F>", -0.15);
  adjust_pair (t, "<bbb-I>", 0.02);
  adjust_pair (t, "<bbb-J>", -0.02);
  adjust_pair (t, "<bbb-K>", 0.01);
  adjust_pair (t, "<bbb-P>", -0.07);
  adjust_pair (t, "<bbb-U>", -0.07);
  adjust_pair (t, "<bbb-V>", -0.1);
  adjust_pair (t, "<bbb-W>", -0.1);
  adjust_pair (t, "<bbb-X>", 0.01);
  adjust_pair (t, "<bbb-Y>", -0.15);
  adjust_pair (t, "<frak-V>", -0.03);
  adjust_pair (t, "<frak-W>", -0.03);
  adjust_pair (t, "<frak-f>", -0.03);
}

void
rsup_adjust_schola (hashmap<string,double>& t) {
  adjust_pair (t, "!", 0.05);
  adjust_pair (t, "#", 0.05);
  adjust_pair (t, "$", 0.05);
  adjust_pair (t, "?", 0.05);
  adjust_pair (t, "0", 0.05);
  adjust_pair (t, "2", 0.03);
  adjust_pair (t, "9", 0.05);
  adjust_pair (t, "\\", -0.05);
  adjust_pair (t, "|", 0.05);
  adjust_pair (t, "A", -0.05);
  adjust_pair (t, "C", 0.03);
  adjust_pair (t, "D", 0.02);
  adjust_pair (t, "G", 0.02);
  adjust_pair (t, "J", 0.02);
  adjust_pair (t, "M", 0.03);
  adjust_pair (t, "P", 0.03);
  adjust_pair (t, "Q", 0.02);
  adjust_pair (t, "V", 0.02);
  adjust_pair (t, "X", 0.01);
  adjust_pair (t, "Y", 0.01);
  adjust_pair (t, "Z", 0.03);
  adjust_pair (t, "f", 0.02);
  adjust_pair (t, "g", 0.03);
  adjust_pair (t, "h", -0.03);
  adjust_pair (t, "i", 0.02);
  adjust_pair (t, "j", 0.03);
  adjust_pair (t, "l", 0.03);
  adjust_pair (t, "m", -0.02);
  adjust_pair (t, "n", -0.02);
  adjust_pair (t, "o", 0.01);
  adjust_pair (t, "q", 0.02);
  adjust_pair (t, "r", 0.03);
  adjust_pair (t, "s", 0.02);
  adjust_pair (t, "t", 0.02);
  adjust_pair (t, "x", 0.01);
  adjust_pair (t, "y", 0.01);
  adjust_pair (t, "z", 0.01);
  adjust_char (t, "<b-A>", 0.02);
  adjust_char (t, "<b-L>", 0.02);
  adjust_pair (t, "<Alpha>", -0.05);
  adjust_pair (t, "<Delta>", -0.03);
  adjust_pair (t, "<Iota>", 0.01);
  adjust_pair (t, "<Lambda>", -0.04);
  adjust_pair (t, "<Xi>", 0.01);
  adjust_char (t, "<#1D6FE>", 0.04);
  adjust_char (t, "<b-gamma>", 0.04);
  adjust_char (t, "<#1D6FF>", 0.04);
  adjust_char (t, "<b-delta>", 0.04);
  adjust_char (t, "<#1D700>", 0.03);
  adjust_char (t, "<b-epsilon>", 0.03);
  adjust_char (t, "<#1D703>", 0.05);
  adjust_char (t, "<b-theta>", 0.05);
  adjust_char (t, "<#1D704>", -0.02);
  adjust_char (t, "<b-iota>", -0.02);
  adjust_char (t, "<#1D706>", -0.12);
  adjust_char (t, "<b-lambda>", -0.12);
  adjust_char (t, "<#1D707>", -0.06);
  adjust_char (t, "<b-mu>", -0.06);
  adjust_char (t, "<#1D70B>", 0.03);
  adjust_char (t, "<b-pi>", 0.03);
  adjust_char (t, "<#1D70D>", 0.02);
  adjust_char (t, "<b-varsigma>", 0.02);
  adjust_char (t, "<#1D70E>", 0.02);
  adjust_char (t, "<b-sigma>", 0.02);
  adjust_char (t, "<#1D70F>", 0.04);
  adjust_char (t, "<b-tau>", 0.04);
  adjust_char (t, "<#1D710>", 0.03);
  adjust_char (t, "<b-upsilon>", 0.03);
  adjust_char (t, "<#1D716>", 0.03);
  adjust_char (t, "<b-varepsilon>", 0.03);
  adjust_char (t, "<#1D717>", 0.03);
  adjust_char (t, "<b-vartheta>", 0.03);
  adjust_char (t, "<#1D71B>", 0.02);
  adjust_char (t, "<b-varpi>", 0.02);
  adjust_char (t, "<alpha>", -0.02);
  adjust_char (t, "<b-up-alpha>", -0.02);
  adjust_char (t, "<gamma>", 0.02);
  adjust_char (t, "<b-up-gamma>", 0.02);
  adjust_char (t, "<epsilon>", 0.02);
  adjust_char (t, "<b-up-epsilon>", 0.02);
  adjust_char (t, "<varepsilon>", 0.02);
  adjust_char (t, "<b-up-varepsilon>", 0.02);
  adjust_char (t, "<iota>", -0.03);
  adjust_char (t, "<b-up-iota>", -0.03);
  adjust_char (t, "<lambda>", -0.15);
  adjust_char (t, "<b-up-lambda>", -0.5);
  adjust_char (t, "<mu>", -0.1);
  adjust_char (t, "<b-up-mu>", -0.1);
  adjust_char (t, "<chi>", -0.05);
  adjust_char (t, "<b-up-chi>", -0.05);
  adjust_pair (t, "<cal-C>", -0.01);
  adjust_pair (t, "<cal-E>", -0.01);
  adjust_pair (t, "<cal-F>", -0.03);
  adjust_pair (t, "<cal-G>", -0.02);
  adjust_pair (t, "<cal-I>", -0.02);
  adjust_pair (t, "<cal-J>", -0.01);
  adjust_pair (t, "<cal-S>", -0.01);
  adjust_pair (t, "<cal-T>", -0.01);
  adjust_pair (t, "<cal-Z>", -0.03);
  adjust_pair (t, "<cal-a>", -0.03);
  adjust_pair (t, "<cal-d>", -0.01);
  adjust_pair (t, "<cal-m>", -0.02);
  adjust_pair (t, "<cal-n>", -0.02);
  adjust_pair (t, "<cal-o>", -0.01);
  adjust_pair (t, "<cal-p>", -0.02);
  adjust_pair (t, "<cal-q>", -0.01);
  adjust_pair (t, "<cal-u>", -0.02);
  adjust_pair (t, "<cal-x>", 0.01);
  adjust_pair (t, "<cal-y>", -0.02);
  adjust_pair (t, "<bbb-A>", -0.08);
  adjust_pair (t, "<bbb-K>", -0.02);
  adjust_pair (t, "<bbb-L>", -0.07);
  adjust_pair (t, "<bbb-P>", 0.01);
  adjust_pair (t, "<bbb-X>", -0.02);
  adjust_pair (t, "<frak-A>", -0.04);
  adjust_pair (t, "<frak-U>", -0.04);
  adjust_pair (t, "<frak-a>", -0.04);
  adjust_pair (t, "<frak-i>", -0.03);
  adjust_pair (t, "<frak-j>", 0.03);
  adjust_pair (t, "<frak-m>", -0.02);
  adjust_pair (t, "<frak-n>", -0.02);
  adjust_pair (t, "<frak-u>", -0.04);
}

/******************************************************************************
* Interface
******************************************************************************/

static hashmap<string,double> rsub_schola (0.0);
static hashmap<string,double> rsup_schola (0.0);

hashmap<string,double>
rsub_schola_table () {
  if (N (rsub_schola) == 0) {
    rsub_adjust_std (rsub_schola);
    rsub_adjust_schola (rsub_schola);
  }
  return rsub_schola;
}

hashmap<string,double>
rsup_schola_table () {
  if (N (rsub_schola) == 0) {
    rsup_adjust_std (rsub_schola);
    rsup_adjust_schola (rsub_schola);
  }
  return rsup_schola;
}
