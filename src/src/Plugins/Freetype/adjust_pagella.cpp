
/******************************************************************************
* MODULE     : adjust_pagella.cpp
* DESCRIPTION: Microtypography for the TeX Gyre Pagella font
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
rsub_adjust_pagella (hashmap<string,double>& t) {
  adjust_pair (t, "!", 0.05);
  adjust_pair (t, "/", -0.05);
  adjust_pair (t, "7", -0.1);
  adjust_pair (t, "D", -0.02);
  adjust_pair (t, "N", -0.02);
  adjust_pair (t, "O", -0.02);
  adjust_pair (t, "P", -0.03);
  adjust_pair (t, "T", -0.03);
  adjust_pair (t, "V", -0.03);
  adjust_pair (t, "Y", -0.02);
  adjust_pair (t, "<Alpha>", 0.02);
  adjust_pair (t, "<Delta>", 0.02);
  adjust_pair (t, "<Epsilon>", 0.02);
  adjust_pair (t, "<Zeta>", 0.02);
  adjust_pair (t, "<Eta>", 0.02);
  adjust_pair (t, "<Theta>", -0.01);
  adjust_pair (t, "<Iota>", 0.02);
  adjust_pair (t, "<Kappa>", 0.03);
  adjust_pair (t, "<Lambda>", 0.02);
  adjust_pair (t, "<Mu>", 0.02);
  adjust_pair (t, "<Pi>", 0.02);
  adjust_pair (t, "<Sigma>", 0.02);
  adjust_pair (t, "<Omega>", 0.02);
  adjust_pair (t, "<Backepsilon>", 0.05);
  adjust_pair (t, "<Mho>", 0.02);
  adjust_char (t, "<#1D701>", 0.02);
  adjust_char (t, "<b-zeta>", 0.02);
  adjust_char (t, "<#1D709>", 0.02);
  adjust_char (t, "<b-xi>", 0.02);
  adjust_char (t, "<sigma>", -0.08);
  adjust_char (t, "<b-up-sigma>", -0.08);
  adjust_char (t, "<varpi>", -0.02);
  adjust_char (t, "<b-up-varpi>", -0.02);
  for (char c= 'A'; c <= 'Z'; c++)
    adjust_pair (t, "<cal-" * string (c) * ">", 0.07);
  for (char c= 'a'; c <= 'z'; c++)
    adjust_pair (t, "<cal-" * string (c) * ">", 0.05);
  adjust_pair (t, "<bbb-A>", -0.05);
  adjust_pair (t, "<bbb-F>", -0.05);
  adjust_pair (t, "<bbb-H>", -0.03);
  adjust_pair (t, "<bbb-J>", -0.05);
  adjust_pair (t, "<bbb-K>", -0.02);
  adjust_pair (t, "<bbb-L>", -0.03);
  adjust_pair (t, "<bbb-N>", -0.03);
  adjust_pair (t, "<bbb-O>", -0.03);
  adjust_pair (t, "<bbb-P>", -0.05);
  adjust_pair (t, "<bbb-R>", -0.03);
  adjust_pair (t, "<bbb-T>", -0.1);
  adjust_pair (t, "<bbb-U>", -0.05);
  adjust_pair (t, "<bbb-V>", -0.1);
  adjust_pair (t, "<bbb-W>", -0.1);
  adjust_pair (t, "<bbb-Y>", -0.1);
}

void
rsup_adjust_pagella (hashmap<string,double>& t) {
  adjust_pair (t, "!", 0.05);
  adjust_pair (t, "?", 0.05);
  adjust_pair (t, "/", 0.05);
  for (char c= 'A'; c <= 'Z'; c++)
    adjust_pair (t, string (c), 0.03);
  for (char c= 'a'; c <= 'z'; c++)
    adjust_pair (t, string (c), 0.02);
  adjust_pair (t, "H", -0.02);
  adjust_pair (t, "B", 0.02);
  adjust_pair (t, "D", 0.01);
  adjust_pair (t, "I", 0.02);
  adjust_pair (t, "J", 0.02);
  adjust_pair (t, "b", 0.02);
  adjust_pair (t, "l", 0.02);
  adjust_pair (t, "p", 0.02);
  adjust_pair (t, "q", 0.02);
  adjust_pair (t, "r", 0.02);
  adjust_pair (t, "s", 0.02);
  adjust_pair (t, "t", 0.02);
  adjust_pair (t, "<Beta>", 0.03);
  adjust_pair (t, "<Gamma>", 0.03);
  adjust_pair (t, "<Delta>", 0.03);
  adjust_pair (t, "<Epsilon>", 0.03);
  adjust_pair (t, "<Zeta>", 0.03);
  adjust_pair (t, "<Eta>", 0.03);
  adjust_pair (t, "<Theta>", 0.03);
  adjust_pair (t, "<Iota>", 0.03);
  adjust_pair (t, "<Kappa>", 0.03);
  adjust_pair (t, "<Lambda>", 0.03);
  adjust_pair (t, "<Mu>", 0.03);
  adjust_pair (t, "<Nu>", 0.03);
  adjust_pair (t, "<Xi>", 0.03);
  adjust_pair (t, "<Omicron>", 0.03);
  adjust_pair (t, "<Pi>", 0.03);
  adjust_pair (t, "<Rho>", 0.08);
  adjust_pair (t, "<Sigma>", 0.03);
  adjust_pair (t, "<Tau>", 0.03);
  adjust_pair (t, "<Upsilon>", 0.03);
  adjust_pair (t, "<Phi>", 0.03);
  adjust_pair (t, "<Psi>", 0.03);
  adjust_pair (t, "<Chi>", 0.03);
  adjust_pair (t, "<Omega>", 0.03);
  adjust_pair (t, "<Backepsilon>", 0.03);
  adjust_pair (t, "<Mho>", 0.03);
  adjust_char (t, "<#1D6FC>", 0.03);
  adjust_char (t, "<b-alpha>", 0.03);
  adjust_char (t, "<#1D6FD>", 0.03);
  adjust_char (t, "<b-beta>", 0.03);
  adjust_char (t, "<#1D6FE>", 0.05);
  adjust_char (t, "<b-gamma>", 0.05);
  adjust_char (t, "<#1D6FF>", 0.05);
  adjust_char (t, "<b-delta>", 0.05);
  adjust_char (t, "<#1D700>", 0.03);
  adjust_char (t, "<b-epsilon>", 0.03);
  adjust_char (t, "<#1D701>", 0.05);
  adjust_char (t, "<b-zeta>", 0.05);
  adjust_char (t, "<#1D702>", 0.05);
  adjust_char (t, "<b-eta>", 0.05);
  adjust_char (t, "<#1D703>", 0.03);
  adjust_char (t, "<b-theta>", 0.03);
  adjust_char (t, "<#1D704>", 0.03);
  adjust_char (t, "<b-iota>", 0.03);
  adjust_char (t, "<#1D705>", 0.05);
  adjust_char (t, "<b-kappa>", 0.05);
  adjust_char (t, "<#1D706>", -0.03);
  adjust_char (t, "<b-lambda>", -0.03);
  adjust_char (t, "<#1D708>", 0.05);
  adjust_char (t, "<b-nu>", 0.05);
  adjust_char (t, "<#1D709>", 0.05);
  adjust_char (t, "<b-xi>", 0.05);
  adjust_char (t, "<#1D70A>", 0.03);
  adjust_char (t, "<b-omicron>", 0.03);
  adjust_char (t, "<#1D70B>", 0.03);
  adjust_char (t, "<b-pi>", 0.03);
  adjust_char (t, "<#1D70C>", 0.03);
  adjust_char (t, "<b-rho>", 0.03);
  adjust_char (t, "<#1D70D>", 0.03);
  adjust_char (t, "<b-varsigma>", 0.03);
  adjust_char (t, "<#1D70E>", 0.03);
  adjust_char (t, "<b-sigma>", 0.03);
  adjust_char (t, "<#1D70F>", 0.03);
  adjust_char (t, "<b-tau>", 0.03);
  adjust_char (t, "<#1D710>", 0.03);
  adjust_char (t, "<b-upsilon>", 0.03);
  adjust_char (t, "<#1D711>", 0.03);
  adjust_char (t, "<b-varphi>", 0.03);
  adjust_char (t, "<#1D712>", 0.03);
  adjust_char (t, "<b-chi>", 0.03);
  adjust_char (t, "<#1D713>", 0.03);
  adjust_char (t, "<b-psi>", 0.03);
  adjust_char (t, "<#1D714>", 0.03);
  adjust_char (t, "<b-omega>", 0.03);
  adjust_char (t, "<#1D716>", 0.03);
  adjust_char (t, "<b-varepsilon>", 0.03);
  adjust_char (t, "<#1D717>", 0.07);
  adjust_char (t, "<b-vartheta>", 0.07);
  adjust_char (t, "<#1D718>", 0.02);
  adjust_char (t, "<b-varkappa>", 0.02);
  adjust_char (t, "<#1D719>", 0.03);
  adjust_char (t, "<b-phi>", 0.03);
  adjust_char (t, "<#1D71A>", 0.03);
  adjust_char (t, "<b-varrho>", 0.03);
  adjust_char (t, "<#1D71B>", 0.03);
  adjust_char (t, "<b-varpi>", 0.03);
  adjust_char (t, "<alpha>", -0.02);
  adjust_char (t, "<b-up-alpha>", -0.02);
  adjust_char (t, "<gamma>", 0.03);
  adjust_char (t, "<b-up-gamma>", 0.03);
  adjust_char (t, "<kappa>", -0.02);
  adjust_char (t, "<b-up-kappa>", -0.02);
  adjust_char (t, "<lambda>", -0.1);
  adjust_char (t, "<b-up-lambda>", -0.1);
  adjust_char (t, "<mu>", -0.05);
  adjust_char (t, "<b-up-mu>", -0.05);
  adjust_char (t, "<vartheta>", 0.03);
  adjust_char (t, "<b-up-vartheta>", 0.03);
  for (char c= 'A'; c <= 'Z'; c++)
    adjust_pair (t, "<cal-" * string (c) * ">", 0.02);
  adjust_pair (t, "<cal-A>", -0.05);
  adjust_pair (t, "<cal-B>", -0.03);
  adjust_pair (t, "<cal-f>", 0.05);
  adjust_pair (t, "<cal-l>", 0.03);
  adjust_pair (t, "<cal-t>", 0.03);
  for (char c= 'A'; c <= 'Z'; c++)
    adjust_pair (t, "<bbb-" * string (c) * ">", -0.02);
  adjust_pair (t, "<bbb-A>", -0.07);
  adjust_pair (t, "<bbb-B>", 0.02);
  adjust_pair (t, "<bbb-D>", 0.02);
  adjust_pair (t, "<bbb-E>", 0.02);
  adjust_pair (t, "<bbb-F>", 0.02);
  adjust_pair (t, "<bbb-K>", -0.07);
  adjust_pair (t, "<bbb-L>", -0.03);
  adjust_pair (t, "<bbb-M>", 0.02);
  adjust_pair (t, "<bbb-R>", -0.07);
  adjust_pair (t, "<bbb-X>", -0.03);
  adjust_pair (t, "<frak-a>", -0.03);
}

/******************************************************************************
* Interface
******************************************************************************/

static hashmap<string,double> rsub_pagella (0.0);
static hashmap<string,double> rsup_pagella (0.0);

hashmap<string,double>
rsub_pagella_table () {
  if (N (rsub_pagella) == 0) {
    rsub_adjust_std (rsub_pagella);
    rsub_adjust_pagella (rsub_pagella);
  }
  return rsub_pagella;
}

hashmap<string,double>
rsup_pagella_table () {
  if (N (rsub_pagella) == 0) {
    rsup_adjust_std (rsub_pagella);
    rsup_adjust_pagella (rsub_pagella);
  }
  return rsup_pagella;
}
