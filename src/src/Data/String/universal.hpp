
/******************************************************************************
* MODULE     : universal.hpp
* DESCRIPTION: Internationalization for the universal character encoding
* COPYRIGHT  : (C) 2015  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef UNIVERSAL_H
#define UNIVERSAL_H
#include "analyze.hpp"

string uni_translit (string s);

string uni_locase_char (string s);
string uni_upcase_char (string s);
string uni_locase_first (string s);
string uni_upcase_first (string s);
string uni_locase_all (string s);
string uni_upcase_all (string s);

#endif // defined UNIVERSAL_H
