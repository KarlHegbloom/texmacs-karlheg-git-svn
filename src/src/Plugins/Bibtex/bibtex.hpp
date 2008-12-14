
/******************************************************************************
* MODULE     : bibtex.hpp
* DESCRIPTION: generating bibliographies using BiBTeX
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef BIBTEX_H
#define BIBTEX_H
#include "tree.hpp"

void set_bibtex_command (string cmd);
tree bibtex_run (string bib, string style, string dir, string fname,
		 tree bib_t);

#endif // BIBTEX_H
