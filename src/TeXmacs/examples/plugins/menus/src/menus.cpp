
/******************************************************************************
* MODULE     : menus.cpp
* DESCRIPTION: A plugin which changes the menus dynamically
*              The routine menus-add is defined in ../progs/init-menus.scm
* COPYRIGHT  : (C) 2003  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include <stdio.h>
#include <iostream.h>

#define DATA_BEGIN   ((char) 2)
#define DATA_END     ((char) 5)
#define DATA_ESCAPE  ((char) 27)

int
main () {
  cout << DATA_BEGIN << "verbatim:";
  cout << "Enter the name of a menu to add at each prompt";
  cout << DATA_END;
  fflush (stdout);

  while (true) {
    char buffer[100];
    cin.getline (buffer, 100, '\n');
    cout << DATA_BEGIN << "verbatim:";
    cout << DATA_BEGIN << "command:(menus-add \""
	 << buffer << "\")" << DATA_END;
    cout << "Added " << buffer << " to menu";
    cout << DATA_END;
    fflush (stdout);
  }
  return 0;
}
