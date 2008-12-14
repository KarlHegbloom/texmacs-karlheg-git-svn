
/******************************************************************************
* MODULE     : prompt.cpp
* DESCRIPTION: Shows how to produce prompts
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

static int counter= 0;

void
next_input () {
  counter++;
  cout << DATA_BEGIN << "prompt#";
  cout << "Input " << counter << "] ";
  cout << DATA_END;
}

int
main () {
  cout << DATA_BEGIN << "verbatim:";
  cout << "A LaTeX -> TeXmacs converter";
  next_input ();
  cout << DATA_END;
  fflush (stdout);

  while (true) {
    char buffer[100];
    cin.getline (buffer, 100, '\n');
    cout << DATA_BEGIN << "verbatim:";
    cout << DATA_BEGIN;
    cout << "latex:$" << buffer << "$";
    cout << DATA_END;
    next_input ();
    cout << DATA_END;
    fflush (stdout);
  }
  return 0;
}
