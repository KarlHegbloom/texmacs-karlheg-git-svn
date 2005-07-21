
/******************************************************************************
* MODULE     : rectangles.hpp
* DESCRIPTION: Rectangles and lists of rectangles with reference counting.
*              Used in graphical programs.
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#ifndef RECTANGLES_H
#define RECTANGLES_H
#include "list.hpp"

class rectangle_rep: concrete_struct {
public:
  SI x1, y1;
  SI x2, y2;

  rectangle_rep (SI x1b, SI y1b, SI x2b, SI y2b);
  friend class rectangle;
};

class rectangle {
  CONCRETE(rectangle);
  rectangle (SI x1b=0, SI y1b=0, SI x2b=0, SI y2b=0);
  operator tree ();
};
CONCRETE_CODE(rectangle);

ostream& operator << (ostream& out, rectangle r);
rectangle copy (rectangle r);
bool operator == (rectangle r1, rectangle r2);
bool operator != (rectangle r1, rectangle r2);
bool intersect (rectangle r1, rectangle r2);
bool operator <= (rectangle r1, rectangle r2);
rectangle translate (rectangle r, SI x, SI y);
rectangle operator / (rectangle r, int d);
double area (rectangle r);

typedef list<rectangle> rectangles;

rectangles operator - (rectangles l1, rectangles l2);
rectangles operator & (rectangles l1, rectangles l2);
rectangles operator | (rectangles l1, rectangles l2);
rectangles operator / (rectangles l, int d);
rectangles translate (rectangles l, SI x, SI y);
rectangles thicken (rectangles l, SI width, SI height);
rectangles correct (rectangles l);
rectangles simplify (rectangles l);
rectangle  least_upper_bound (rectangles l);
double area (rectangles r);

#endif // defined RECTANGLES_H
