
/******************************************************************************
* MODULE     : list.hpp
* DESCRIPTION: linked lists with reference counting
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#ifndef LIST_H
#define LIST_H
#include "tree.hpp"

class tree;
template<class T> class list_rep;
template<class T> class list;

template<class T> bool nil (list<T> l);
template<class T> bool atom (list<T> l);
template<class T> bool strong_equal (list<T> l1, list<T> l2);

template<class T> class list {
  CONCRETE_NULL_TEMPLATE(list,T);
  inline list (T item);
  inline list (T item, list<T> next);
  inline list (T item1, T item2, list<T> next);
  inline list (T item1, T item2, T item3, list<T> next);
  T& operator [] (int i);
  operator tree ();

  friend bool atom LESSGTR (list<T> l);
  friend bool strong_equal LESSGTR (list<T> l1, list<T> l2);
};

extern int list_count;
template<class T> class list_rep: concrete_struct {
public:
  T       item;
  list<T> next;

  inline list_rep<T> (T item2, list<T> next2): item(item2), next(next2) {
    DEBUG(list_count++); }
  inline ~list_rep<T> () { DEBUG(list_count--); }
  friend class list<T>;
};

CONCRETE_NULL_TEMPLATE_CODE(list,class,T);
#define TMPL template<class T>
TMPL inline list<T>::list (T item): rep (new list_rep<T>(item, list<T> ())) {}
TMPL inline list<T>::list (T item, list<T> next):
  rep (new list_rep<T>(item, next)) {}
TMPL inline list<T>::list (T item1, T item2, list<T> next):
  rep (new list_rep<T>(item1, list<T> (item2, next))) {}
TMPL inline list<T>::list (T item1, T item2, T item3, list<T> next):
  rep (new list_rep<T>(item1, list<T> (item2, item3, next))) {}
TMPL inline bool atom (list<T> l) { return (!nil (l)) && nil (l->next); }

TMPL int      N (list<T> l);
TMPL list<T>  copy (list<T> l);
TMPL list<T>  operator * (list<T> l1, T x);
TMPL list<T>  operator * (list<T> l1, list<T> l2);
TMPL list<T>  head (list<T> l, int n=1);
TMPL list<T>  tail (list<T> l, int n=1);
TMPL T        last_item (list<T> l);
TMPL T&       access_last (list<T>& l);
TMPL list<T>& suppress_last (list<T>& l);
TMPL list<T>  reverse (list<T> l);

TMPL ostream& operator << (ostream& out, list<T> l);
TMPL list<T>& operator << (list<T>& l, T item);
TMPL list<T>& operator << (list<T>& l1, list<T> l2);
TMPL list<T>& operator >> (T item, list<T>& l);
TMPL list<T>& operator << (T& item, list<T>& l);
TMPL bool     operator == (list<T> l1, list<T> l2);
TMPL bool     operator != (list<T> l1, list<T> l2);
TMPL bool     operator < (list<T> l1, list<T> l2);
TMPL bool     operator <= (list<T> l1, list<T> l2);
#undef TMPL

#include "list.cpp"

#endif // defined LIST_H
