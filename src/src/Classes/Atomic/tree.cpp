
/******************************************************************************
* MODULE     : tree.cpp
* DESCRIPTION: fixed size trees with reference counting
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "tree.hpp"
#include "drd_std.hpp"

/******************************************************************************
* Main routines for trees
******************************************************************************/

#ifdef OS_WIN32
const tree_label tree::init=UNINIT;
#endif

void
destroy_tree_rep (tree_rep* rep) {
  if (rep->op == STRING) delete (static_cast<atomic_rep*> (rep));
  else delete (static_cast<compound_rep*>(rep));
}

tree::tree (tree_label l, tree t1):
  rep (new compound_rep (l, array<tree> (1))) {
  (static_cast<compound_rep*> (rep))->a[0]=t1;
}

tree::tree (tree_label l, tree t1, tree t2):
  rep (new compound_rep (l, array<tree> (2))) {
  (static_cast<compound_rep*> (rep))->a[0]=t1;
  (static_cast<compound_rep*> (rep))->a[1]=t2;
}

tree::tree (tree_label l, tree t1, tree t2, tree t3):
  rep (new compound_rep (l, array<tree> (3))) {
  (static_cast<compound_rep*> (rep))->a[0]=t1;
  (static_cast<compound_rep*> (rep))->a[1]=t2;
  (static_cast<compound_rep*> (rep))->a[2]=t3;
}

tree::tree (tree_label l, tree t1, tree t2, tree t3, tree t4):
  rep (new compound_rep (l, array<tree> (4))) {
  (static_cast<compound_rep*> (rep))->a[0]=t1;
  (static_cast<compound_rep*> (rep))->a[1]=t2;
  (static_cast<compound_rep*> (rep))->a[2]=t3;
  (static_cast<compound_rep*> (rep))->a[3]=t4;
}

tree::tree (tree_label l, tree t1, tree t2, tree t3, tree t4, tree t5):
  rep (new compound_rep (l, array<tree> (5))) {
  (static_cast<compound_rep*> (rep))->a[0]=t1;
  (static_cast<compound_rep*> (rep))->a[1]=t2;
  (static_cast<compound_rep*> (rep))->a[2]=t3;
  (static_cast<compound_rep*> (rep))->a[3]=t4;
  (static_cast<compound_rep*> (rep))->a[4]=t5;
}

tree::tree (tree_label l,
	    tree t1, tree t2, tree t3, tree t4, tree t5, tree t6):
  rep (new compound_rep (l, array<tree> (6)))
{
  (static_cast<compound_rep*> (rep))->a[0]=t1;
  (static_cast<compound_rep*> (rep))->a[1]=t2;
  (static_cast<compound_rep*> (rep))->a[2]=t3;
  (static_cast<compound_rep*> (rep))->a[3]=t4;
  (static_cast<compound_rep*> (rep))->a[4]=t5;
  (static_cast<compound_rep*> (rep))->a[5]=t6;
}

tree
tree::operator () (int begin, int end) {
  int i;
  tree r (rep->op, end-begin);
  for (i=begin; i<end; i++)
    r[i-begin]= (static_cast<compound_rep*> (rep))->a[i];
  return r;
}

bool
operator == (tree t, tree u) {
  return (L(t)==L(u)) &&
    (L(t)==STRING? (t->label==u->label): (A(t)==A(u)));
}

bool
operator != (tree t, tree u) {
  return (L(t)!=L(u)) ||
    (L(t)==STRING? (t->label!=u->label): (A(t)!=A(u)));
}

tree
copy (tree t) {
  if (is_atomic (t)) return tree (copy (t->label));
  else {
    int i, n= N(t);
    tree t2 (t, n);
    for (i=0; i<n; i++) t2[i]= copy (t[i]);
    return t2;
  }
}

tree&
operator << (tree& t, tree t2) {
  CHECK_COMPOUND (t, "operator << (tree&, tree)");
  (static_cast<compound_rep*> (t.rep))->a << t2;
  return t;
}

tree&
operator << (tree& t, array<tree> a) {
  CHECK_COMPOUND (t, "operator << (tree&, array<tree>)");
  (static_cast<compound_rep*> (t.rep))->a << a;
  return t;
}

ostream&
operator << (ostream& out, tree t) {
  if (is_atomic (t)) return out << t->label;
  else {
    int i, n= N(t);
    out << as_string (L(t));
    if (n==0) return out << "()";
    out << " (";
    for (i=0; i< n-1; i++)
      out << t[i] << ", ";
    out << t[i] << ")";
    return out;
  }
}

void
print_tree (tree t, int tab) {
  int i;
  for (i=0; i<tab; i++) cout << " ";
  if (is_atomic (t)) cout << t->label << "\n";
  else {
    cout << as_string (L(t)) << "\n";
    for (i=0; i<N(t); i++) print_tree (t[i], tab+2);
  }
}

int
hash (array<tree> a) {
  register int i, h=0, n=N(a);
  for (i=0; i<n; i++) {
    h=(h<<7) + (h>>25);
    h=h + hash(a[i]);
  }
  return h;
}

int
hash (tree t) {
  if (is_atomic (t)) return hash (t->label);
  else return ((int) L(t)) ^ hash (A(t));
}

/******************************************************************************
* Tree predicates
******************************************************************************/

bool
is_document (tree t) {
  return L(t) == DOCUMENT;
}

bool
is_concat (tree t) {
  return L(t) == CONCAT;
}

bool
is_format (tree t) {
  return is_document (t) || is_concat (t);
}

bool
is_formatting (tree t) {
  return (L(t)>=WITH_LIMITS) && (L(t)<=NEW_DPAGE);
}

bool
is_table (tree t) {
  return
    is_func (t, TABLE) || is_func (t, SUBTABLE) ||
    is_func (t, ROW) || is_func (t, CELL);
}

bool
is_table_format (tree t) {
  return is_func (t, TFORMAT);
}

bool
is_multi_paragraph (tree t) {
  switch (L(t)) {
  case DOCUMENT:
    return true;
  case SURROUND:
    return is_multi_paragraph (t[2]);
  case DATOMS:
  case DLINES:
  case DPAGES:
  case WITH:
  case MARK:
    return is_multi_paragraph (t[N(t)-1]);
  case INCLUDE:
    return true;
  default:
    if (L(t) < START_EXTENSIONS) return false;
    else {
      int i, n= N(t);
      if (as_string (L(t)) == "footnote") return false;
      for (i=0; i<n; i++)
	if (is_multi_paragraph (t[i]))
	  return true;
      return false;
    }
  }
}

bool
is_script (tree t) {
  return
    is_func (t, LSUB) || is_func (t, LSUP) ||
    is_func (t, RSUB) || is_func (t, RSUP);
}

bool
is_script (tree t, bool& right) {
  if (is_func (t, LSUB) ||
      is_func (t, LSUP)) { right=false; return true; }
  if (is_func (t, RSUB) ||
      is_func (t, RSUP)) { right=true; return true; }
  return false;
}

bool
is_prime (tree t) {
  return ((L(t) == LPRIME) || (L(t) == RPRIME)) && (N(t) == 1);
}

bool
is_inactive (tree t) {
  return
    (N(t) == 1) &&
    ((L(t) == INACTIVE) || (L(t) == VAR_INACTIVE));
}

bool
is_empty (tree t) {
  if (is_atomic (t)) return (t == "");
  if (is_document (t) || is_concat (t)) {
    int i, n= N(t);
    for (i=0; i<n; i++)
      if (!is_empty (t[i])) return false;
    return is_concat (t) || (n<=1);
  }
  return false;
}

/******************************************************************************
* Compound trees
******************************************************************************/

tree
compound (string s) {
  return tree (make_tree_label (s));
}

tree
compound (string s, tree t1) {
  return tree (make_tree_label (s), t1);
}

tree
compound (string s, tree t1, tree t2) {
  return tree (make_tree_label (s), t1, t2);
}

tree
compound (string s, tree t1, tree t2, tree t3) {
  return tree (make_tree_label (s), t1, t2, t3);
}

tree
compound (string s, tree t1, tree t2, tree t3, tree t4) {
  return tree (make_tree_label (s), t1, t2, t3, t4);
}

bool
is_extension(tree_label l) {
  return l >= START_EXTENSIONS;
}

bool
is_extension (tree t) {
  return L(t) >= START_EXTENSIONS;
}

bool
is_extension (tree t, int n) {
  return (L(t) >= START_EXTENSIONS) && (N(t) == n);
}

bool
is_compound (tree t, string s) {
  return as_string (L(t)) == s;
}

bool
is_compound (tree t, string s, int n) {
  return (as_string (L(t)) == s) && (N(t) == n);
}

/******************************************************************************
* Routines for simplification and correction
******************************************************************************/

static void
simplify_concat (tree& r, tree t) {
  int i, n= N(t);
  for (i=0; i<n; i++)
    if (is_concat (t[i])) simplify_concat (r, t[i]);
    else if (t[i] == "");
    else if (is_atomic (t[i]) && (N(r)>0) && is_atomic (r[N(r)-1]))
      r[N(r)-1]= tree (r[N(r)-1]->label * t[i]->label);
    else r << t[i];
}

tree
simplify_concat (tree t) {
  tree r (CONCAT);
  simplify_concat (r, t);
  if (N(r) == 0) return "";
  if (N(r) == 1) return r[0];
  return r;
}

tree
simplify_correct (tree t) {
  if (is_atomic (t)) return t;
  if (is_func (t, QUOTE, 1) && (is_atomic (t[0]))) return t[0];
  int i, n= N(t);
  tree r (t, n);
  for (i=0; i<n; i++)
    r[i]= simplify_correct (t[i]);
  if (is_concat (r)) r= simplify_concat (r);
  return r;
}
