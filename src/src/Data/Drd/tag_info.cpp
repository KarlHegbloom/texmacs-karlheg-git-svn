
/******************************************************************************
* MODULE     : tag_info.cpp
* DESCRIPTION: DRD information about tags
* COPYRIGHT  : (C) 2003  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "tag_info.hpp"

#define get_bits(which,nr) which=i&((1<<nr)-1);i=i>>nr
#define set_bits(which,nr) i+=((int)which)<<offset;offset+=nr

/******************************************************************************
* Properties of the tag
******************************************************************************/

parent_info::parent_info (int a, int x, int am, int cm, bool frozen) {
  arity_mode       = am;
  arity_base       = a;
  arity_extra      = x;
  child_mode       = cm;
  no_border        = false;
  block            = false;
  freeze_arity     = frozen;
  freeze_no_border = frozen;
  freeze_block     = frozen;
}

parent_info::parent_info (string s) {
  int i= as_int (s);
  get_bits (arity_mode      , 2);
  get_bits (arity_base      , 6);
  get_bits (arity_extra     , 4);
  get_bits (child_mode      , 2);
  get_bits (no_border       , 1);
  get_bits (block           , 2);
  get_bits (freeze_arity    , 1);
  get_bits (freeze_no_border, 1);
  get_bits (freeze_block    , 1);
}

parent_info::operator string () {
  int i=0, offset=0;
  set_bits (arity_mode      , 2);
  set_bits (arity_base      , 6);
  set_bits (arity_extra     , 4);
  set_bits (child_mode      , 2);
  set_bits (no_border       , 1);
  set_bits (block           , 2);
  set_bits (freeze_arity    , 1);
  set_bits (freeze_no_border, 1);
  set_bits (freeze_block    , 1);
  return as_string (i);
}

bool
parent_info::operator == (const parent_info& pi) {
  return
    (arity_mode       == pi.arity_mode      ) &&
    (arity_base       == pi.arity_base      ) &&
    (arity_extra      == pi.arity_extra     ) &&
    (child_mode       == pi.child_mode      ) &&
    (no_border        == pi.no_border       ) &&
    (block            == pi.block           ) &&
    (freeze_arity     == pi.freeze_arity    ) &&
    (freeze_no_border == pi.freeze_no_border) &&
    (freeze_block     == pi.freeze_block    );
}

bool
parent_info::operator != (const parent_info& pi) {
  return !(operator == (pi));
}

ostream&
operator << (ostream& out, parent_info pi) {
  return out << ((string) pi);
}

/******************************************************************************
* Properties of the children of the tag
******************************************************************************/

child_info::child_info (bool frozen) {
  accessible         = ACCESSIBLE_NEVER;
  writability        = WRITABILITY_NORMAL;
  block              = 0;
  mode               = MODE_PARENT;
  freeze_accessible  = frozen;
  freeze_writability = frozen;
  freeze_block       = frozen;
  freeze_mode        = frozen;
}

child_info::child_info (string s) {
  int i= as_int (s);
  get_bits (accessible        , 2);
  get_bits (writability       , 2);
  get_bits (block             , 2);
  get_bits (mode              , 3);
  get_bits (freeze_accessible , 1);
  get_bits (freeze_writability, 1);
  get_bits (freeze_block      , 1);
  get_bits (freeze_mode       , 1);
}

child_info::operator string () {
  int i=0, offset=0;
  set_bits (accessible        , 2);
  set_bits (writability       , 2);
  set_bits (block             , 2);
  set_bits (mode              , 3);
  set_bits (freeze_accessible , 1);
  set_bits (freeze_writability, 1);
  set_bits (freeze_block      , 1);
  set_bits (freeze_mode       , 1);
  return as_string (i);
}

bool
child_info::operator == (const child_info& ci) {
  return
    (accessible         == ci.accessible        ) &&
    (writability        == ci.writability       ) &&
    (block              == ci.block             ) &&
    (mode               == ci.mode              ) &&
    (freeze_accessible  == ci.freeze_accessible ) &&
    (freeze_writability == ci.freeze_writability) &&
    (freeze_block       == ci.freeze_block      ) &&
    (freeze_mode        == ci.freeze_mode       );
}

bool
child_info::operator != (const child_info& ci) {
  return !(operator == (ci));
}

ostream&
operator << (ostream& out, child_info ci) {
  return out << ((string) ci);
}

/******************************************************************************
* Constructors, destructors and converters
******************************************************************************/

tag_info_rep::tag_info_rep (parent_info pi2, array<child_info> ci2, tree x):
  pi (pi2), ci (ci2), extra (x) {}

tag_info_rep::tag_info_rep (int a, int x, int am, int cm, bool frozen):
  pi (a, x, am, cm, frozen),
  ci ((a+x)==0? 0: (cm==CHILD_UNIFORM? 1: (cm==CHILD_BIFORM? 2: (am+cm))))
{
  if (frozen) {
    int i, n= N(ci);
    for (i=0; i<n; i++)
      ci[i]= child_info (true);
  }
}

tag_info::tag_info (parent_info pi, array<child_info> ci, tree extra) {
  rep= new tag_info_rep (pi, ci, extra);
}

tag_info::tag_info (int a, int x, int am, int cm, bool frozen) {
  rep= new tag_info_rep (a, x, am, cm, frozen);
}

tag_info::tag_info (tree t) {
  if ((!is_func (t, TUPLE)) || (N(t)<2) || (L(t[1]) != TUPLE)) {
    cerr << "\nt= " << t << "\n";
    fatal_error ("Bad tag_info", "tag_info::tag_info (tree)");
  }
  parent_info pi (as_string (t[0]));
  int i, n= N(t[1]);
  array<child_info> ci (n);
  for (i=0; i<n; i++)
    ci[i]= as_string (t[1][i]);
  rep= new tag_info_rep (pi, ci, N(t)==3? t[2]: tree (""));
}

tag_info::operator tree () {
  if (rep->extra == "") return tree (TUPLE, (string) rep->pi, (tree) rep->ci);
  else return tree (TUPLE, (string) rep->pi, (tree) rep->ci, rep->extra);
}

/******************************************************************************
* Access routines and getting the index of a child
******************************************************************************/

tag_info
tag_info_rep::no_border () {
  pi.no_border= true;
  return tag_info (pi, ci, extra);
}

tag_info
tag_info_rep::accessible (int i) {
  ci[i].accessible= ACCESSIBLE_ALWAYS;
  return tag_info (pi, ci, extra);
}

tag_info
tag_info_rep::hidden (int i) {
  ci[i].accessible= ACCESSIBLE_HIDDEN;
  return tag_info (pi, ci, extra);
}

tag_info
tag_info_rep::disable_writable (int i) {
  ci[i].writability= WRITABILITY_DISABLE;
  return tag_info (pi, ci, extra);
}

tag_info
tag_info_rep::enable_writable (int i) {
  ci[i].writability= WRITABILITY_ENABLE;
  return tag_info (pi, ci, extra);
}

tag_info
tag_info_rep::name (string s) {
  set_attribute ("name", s);
  return tag_info (pi, ci, extra);
}

void
tag_info_rep::set_attribute (string which, tree val) {
  if (extra == "") extra= tree (ATTR);
  extra << tree (which) << val;
}

tree
tag_info_rep::get_attribute (string which) {
  if (!is_func (extra, ATTR)) return "";
  int i, n= N(extra);
  for (i=0; i+1<n; i+=2)
    if (extra[i] == which)
      return extra[i+1];
  return "";
}

int
tag_info_rep::get_index (int child, int n) {
  switch (pi.child_mode) {
  case CHILD_UNIFORM:
    return 0;
  case CHILD_BIFORM:
    if (pi.arity_mode != ARITY_VAR_REPEAT) {
      if (child < ((int) pi.arity_base)) return 0;
      else return 1;
    }
    else {
      if (child < (n-((int) pi.arity_base))) return 0;
      else return 1;
    }
  case CHILD_DETAILED:
    if (((int) pi.arity_mode) <= ARITY_OPTIONS)
      return child;
    else if (pi.arity_mode == ARITY_REPEAT) {
      if (child < ((int) pi.arity_base)) return child;
      else return (child - pi.arity_base) % pi.arity_extra + pi.arity_base;
    }
    else {
      if (child < (n-((int) pi.arity_base))) return child % pi.arity_extra;
      else return pi.arity_base + pi.arity_extra + child - n;
    }
  }
  return 0;
}

child_info&
tag_info::operator () (int child, int n) {
  return rep->ci [rep->get_index (child, n)];
}

/******************************************************************************
* Usual extra routines
******************************************************************************/

ostream&
operator << (ostream& out, tag_info ti) {
  out << "[ " << ti->pi << ", " << ti->ci;
  if (ti->extra != "") out << ", " << ti->extra << "\n";
  return out << " ]";
}

tag_info
copy (tag_info ti) {
  return tag_info (ti->pi, copy (ti->ci), copy (ti->extra));
}

bool
operator == (tag_info ti1, tag_info ti2) {
  return
    (ti1->pi == ti2->pi) && (ti1->ci == ti2->ci) && (ti1->extra == ti2->extra);
}

bool
operator != (tag_info ti1, tag_info ti2) {
  return !(ti1 == ti2);
}
