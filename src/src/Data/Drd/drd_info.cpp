
/******************************************************************************
* MODULE     : drd_info.cpp
* DESCRIPTION: data relation descriptions
* COPYRIGHT  : (C) 2003  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "drd_info.hpp"
#include "iterator.hpp"

/******************************************************************************
* Constructors and basic operations
******************************************************************************/

drd_info_rep::drd_info_rep (string name2):
  name (name2), info (tag_info ()) {}
drd_info_rep::drd_info_rep (string name2, drd_info base):
  name (name2), info (tag_info (), base->info) {}
drd_info::drd_info (string name):
  rep (new drd_info_rep (name)) {}
drd_info::drd_info (string name, drd_info base):
  rep (new drd_info_rep (name, base)) {}

tree
drd_info_rep::get_locals () {
  tree t (COLLECTION);
  iterator<tree_label> it= iterate (info->item);
  while (it->busy()) {
    tree_label l= it->next();
    tree v= (tree) info->item[l];
    t << tree (ASSOCIATE, as_string (l), v);
  }
  return t;
}

void
drd_info_rep::set_locals (tree t) {
  if (!is_func (t, COLLECTION))
    fatal_error ("Bad set locals", "drd_info_rep::set_locals");
  int i, n= N(t);
  for (i=0; i<n; i++)
    if (is_func (t[i], ASSOCIATE, 2) && is_atomic (t[i][0]))
      info (make_tree_label (t[i][0]->label))= tag_info (t[i][1]);
}

bool
drd_info_rep::contains (string l) {
  return existing_tree_label (l) && info->contains (as_tree_label (l));
}

ostream&
operator << (ostream& out, drd_info drd) {
  return out << "drd [" << drd->name << "]";
}

/******************************************************************************
* Arity related methods
******************************************************************************/

void
drd_info_rep::set_arity (tree_label l, int arity, int extra, int am, int cm) {
  if (info[l]->pi.freeze_arity) return;
  if (!info->contains (l)) info(l)= copy (info[l]);
  tag_info& ti= info(l);
  ti->pi.arity_mode= am;
  ti->pi.child_mode= cm;
  if (am != ARITY_VAR_REPEAT) {
    ti->pi.arity_base = arity;
    ti->pi.arity_extra= extra;
  }
  else {
    ti->pi.arity_base = extra;
    ti->pi.arity_extra= arity;
  }
  int n;
  if (arity+extra == 0) n= 0;
  else if (cm == CHILD_UNIFORM) n= 1;
  else if (cm == CHILD_BIFORM) n= 2;
  else n= arity+extra;
  if (N(ti->ci) != n) ti->ci= array<child_info> (n);
}

int
drd_info_rep::get_arity_mode (tree_label l) {
  return info[l]->pi.arity_mode;
}

int
drd_info_rep::get_child_mode (tree_label l) {
  return info[l]->pi.child_mode;
}

int
drd_info_rep::get_arity_base (tree_label l) {
  return info[l]->pi.arity_base;
}

int
drd_info_rep::get_arity_extra (tree_label l) {
  return info[l]->pi.arity_extra;
}

int
drd_info_rep::get_nr_indices (tree_label l) {
  return N(info[l]->ci);
}

void
drd_info_rep::freeze_arity (tree_label l) {
  if (!info->contains (l)) info(l)= copy (info[l]);
  tag_info& ti= info(l);
  ti->pi.freeze_arity= true;
}

int
drd_info_rep::get_old_arity (tree_label l) {
  tag_info ti= info[l];
  if (ti->pi.arity_mode != ARITY_NORMAL) return -1;
  else return ((int) ti->pi.arity_base) + ((int) ti->pi.arity_extra);
}

bool
drd_info_rep::is_dynamic (tree t) {
  if (L(t) >= START_EXTENSIONS) return true; // FIXME: temporary fix
  if (is_atomic (t)) return false;
  if (is_func (t, DOCUMENT) || is_func (t, PARAGRAPH) || is_func (t, CONCAT) ||
      is_func (t, TABLE) || is_func (t, ROW)) return false;
  return info[L(t)]->pi.arity_mode != ARITY_NORMAL;
}

/******************************************************************************
* Border accessability related methods
******************************************************************************/

void
drd_info_rep::set_no_border (tree_label l, bool has_no_border) {
  if (info[l]->pi.freeze_no_border) return;
  if (!info->contains (l)) info(l)= copy (info[l]);
  tag_info& ti= info(l);
  ti->pi.no_border= has_no_border;
}

bool
drd_info_rep::get_no_border (tree_label l) {
  return info[l]->pi.no_border;
}

void
drd_info_rep::freeze_no_border (tree_label l) {
  if (!info->contains (l)) info(l)= copy (info[l]);
  tag_info& ti= info(l);
  ti->pi.freeze_no_border= true;
}

bool
drd_info_rep::is_child_enforcing (tree t) {
  return info[L(t)]->pi.no_border && (N(t) != 0);
}

/******************************************************************************
* Children's accessability related methods
******************************************************************************/

void
drd_info_rep::set_accessible (tree_label l, int nr, bool is_accessible) {
  if (!info->contains (l)) info(l)= copy (info[l]);
  tag_info  & ti= info(l);
  child_info& ci= ti->ci[nr];
  if (ci.freeze_accessible) return;
  ci.accessible= is_accessible;
}

bool
drd_info_rep::get_accessible (tree_label l, int nr) {
  return info[l]->ci[nr].accessible;
}

bool
drd_info_rep::all_accessible (tree_label l) {
  int i, n= N(info[l]->ci);
  for (i=0; i<n; i++)
    if (!info[l]->ci[i].accessible)
      return false;
  return n>0;
}

void
drd_info_rep::freeze_accessible (tree_label l, int nr) {
  if (!info->contains (l)) info(l)= copy (info[l]);
  tag_info  & ti= info(l);
  child_info& ci= ti->ci[nr];
  ci.freeze_accessible= true;
}

bool
drd_info_rep::is_accessible_child (tree t, int i) {
  tag_info ti= info[L(t)];
  int index= ti->get_index (i, N(t));
  if ((index<0) || (index>=N(ti->ci))) return false;
  return ti->ci[index].accessible;
}

/******************************************************************************
* Heuristic initialization of DRD
******************************************************************************/

static bool
accessible_arg (drd_info_rep* drd, tree t, tree arg) {
  if (is_atomic (t)) return false;
  else if (t == arg) return true;
  else if (is_func (t, MAP_ARGS) && (t[2] == arg[0])) {
    if ((N(t) >= 4) && (N(arg) >= 2) && (as_int (t[3]) > as_int (arg[1])))
      return false;
    if ((N(t) == 5) && (N(arg) >= 2) && (as_int (t[3]) <= as_int (arg[1])))
      return false;
    tree_label inner= make_tree_label (as_string (t[0]));
    tree_label outer= make_tree_label (as_string (t[1]));
    return
      (drd->get_nr_indices (inner) > 0) &&
      drd->get_accessible (inner, 0) &&
      drd->all_accessible (outer);
  }
  else if (is_func (t, MACRO)) return false;
  else {
    int i, n= N(t);
    for (i=0; i<n; i++)
      if (drd->is_accessible_child (t, i))
	if (accessible_arg (drd, t[i], arg))
	  return true;
    return false;
  }
}

bool
drd_info_rep::heuristic_init_macro (string var, tree macro) {
  tree_label l = make_tree_label (var);
  tag_info old_ti= copy (info[l]);
  int i, n= N(macro)-1;
  set_arity (l, n, 0, ARITY_NORMAL, CHILD_DETAILED);
  for (i=0; i<n; i++) {
    tree arg (ARGUMENT, macro[i]);
    set_accessible (l, i, accessible_arg (this, macro[n], arg));
  }
  // if (old_ti != info[l])
  //   cout << var << ": " << old_ti << " -> " << info[l] << "\n";
  return (old_ti != info[l]);
}

static int
minimal_arity (tree t, tree var) {
  if (is_atomic (t)) return 0;
  else if (is_func (t, ARGUMENT, 2) && (t[0] == var))
    return as_int (t[1]) + 1;
  else if (is_func (t, MAP_ARGS) && (N(t)>=4) && (t[2] == var))
    return as_int (t[3]);
  else {
    int i, n= N(t), m= 0;
    for (i=0; i<n; i++)
      m= max (m, minimal_arity (t[i], var));
    return m;
  }
}

bool
drd_info_rep::heuristic_init_xmacro (string var, tree xmacro) {
  tree_label l = make_tree_label (var);
  tag_info old_ti= copy (info[l]);
  int i, m= minimal_arity (xmacro[1], xmacro[0]);
  set_arity (l, m, 1, ARITY_REPEAT, CHILD_DETAILED);
  for (i=0; i<=m; i++) {
    tree arg (ARGUMENT, xmacro[0], as_string (i));
    set_accessible (l, i, accessible_arg (this, xmacro[1], arg));
  }
  // if (old_ti != info[l])
  //   cout << var << ": " << old_ti << " -> " << info[l] << "\n";
  return (old_ti != info[l]);
}

void
drd_info_rep::heuristic_init (hashmap<string,tree> env) {
  // int tt= texmacs_time ();
  bool flag= true;
  int round= 0;
  while (flag) {
    // cout << HRULE;
    flag= false;
    iterator<string> it= iterate (env);
    while (it->busy()) {
      string var= it->next();
      tree   val= env[var];
      if (is_func (val, MACRO))
	flag= heuristic_init_macro (var, val) | flag;
      if (is_func (val, XMACRO))
	flag= heuristic_init_xmacro (var, val) | flag;
    }
    if ((round++) == 10) {
      cout << "TeXmacs] Warning: bad heuristic drd convergence\n";
      flag= false;
    }
  }
  // cout << "--> " << (texmacs_time ()-tt) << "ms\n";
}
