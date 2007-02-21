
/******************************************************************************
* MODULE     : bridge.cpp
* DESCRIPTION: Bridge between logical and physically typesetted document
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "bridge.hpp"
#include "Boxes/construct.hpp"

bridge bridge_document (typesetter, tree, path);
bridge bridge_surround (typesetter, tree, path);
bridge bridge_hidden (typesetter, tree, path);
bridge bridge_formatting (typesetter, tree, path, string);
bridge bridge_with (typesetter, tree, path);
bridge bridge_rewrite (typesetter, tree, path);
bridge bridge_argument (typesetter, tree, path);
bridge bridge_default (typesetter, tree, path);
bridge bridge_compound (typesetter, tree, path);
bridge bridge_mark (typesetter, tree, path);
bridge bridge_expand_as (typesetter, tree, path);
bridge bridge_eval (typesetter, tree, path);
bridge bridge_auto (typesetter, tree, path, tree, bool);
bridge bridge_locus (typesetter, tree, path);
bridge bridge_ornament (typesetter, tree, path);
bridge bridge_canvas (typesetter, tree, path);

bridge nil_bridge;

/******************************************************************************
* Constructors and basic operations
******************************************************************************/

bridge_rep::bridge_rep (typesetter ttt2, tree st2, path ip2):
  ttt (ttt2), env (ttt->env), st (st2), ip (ip2),
  status (CORRUPTED), changes (UNINIT) {}

static tree inactive_auto
  (MACRO, "x", tree (REWRITE_INACTIVE, tree (ARG, "x"), "recurse*"));
static tree error_m
  (MACRO, "x", tree (REWRITE_INACTIVE, tree (ARG, "x", "0"), "error*"));
static tree inactive_m
  (MACRO, "x", tree (REWRITE_INACTIVE, tree (ARG, "x", "0"), "once*"));
static tree var_inactive_m
  (MACRO, "x", tree (REWRITE_INACTIVE, tree (ARG, "x", "0"), "recurse*"));

bridge
make_inactive_bridge (typesetter ttt, tree st, path ip) {
  if (is_document (st))
    return bridge_document (ttt, st, ip);
  else return bridge_auto (ttt, st, ip, inactive_auto, false);
}

bridge
make_bridge (typesetter ttt, tree st, path ip) {
  // cout << "Make bridge " << st << ", " << ip << "\n";
  // cout << "Preamble mode= " << ttt->env->preamble << "\n";
  if (ttt->env->preamble)
    return make_inactive_bridge (ttt, st, ip);
  switch (L(st)) {
  case ERROR:
    return bridge_auto (ttt, st, ip, error_m, true);
  case DOCUMENT:
    return bridge_document (ttt, st, ip);
  case SURROUND:
    return bridge_surround (ttt, st, ip);
  case HIDDEN:
    return bridge_hidden (ttt, st, ip);
  case DATOMS:
    return bridge_formatting (ttt, st, ip, ATOM_DECORATIONS);
  case DLINES:
    return bridge_formatting (ttt, st, ip, LINE_DECORATIONS);
  case DPAGES:
    return bridge_formatting (ttt, st, ip, PAGE_DECORATIONS);
  case TFORMAT:
    return bridge_formatting (ttt, st, ip, CELL_FORMAT);
  case WITH:
    return bridge_with (ttt, st, ip);
  case COMPOUND:
    return bridge_compound (ttt, st, ip);
  case ARG:
    return bridge_argument (ttt, st, ip);
  case MARK:
    return bridge_mark (ttt, st, ip);
  case EXPAND_AS:
    return bridge_expand_as (ttt, st, ip);
  case EVAL:
  case QUASI:
    return bridge_eval (ttt, st, ip);
  case EXTERN:
    return bridge_rewrite (ttt, st, ip);
  case INCLUDE:
    return bridge_rewrite (ttt, st, ip);
  case STYLE_ONLY:
  case VAR_STYLE_ONLY:
  case ACTIVE:
  case VAR_ACTIVE:
    return bridge_compound (ttt, st, ip);
  case INACTIVE:
    return bridge_auto (ttt, st, ip, inactive_m, true);
  case VAR_INACTIVE:
    return bridge_auto (ttt, st, ip, var_inactive_m, true);
  case REWRITE_INACTIVE:
    return bridge_rewrite (ttt, st, ip);
  case LOCUS:
    return bridge_locus (ttt, st, ip);
  case HLINK:
  case ACTION:
    return bridge_compound (ttt, st, ip);
  case CANVAS:
    return bridge_canvas (ttt, st, ip);
  case ORNAMENT:
    return bridge_ornament (ttt, st, ip);
  default:
    if (L(st) < START_EXTENSIONS) return bridge_default (ttt, st, ip);
    else return bridge_compound (ttt, st, ip);
  }
}

void
replace_bridge (bridge& br, tree st, path ip) {
  bridge new_br= make_bridge (br->ttt, st, ip);
  new_br->changes= br->changes;
  br= new_br;
}

bool
bridge::operator == (bridge item2) {
  return rep == item2.rep;
}

bool
bridge::operator != (bridge item2) {
  return rep != item2.rep;
}

ostream&
operator << (ostream& out, bridge br) {
  return out << "bridge [" << br->st << ", " << br->ip << "]";
}

/******************************************************************************
* Event notification
******************************************************************************/

void
bridge_rep::notify_insert (path p, tree u) {
  // cout << "Insert " << p << ", " << u << " in " << st << "\n";
  path q= path_up (p);
  int  l= last_item (p);
  tree t= subtree (st, q);
  if (is_atomic (t)) {
    if (is_compound (u))
      fatal_error ("two atoms expected", "bridge_rep::notify_insert");
    t= t->label (0, l) * u->label * t->label (l, N(t->label));
  }
  else t= (t (0, l) * u) * t (l, N(t));
  notify_assign (q, t);
}

void
bridge_rep::notify_remove (path p, int nr) {
  // cout << "Insert " << p << ", " << nr << " in " << st << "\n";
  path q= path_up (p);
  int  l= last_item (p);
  tree t= subtree (st, q);
  if (is_atomic (t)) t= t->label (0, l) * t->label (l+nr, N(t->label));
  else t= t (0, l) * t (l+nr, N(t));
  notify_assign (q, t);
}

void
bridge_rep::notify_split (path p) {
  // cout << "Split " << p << " in " << st << "\n";
  path q  = path_up (p, 2);
  int  pos= last_item (path_up (p));
  int  l  = last_item (p);
  tree t  = subtree (st, q);

  if (is_atomic (t[pos])) {
    string s1= t[pos]->label (0, l), s2= t[pos]->label (l, N (t[pos]->label));
    notify_insert (q * pos, tree (L(t), s1));
    notify_assign (q * (pos+1), s2);
  }
  else {
    tree t1= t[pos] (0, l), t2= t[pos] (l, N(t[pos]));
    notify_insert (q * pos, tree (L(t), t1));
    notify_assign (q * (pos+1), t2);
  }
}

void
bridge_rep::notify_join (path p) {
  // cout << "Join " << p << " in " << st << "\n";
  path q  = path_up (p);
  int  pos= last_item (p);
  tree t  = subtree (st, q);

  if (is_atomic (t[pos]) && is_atomic (t[pos+1])) {
    string j= t[pos]->label * t[pos+1]->label;
    notify_remove (q * pos, 1);
    notify_assign (q * pos, j);
  }
  else {
    tree j= t[pos] * t[pos+1];
    notify_remove (q * pos, 1);
    notify_assign (q * pos, j);
  }
}

/******************************************************************************
* Getting environment variables and typesetting
******************************************************************************/

void
bridge_rep::my_clean_links () {
  link_env= link_repository (true);
}

void
bridge_rep::my_exec_until (path p) {
  env->exec_until (st, p);
}

bool
bridge_rep::my_typeset_will_be_complete () {
  return (status & VALID_MASK) == CORRUPTED;
}

void
bridge_rep::my_typeset (int desired_status) {
  if ((desired_status & WANTED_MASK) == WANTED_PARAGRAPH)
    ttt->insert_paragraph (st, ip);
  if ((desired_status & WANTED_MASK) == WANTED_PARUNIT)
    ttt->insert_parunit (st, ip);
}

void
bridge_rep::exec_until (path p, bool skip_flag) {
  // This virtual routine is redefined in bridge_auto in order to
  // treat cursor positions on the border in a special way depending
  // on skip_flag

  (void) skip_flag;
  // cout << "Exec until " << p << " in " << st << "\n";
  if ((status & VALID_MASK) != PROCESSED) {
    // cout << "  Re-execute until\n";
    env->exec_until (st, p);
  }
  else if (p == path (right_index (st))) {
    // cout << "  Patch env\n";
    env->patch_env (changes);
  }
  else if (p != path (0)) {
    // cout << "  My execute until\n";
    my_exec_until (p);
  }
  // cout << "  Done\n";
}

extern tree the_et;

void
bridge_rep::typeset (int desired_status) {
  // FIXME: this dirty hack ensures a perfect coherence between
  // the bridge and the edit tree at the typesetting stage.
  // This should not be necessary, but we use because the ip_observers
  // may become wrong otherwise.
  if (is_accessible (ip))
    st= subtree (the_et, reverse (ip));
  if (!is_accessible (ip)) {
    path ip2= obtain_ip (st);
    if (ip2 != path (DETACHED))
      ip= ip2;
  }

  //cout << "Typesetting " << st << ", " << desired_status << LF << INDENT;
  if ((status==desired_status) && (N(ttt->old_patch)==0)) {
    // cout << "  cached\n";
    env->monitored_patch_env (changes);
    // cout << "  changes       = " << changes << "\n";
  }
  else {
    // cout << "Typesetting " << st << ", " << desired_status << "\n";
    // cout << "  recomputing\n";
    hashmap<string,tree> prev_back (UNINIT);
    my_clean_links ();
    link_repository old_link_env= env->link_env;
    env->link_env= link_env;
    ttt->local_start (l, sb);
    env->local_start (prev_back);
    my_typeset (desired_status);
    env->local_update (ttt->old_patch, changes);
    env->local_end (prev_back);
    ttt->local_end (l, sb);
    env->link_env= old_link_env;
    status= desired_status;
    // cout << "  old_patch     = " << ttt->old_patch << "\n";
    // cout << "  changes       = " << changes << "\n";
    // cout << "Typesetted " << st << ", " << desired_status << "\n";
  }
  // cout << "Typesetted " << st << ", " << desired_status << "\n";

  // ttt->insert_stack (l, sb);
  //if (N(l) == 0); else
  if (ttt->paper || (N(l) <= 1)) ttt->insert_stack (l, sb);
  else {
    bool flag= false;
    int i, n= N(l);
    for (i=0; i<n; i++)
      flag= flag || (N (l[i]->fl) != 0) || (l[i]->nr_cols > 1);
    if (flag) ttt->insert_stack (l, sb);
    else {
      int first=-1, last=-1;
      array<box> bs;
      array<SI>  spc;
      for (i=0; i<n; i++)
	if (l[i]->type != PAGE_CONTROL_ITEM) {
	  if (first == -1 && l[i]->type == PAGE_LINE_ITEM) first= N(bs);
	  bs  << l[i]->b;
	  spc << l[i]->spc->def;
	  last= i;
	}
      box lb= stack_box (path (ip), bs, spc);
      if (first != -1) lb= move_box (path (ip), lb, 0, bs[first]->y2);
      array<page_item> new_l (1);
      new_l[0]= page_item (lb);
      new_l[0]->spc= l[last]->spc;
      ttt->insert_stack (new_l, sb);
    }
  }

  //cout << UNINDENT;
  //cout << "l   = " << l << LF;
  //cout << "sb  = " << sb << LF;
  //cout << "l   = " << ttt->l << LF;
  //cout << "a   = " << ttt->a << LF;
  //cout << "b   = " << ttt->b << LF;
}
