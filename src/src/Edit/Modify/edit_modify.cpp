
/******************************************************************************
* MODULE     : edit_modify.cpp
* DESCRIPTION: base routines for modifying the edit tree + notification
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "edit_modify.hpp"
#include "tm_window.hpp"
#ifdef EXPERIMENTAL
#include "../../Style/Memorizer/clean_copy.hpp"
#endif

extern int max_undo_depth;

/******************************************************************************
* Constructors and destructors
******************************************************************************/

edit_modify_rep::edit_modify_rep ():
  undo_flag (false), redo_flag (false) {}
edit_modify_rep::~edit_modify_rep () {}

/******************************************************************************
* modification routines
******************************************************************************/

// FIXME: the following notification loop is slow when we have many
// open buffers. In the future, we might obtain the relevant editors
// from all possible prefixes of p using a hashtable

// FIXME: the undo system is not safe when a change is made inside
// a buffer which has no editor attached to it

#define FOR_ALL_EDITORS_BEGIN(p)			\
  int i, j;						\
  for (i=0; i<sv->nr_bufs(); i++) {			\
    tm_buffer b= sv->get_buf (i);			\
    if (b->rp <= p)					\
      for (j=0; j<N(b->vws); j++) {			\
	editor& ed= ((tm_view) (b->vws[j]))->ed;

#define FOR_ALL_EDITORS_END			\
      }						\
  }

#define CHECK_OTHER_EDITOR(cmd)				\
  if (!(rp <= pp)) {					\
    tm_buffer other= sv->get_buf (pp);			\
    if (N(other->vws) != 0) {				\
      ((tm_view) (other->vws[0]))->ed->cmd;		\
      return;						\
    }							\
    /*else system_warning ("Dangerous change");*/	\
  }

void
edit_modify_rep::assign (path pp, tree u) {
  CHECK_OTHER_EDITOR (assign (pp, u));
  path p= copy (pp);
  // cout << "Assign " << u << " at " << p << "\n";
  notify_undo ("assign", p, subtree (et, p));

  FOR_ALL_EDITORS_BEGIN (p)
    ed->notify_assign (p, u);
  FOR_ALL_EDITORS_END

  ::assign (subtree (et, p), u);
#ifdef EXPERIMENTAL
  global_notify_assign (p, u);
#endif
  finished (pp);
}

void
edit_modify_rep::insert (path pp, tree u) {
  CHECK_OTHER_EDITOR (insert (pp, u));
  path p= copy (pp);
  // cout << "Insert " << u << " at " << p << "\n";
  notify_undo ("remove", p, as_string (is_atomic (u)? N(u->label): N(u)));

  FOR_ALL_EDITORS_BEGIN (p)
    ed->notify_insert (p, u);
  FOR_ALL_EDITORS_END

  ::insert (subtree (et, path_up (p)), last_item (p), u);
#ifdef EXPERIMENTAL
  global_notify_insert (p, u);
#endif
  finished (pp);
}

void
edit_modify_rep::remove (path pp, int nr) {
  CHECK_OTHER_EDITOR (remove (pp, nr));
  if (nr <= 0) return;
  path p= copy (pp);
  // cout << "Remove " << nr << " at " << p << "\n";
  tree& st= subtree (et, path_up (p));
  int l= last_item (p);
  if (is_atomic (st)) notify_undo ("insert", p, st->label (l, l+ nr));
  else notify_undo ("insert", p, st (l, l+ nr));

  FOR_ALL_EDITORS_BEGIN (p)
    ed->notify_remove (p, nr);
  FOR_ALL_EDITORS_END

  ::remove (subtree (et, path_up (p)), last_item (p), nr);
#ifdef EXPERIMENTAL
  global_notify_remove (p, nr);
#endif
  finished (pp);
}

void
edit_modify_rep::split (path pp) {
  CHECK_OTHER_EDITOR (split (pp));
  path p= copy (pp);
  // cout << "Split at " << p << "\n";
  if (N(p)<2) fatal_error ("path too short in split", "editor::split");
  tree& st= subtree (et, path_up (path_up (p)));
  int  l1 = last_item (path_up (p));
  int  l2 = last_item (p);
  notify_undo ("join", path_up (p), "");

  FOR_ALL_EDITORS_BEGIN (p)
    ed->notify_split (p);
  FOR_ALL_EDITORS_END

  ::split (st, l1, l2);
#ifdef EXPERIMENTAL
  global_notify_split (p);
#endif
  finished (pp);
}

void
edit_modify_rep::join (path pp) {
  CHECK_OTHER_EDITOR (join (pp));
  path p= copy (pp);
  // cout << "Join at " << p << "\n";
  if (N(p)<1) fatal_error ("path too short in join", "editor::join");
  tree& st= subtree (et, path_up (p));
  int  l1 = last_item (p);
  // int  l2 = is_atomic (st[l1])? N (st[l1]->label): N (st[l1]);
  if (l1+1 >= arity (st)) fatal_error ("invalid join", "editor::join");
  bool string_mode= is_atomic (st[l1]) && is_atomic (st[l1+1]);
  int len= string_mode? N (st[l1]->label): arity (st[l1]);
  notify_undo ("split", p * len, "");

  FOR_ALL_EDITORS_BEGIN (p)
    ed->notify_join (p);
  FOR_ALL_EDITORS_END

  ::join (st, l1);
#ifdef EXPERIMENTAL
  global_notify_join (p);
#endif
  finished (pp);
}

void
edit_modify_rep::assign_node (path pp, tree_label op) {
  CHECK_OTHER_EDITOR (assign_node (pp, op));
  path p= copy (pp);
  // cout << "Assign node " << get_label (tree (op)) << " at " << p << "\n";
  tree& st= subtree (et, p);
  notify_undo ("assign_node", p, get_label (st));

  FOR_ALL_EDITORS_BEGIN (p)
    ed->notify_assign_node (p, op);
  FOR_ALL_EDITORS_END

  ::assign_node (subtree (et, p), op);
#ifdef EXPERIMENTAL
  global_notify_assign_node (p, op);
#endif
  finished (pp);
}

void
edit_modify_rep::insert_node (path pp, tree t) {
  CHECK_OTHER_EDITOR (insert_node (pp, t));
  path p= copy (pp);
  // cout << "Insert node " << t << " at " << p << "\n";
  notify_undo ("remove_node", p, "");

  FOR_ALL_EDITORS_BEGIN (p)
    ed->notify_insert_node (p, t);
  FOR_ALL_EDITORS_END

  ::insert_node (subtree (et, path_up (p)), last_item (p), t);
#ifdef EXPERIMENTAL
  global_notify_insert_node (p, t);
#endif
  finished (pp);
}

void
edit_modify_rep::remove_node (path pp) {
  CHECK_OTHER_EDITOR (remove_node (pp));
  path p= copy (pp);
  // cout << "Remove node at " << p << "\n";
  int pos= last_item (pp);
  tree& st= subtree (et, path_up (p));
  notify_undo ("insert_node", p, st (0, pos) * st (pos+1, N(st)));

  FOR_ALL_EDITORS_BEGIN (p)
    ed->notify_remove_node (p);
  FOR_ALL_EDITORS_END

  ::remove_node (subtree (et, path_up (p)), pos);
#ifdef EXPERIMENTAL
  global_notify_remove_node (p);
#endif
  finished (pp);
}

void
edit_modify_rep::finished (path pp) {
  // cout << "Finished at " << pp << "\n";
  FOR_ALL_EDITORS_BEGIN (pp)
    ed->post_notify (pp);
  FOR_ALL_EDITORS_END
}

/******************************************************************************
* Cursor handling after notification of changes in document
******************************************************************************/

void
edit_modify_rep::notify_assign (path p, tree u) {
  (void) u;
  if (!(rp <= p)) return;
  cur_pos= position_new (tp);
  ::notify_assign (get_typesetter (), p - rp, u);
}

void
edit_modify_rep::notify_insert (path p, tree u) {
  if (!(rp <= p)) return;
  cur_pos= position_new (tp);
  ::notify_insert (get_typesetter (), p - rp, u);
}

void
edit_modify_rep::notify_remove (path p, int nr) {
  if (!(rp <= p)) return;
  cur_pos= position_new (tp);
  ::notify_remove (get_typesetter (), p - rp, nr);
}

void
edit_modify_rep::notify_split (path p) {
  if (!(rp <= p)) return;
  cur_pos= position_new (tp);
  ::notify_split (get_typesetter (), p - rp);
}

void
edit_modify_rep::notify_join (path p) {
  if (!(rp <= p)) return;
  cur_pos= position_new (tp);
  ::notify_join (get_typesetter (), p - rp);
}

void
edit_modify_rep::notify_assign_node (path p, tree_label op) {
  if (!(rp <= p)) return;
  cur_pos= position_new (tp);
  ::notify_assign_node (get_typesetter (), p - rp, op);
}

void
edit_modify_rep::notify_insert_node (path p, tree t) {
  if (!(rp <= p)) return;
  cur_pos= position_new (tp);
  ::notify_insert_node (get_typesetter (), p - rp, t);
}

void
edit_modify_rep::notify_remove_node (path p) {
  if (!(rp <= p)) return;
  cur_pos= position_new (tp);
  ::notify_remove_node (get_typesetter (), p - rp);
}

void
edit_modify_rep::post_notify (path p) {
  // cout << "Post notify\n";
  if (!(rp <= p)) return;
  selection_cancel ();
  invalidate_mutators ();
  notify_change (THE_TREE);
  tp= position_get (cur_pos);
  position_delete (cur_pos);
  cur_pos= nil_observer;
  go_to_correct (tp);
  /*
  cout << "et= " << et << "\n";
  cout << "tp= " << tp << "\n\n";
  */
}

/******************************************************************************
* undo and redo handling
******************************************************************************/

static tree
encode (string op, path p, tree t) {
  string s= copy (op);
  while (!is_nil (p)) {
    s << ";" << as_string (p->item);
    p= p->next;
  }
  if (t == "") return s;
  else return tree (TUPLE, s, t);
}

static void
decode (tree x, string& op, path& p, tree& t) {
  string s;
  if (is_atomic (x)) {
    s= x->label;
    t= "";
  }
  else {
    s= x[0]->label;
    t= x[1];
  }

  p= path ();
  int i=N(s);
  while (i>0) {
    int end= i;
    for (; i>0; i--)
      if (s[i-1]==';') break;
    if (i==0) op= s (i, end);
    else {
      p= path (as_int (s (i, end)), p);
      i--;
    }
  }
}

void
edit_modify_rep::notify_undo (string op, path p, tree t) {
  // cout << "Undone by " << op << " " << t << " at " << p << "\n";
  tree x= encode (op, p, t);
  if (undo_flag) buf->redo= tree (BACKUP, x, buf->redo);
  else {
    if (!redo_flag) {
      if (buf->redo != "nil") {
	buf->redo_to_undo ();
	buf->mark_undo_block ();
      }
      if ((max_undo_depth > 0) && (buf->undo_depth >= (2*max_undo_depth)))
	buf->truncate_undos (max_undo_depth);
    }
    if ((op == "remove") &&
	(buf->needs_to_be_autosaved ()) &&
	(buf->undo != "nil") && (buf->undo[0] == "") &&
	(buf->undo[1] != "nil") &&
	(buf->undo[1][1] != "nil") && (buf->undo[1][1][0] == ""))
      {
	string op2; path p2; tree t2;
	decode (buf->undo[1][0], op2, p2, t2);
	if ((op2 == "remove") && (p == path_add (p2, as_int (t2)))) {
	  if (is_atomic (subtree (et, path_up (p)))) {
	    buf->unmark_undo_block ();
	    buf->undo= buf->undo [1];
	    int nr= as_int (t2)+ as_int (t);
	    x= encode (op, p2, as_string (nr));
	  }
	}
      }
    buf->undo= tree (BACKUP, x, buf->undo);
  }
  /*
  cout << "undo tree : " << buf->undo << "\n";
  cout << "redo tree : " << buf->redo << "\n";
  cout << "exdo tree : " << buf->exdo << "\n";
  cout << "undo depth: " << buf->undo_depth << "\n";
  cout << "redo depth: " << buf->redo_depth << "\n";
  cout << "last save : " << buf->last_save << "\n";
  */
}

void
edit_modify_rep::mark_undo_blocks () {
  int i;
  for (i=0; i<sv->nr_bufs(); i++) {
    tm_buffer b= sv->get_buf (i);
    b->mark_undo_block ();
  }
}

void
edit_modify_rep::remove_undo_mark () {
  if (buf->undo != "nil") {
    tree s= buf->undo, t= buf->undo[1];
    while ((t != "nil") && (t[0] != "")) {
      s= t;
      t= t[1];
    }
    if (t != "nil") {
      s[1]= t[1];
      buf->undo_depth--;
    }
  }
}

void
edit_modify_rep::add_undo_mark () {
  buf->mark_undo_block ();
}

void
edit_modify_rep::undo (bool redoable) {
  if (inside_graphics () && !as_bool (eval ("graphics-undo-enabled"))) {
    eval ("(graphics-reset-context 'undo)");
    return;
  }
  buf->unmark_undo_block ();
  if (buf->undo == "nil") {
    set_message ("No more undo information available", "undo");
    return;
  }
  buf->mark_redo_block ();
  while ((buf->undo != "nil") && (buf->undo[0] != "")) {
    tree x= buf->undo[0];
    buf->undo= buf->undo[1];
    undo_flag= true;
    buf->exdo= tree (BACKUP, copy (x), buf->exdo);
    perform_undo_redo (x);
    undo_flag= false;
  }
  if (!redoable) {
    buf->unmark_redo_block ();
    while ((buf->redo != "nil") && (buf->redo[0] != ""))
      buf->redo= buf->redo[1];
    buf->unmark_redo_block ();
  }         
  buf->unmark_undo_block ();
  if (buf->undo_depth == buf->last_save) {
    beep ();
    set_message ("Your document is back in its original state", "undo");
  }
  if (inside_graphics ())
    eval ("(graphics-reset-context 'undo)");
}

void
edit_modify_rep::forget_undo () {
  buf->unmark_undo_block ();
  while ((buf->undo != "nil") && (buf->undo[0] != ""))
    buf->undo= buf->undo[1];
  buf->unmark_undo_block ();
}

void
edit_modify_rep::unredoable_undo () {
  undo (false);
}

void
edit_modify_rep::undo () {
  undo (true);
}

void
edit_modify_rep::redo () {
  buf->unmark_redo_block ();
  if (buf->redo == "nil") {
    set_message ("No more redo information available", "redo");
    return;
  }
  buf->mark_undo_block ();
  while ((buf->redo != "nil") && (buf->redo[0] != "")) {
    tree x= buf->redo[0];
    buf->redo= buf->redo[1];
    buf->exdo= buf->exdo[1];
    redo_flag= true;
    perform_undo_redo (x);
    redo_flag= false;
  }
  buf->unmark_redo_block ();
  if (buf->undo_depth == buf->last_save) {
    beep ();
    set_message ("Your document is back in its original state", "redo");
  }
}

void
edit_modify_rep::perform_undo_redo (tree x) {
  string op; path p; tree t;
  decode (x, op, p, t);

  if (op == "assign") {
    assign (p, t);
    go_to (end (et, p));
  }
  else if (op == "insert") {
    insert (p, t);
    if (is_atomic (t)) go_to (path_add (p, N(t->label)));
    else go_to (end (et, path_add (p, N(t)-1)));
  }
  else if (op == "remove") {
    if (is_atomic (subtree (et, path_up (p)))) {
      remove (p, as_int (t));
      go_to (p);
    }
    else {
      remove (p, as_int (t));
      if (last_item (p) == 0) go_to (start (et, p));
      else go_to (end (et, path_dec (p)));
    }
  }
  else if (op == "split") {
    split (p);
    path q= path_inc (path_up (p));
    if (is_atomic (subtree (et, q))) go_to (q * 0);
    else go_to (start (et, q));
    // else go_to (start (et, q * 0));
  }
  else if (op == "join") {
    tree& st1= subtree (et, p);
    tree& st2= subtree (et, path_inc (p));
    if (is_atomic (st1) && is_atomic (st2)) {
      int last= N (st1->label);
      join (p);
      go_to (p * last);
    }
    else {
      int  last= arity (st1) - 1;
      join (p);
      if (last == -1) go_to (start (et, p));
      else go_to (end (et, p * last));
    }
  }
  else if (op == "assign_node") {
    if (p <= tp) assign_node (p, as_tree_label (t->label));
    else {
      assign_node (p, as_tree_label (t->label));
      go_to (end (et, p));
    }
  }
  else if (op == "insert_node") {
    if (p < tp) insert_node (p, t);
    else {
      insert_node (p, t);
      go_to (end (et, p));
    }
  }
  else if (op == "remove_node") {
    if (p < tp) remove_node (p);
    else if (tp == path_up (p) * 0) {
      remove_node (p);
      go_to (start (et, path_up (p)));
    }
    else {
      remove_node (p);
      go_to (end (et, path_up (p)));
    }
  }
}

/******************************************************************************
* handling multiple cursor positions
******************************************************************************/

/*
int
edit_modify_rep::position_new () {
  int i, n= N(pps);
  for (i=0; i<n; i++)
    if (!code_to_nr->contains (i))
      break;
  pps << copy (tp);
  nr_to_code << i;
  code_to_nr (i)= n;
  return i;
}

void
edit_modify_rep::position_delete (int i) {
  if (!code_to_nr->contains (i)) return;
  int j= code_to_nr (i), n= N(pps), k;
  for (k=j; k<n-1; k++) {
    int l= nr_to_code[k+1];
    pps[k]= pps[k+1];
    nr_to_code[k]= l;
    code_to_nr(l)= k;
  }
  pps->resize (n-1);
  nr_to_code->resize (n-1);
  code_to_nr->reset (i);
}

void
edit_modify_rep::position_set (int i, path p) {
  pps[code_to_nr(i)]= copy (p);
}

path
edit_modify_rep::position_get (int i) {
  return copy (pps[code_to_nr(i)]);
}
*/

observer
edit_modify_rep::position_new (path p) {
  tree st= subtree (et, path_up (p));
  int index= last_item (p);
  observer o= tree_position (st, index);
  attach_position (st, o);
  return o;
}

void
edit_modify_rep::position_delete (observer o) {
  tree st;
  int  index;
  if (o->get_position (st, index))
    detach_position (st, o);
}

void
edit_modify_rep::position_set (observer o, path p) {
  tree st= subtree (et, path_up (p));
  int index= last_item (p);
  o->set_position (st, index);
}

path
edit_modify_rep::position_get (observer o) {
  //return super_correct (et, obtain_position (o));
  return correct_cursor (et, obtain_position (o));
}
