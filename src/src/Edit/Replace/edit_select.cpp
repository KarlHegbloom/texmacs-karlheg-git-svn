
/******************************************************************************
* MODULE     : edit_select.cpp
* DESCRIPTION: Selection handling
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "Replace/edit_select.hpp"
#include "Interface/edit_interface.hpp"
#include "convert.hpp"

/******************************************************************************
* Internationalization
******************************************************************************/

string
selection_encode (string lan, string s) {
  if ((lan == "czech") || (lan == "hungarian") ||
      (lan == "polish") || (lan == "slovene"))
    return cork_to_il2 (s);
  else if ((lan == "bulgarian") || (lan == "russian"))
    return koi8_to_iso (s);
  else if (lan == "ukrainian")
    return koi8uk_to_iso (s);
  else if (lan == "spanish")
    return spanish_to_ispanish (s);
  else if (lan == "german")
    return german_to_igerman (s);
  else return s;
}

string
selection_decode (string lan, string s) {
  if ((lan == "czech") || (lan == "hungarian") ||
      (lan == "polish") || (lan == "slovene"))
    return il2_to_cork (s);
  else if ((lan == "bulgarian") || (lan == "russian"))
    return iso_to_koi8 (s);
  else if (lan == "ukrainian")
    return iso_to_koi8uk (s);
  else if (lan == "spanish")
    return ispanish_to_spanish (s);
  else if (lan == "german")
    return igerman_to_german (s);
  else return s;
}

/******************************************************************************
* Constructor and destructor
******************************************************************************/

edit_select_rep::edit_select_rep ():
  selecting (false), shift_selecting (false), mid_p (),
  selection_import ("texmacs"), selection_export ("texmacs") {}
edit_select_rep::~edit_select_rep () {}
void edit_select_rep::get_selection (path& start, path& end) {
  start= copy (start_p); end= copy (end_p); }
void edit_select_rep::set_selection (path start, path end) {
  start_p= copy (start); end_p= copy (end); }

/******************************************************************************
* Selecting particular things
******************************************************************************/

void
edit_select_rep::select (path p) {
  select (start (et, p), end (et, p));
}

void
edit_select_rep::select (path p1, path p2) {
  start_p= p1;
  end_p  = p2;
  if (path_less_eq (end_p, start_p)) {
    start_p= p2;
    end_p  = p1;
  }
  notify_change (THE_SELECTION);
}

void
edit_select_rep::select_all () {
  select (rp);
}

void
edit_select_rep::select_line () {
  select (search_parent_upwards (DOCUMENT));
}

/******************************************************************************
* For interface with cursor movement
******************************************************************************/

void
edit_select_rep::select_from_cursor () {
  if (!(selecting || shift_selecting)) return;
  if (path_less (mid_p, tp)) {
    start_p= copy (mid_p);
    end_p  = copy (tp);
  }
  else {
    start_p= copy (tp);
    end_p  = copy (mid_p);
  }
  notify_change (THE_SELECTION);
}

void
edit_select_rep::select_from_cursor_if_active () {
  if (selecting) select_from_cursor ();
}

void
edit_select_rep::select_from_keyboard (bool flag) {
  selecting= flag;
  shift_selecting= false;
  if (flag) mid_p= copy (tp);
  else mid_p= rp;
}

void
edit_select_rep::select_from_shift_keyboard () {
  if ((!shift_selecting) || (end_p == start_p) ||
      ((tp != start_p) && (tp != end_p)))
    {
      selecting= false;
      shift_selecting= true;
      mid_p= copy (tp);
    }
}

/******************************************************************************
* Enlarging an existing selection
******************************************************************************/

void
edit_select_rep::select_enlarge () {
  if (start_p == end_p) {
    path p = path_up (start_p);
    tree st= subtree (et, p);
    if (is_atomic (st)) {
      string s= st->label;
      string mode= get_env_string (MODE);
      if (mode == "text" || mode == "src") {
	int i1= last_item (start_p), i2= i1;
	while (i1>0) {
	  if (s[i1-1] == ' ' || is_punctuation (s[i1-1])) break;
	  tm_char_backwards (s, i1);
	}
	while (i2<N(s)) {
	  if (s[i2] == ' ' || is_punctuation (s[i2])) break;
	  tm_char_forwards (s, i2);
	}
	if (i1<i2) select (p * i1, p * i2);
	else {
	  if (s == "") { p= path_up (p); select (p * 0, p * 1); }
	  else select (p * 0, p * N(s));
	}
      }
      else {
	path q= path_up (p);
	if (is_concat (subtree (et, q)) || (s == ""))
	  select (q * 0, q * 1);
	else select (p * 0, p * N(s));
      }
    }
    else select (p * 0, p * 1);
  }
  else {
    path p= common (start_p, end_p);
    if (!(rp < p)) {
      selection_cancel ();
      set_message ("", "");
      return;
    }
    tree st= subtree (et, p);
    path q = path_up (p);
    int  i1= last_item (start_p);
    int  i2= last_item (end_p);
    if (is_atomic (st) && (!is_concat (subtree (et, q))) &&
	((i1 != 0) || (i2 != N(st->label))))
      select (p * 0, p * N(st->label));
    else select (q * 0, q * 1);
  }

  path p = common (start_p, end_p);
  tree st= subtree (et, p);
  if (is_func (st, TFORMAT) || is_func (st, DOCUMENT, 1) ||
      drd->var_without_border (L(st)))
    select_enlarge ();
  else {
    string s;
    if (is_atomic (st)) s= "text";
    else if (is_func (st, COMPOUND)) s= as_string (st[0]);
    else if (is_func (st, WITH)) s= "with#" * as_string (st[0]);
    else s= as_string (L(st));
    set_message ("selected#" * s, "enlarge selection");
  }
  selecting= shift_selecting= false;
}

static bool
stop_enlarge_environmental (tree t) {
  if (is_func (t, WITH, 3) && (t[0] == MODE) && (t[1] == "math")) return true;
  if (!is_extension (t)) return false;
  if (is_multi_paragraph (t)) return true;
  string s= as_string (L(t));
  return
    (s == "part") ||
    (s == "chapter") ||
    (s == "section") ||
    (s == "subsection") ||
    (s == "subsubsection") ||
    (s == "paragraph") ||
    (s == "subparagraph");
}

void
edit_select_rep::select_enlarge_environmental () {
  select_enlarge ();
  if (end_p == start_p) return;
  path p= common (start_p, end_p);
  tree st= subtree (et, p);
  if (stop_enlarge_environmental (st)) return;
  select_enlarge_environmental ();
}

/******************************************************************************
* Test whether selection is active
******************************************************************************/

bool
edit_select_rep::selection_active_any () {
  return end_p != start_p;
  // return made_selection;
}

bool
edit_select_rep::selection_active_normal () {
  return selection_active_any () && (!selection_active_table ());
}

bool
edit_select_rep::selection_active_table () {
  if (!selection_active_any ()) return false;
  path p= common (start_p, end_p);
  if ((p == start_p) || (p == end_p)) p= path_up (p);
  tree t= subtree (et, p);
  return
    is_func (t, TFORMAT) || is_func (t, TABLE) ||
    is_func (t, ROW) || is_func (t, CELL);
}

bool
edit_select_rep::selection_active_small () {
  if (!selection_active_normal ()) return false;
  path p1, p2;
  selection_get (p1, p2);
  if (p2 == p1) return false;
  if (is_multi_paragraph (subtree (et, common (p1, p2)))) return false;
  return true;
}

bool
edit_select_rep::selection_active_enlarging () {
  return (selecting || (end_p != start_p)) && (mid_p == tp);
}

/******************************************************************************
* Subroutines for table selections
******************************************************************************/

static path
table_search_format (tree t, path p) {
  tree st= subtree (t, p);
  if (is_func (st, TFORMAT) && is_func (st[N(st)-1], TABLE)) return p;
  while ((!nil (p)) && (!is_func (subtree (t, p), TABLE))) p= path_up (p);
  if ((!nil (p)) && (is_func (subtree (t, path_up (p)), TFORMAT)))
    p= path_up (p);
  return p;
}

static void
table_search_coordinates (tree t, path p, int& row, int& col) {
  row= col= 0;
  while (true) {
    if (nil (p)) p= path (1);
    if (p == path (0)) p= path (0, 0);
    if (p == path (1)) p= path (N(t)-1, 1);
    if (is_func (t, TFORMAT));
    else if (is_func (t, TABLE)) row= p->item;
    else if (is_func (t, ROW)) col= p->item;
    else return;
    t= t [p->item];
    p= p->next;
  }
}

static path
table_search_cell (tree t, int row, int col) {
  path p;
  while (is_func (t, TFORMAT)) {
    p= p * (N(t)-1);
    t= t [N(t)-1];
  }
  p= p * row;
  t= t [row];
  while (is_func (t, TFORMAT)) {
    p= p * (N(t)-1);
    t= t [N(t)-1];
  }
  p= p * col;
  t= t [col];
  while (is_func (t, TFORMAT)) {
    p= p * (N(t)-1);
    t= t [N(t)-1];
  }
  return p;
}

/******************************************************************************
* Get the selection
******************************************************************************/

void
selection_correct (tree t, path i1, path i2, path& o1, path& o2) {
  if (i1 == i2) {
    o1= i1;
    o2= i2;
  }
  else if (atom (i1) || atom (i2)) {
    if (is_atomic (t)) {
      o1= i1;
      o2= i2;
    }
    else {
      o1= start (t);
      o2= end (t);
    }
  }
  else if (i1->item == i2->item) {
    selection_correct (t[i1->item], i1->next, i2->next, o1, o2);
    o1= path (i1->item, o1);
    o2= path (i2->item, o2);
  }
  else {
    tree_label l= L(t);
    if ((l==DOCUMENT) || (l==PARA) || (l==CONCAT)) {
      if (is_compound (t[i1->item])) {
	path mid;
	selection_correct (t[i1->item], i1->next, end (t[i1->item]), o1, mid);
	o1= path (i1->item, o1);
      }
      else o1= i1;
      if (is_compound (t[i2->item])) {
	path mid;
	selection_correct (t[i2->item], start(t[i2->item]), i2->next, mid, o2);
	o2= path (i2->item, o2);
      }
      else o2= i2;
    }
    else {
      o1= start (t);
      o2= end (t);
    }
  }
}

static void
selection_bcorrect (drd_info drd, tree t, path i1, path i2, path& o1, path& o2)
{
  o1= i1; o2= i2;
  if (is_compound (t) && !atom (i1) && !atom (i2) && i1->item == i2->item) {
    path O1, O2;
    selection_bcorrect (drd, t[i1->item], i1->next, i2->next, O1, O2);
    if (drd->var_without_border (L(t[i1->item])) && (O1->item != O2->item)) {
      o1= path (0);
      o2= path (1);
    }
    else {
      o1= path (i1->item, O1);
      o2= path (i1->item, O2);
    }
  }
}

tree
compute_selection (tree t, path start, path end) {
  int  i1= start->item;
  int  i2= end->item;
  path p1= start->next;
  path p2= end->next;

  if (nil (p1) || nil (p2)) {
    if (start == path (right_index (t))) return "";
    if (end == path (0)) return "";
    if (start == end) return "";
    if (nil (p1) && nil (p2)) {
      if (is_compound (t)) return copy (t);
      if (i1>=i2) return "";
      return t->label (i1, i2);
    }
    if (is_compound (t) && (!is_format (t))) return copy (t);
    if (nil (p1)) {
      i1= 0;
      p1= (start->item==0? 0: right_index (t[i1]));
    }
    if (nil (p2)) {
      i2= N(t)-1;
      p2= (end->item==0? 0: right_index (t[i2]));
    }
  }

  if (i1==i2) return compute_selection (t[i1], p1, p2);
  if (is_compound (t) && (!is_format (t))) return copy (t);

  int i;
  tree r (t, i2-i1+1);
  r[0]     = compute_selection (t[i1], p1, path (right_index (t[i1])));
  r[N(r)-1]= compute_selection (t[i2], path (0), p2);
  for (i=1; i<N(r)-1; i++) r[i]= copy (t[i+i1]);
  return r;
}

path
edit_select_rep::selection_get_subtable (
  int& row1, int& col1, int& row2, int& col2)
{
  path fp= ::table_search_format (et, common (start_p, end_p));
  tree st= subtree (et, fp);
  table_search_coordinates (st, tail (start_p, N(fp)), row1, col1);
  table_search_coordinates (st, tail (end_p, N(fp)), row2, col2);
  if (row1>row2) { int tmp= row1; row1= row2; row2= tmp; }
  if (col1>col2) { int tmp= col1; col1= col2; col2= tmp; }
  table_bound (fp, row1, col1, row2, col2);
  return fp;
}

void
edit_select_rep::selection_get (selection& sel) {
  if (selection_active_table ()) {
    int row1, col1, row2, col2;
    path fp= selection_get_subtable (row1, col1, row2, col2);
    tree st= subtree (et, fp);

    int i, j;
    rectangle r (0, 0, 0, 0);
    for (i=row1; i<=row2; i++)
      for (j=col1; j<=col2; j++) {
	path cp= fp * ::table_search_cell (st, i, j);
	sel= eb->find_check_selection (cp * 0, cp * 1);
	if (sel->valid) {
	  rectangles rs= sel->rs;
	  if (r != rectangle (0, 0, 0, 0)) rs= rectangles (r, rs);
	  r= least_upper_bound (rs);
	}
      }
    sel= selection (rectangles (r), fp * 0, fp * 1);
  }
  else {
    path aux_start, aux_end, p_start, p_end;
    selection_bcorrect (drd, et, start_p, end_p, aux_start, aux_end);
    selection_correct (et, aux_start, aux_end, p_start, p_end);
    sel= eb->find_check_selection (p_start, p_end);
  }
}

void
edit_select_rep::selection_get (path& start, path& end) {
  selection sel; selection_get (sel);
  start= sel->start;
  end  = sel->end;
}

path
edit_select_rep::selection_get_start () {
  return start_p;
}

path
edit_select_rep::selection_get_end () {
  return end_p;
}

tree
edit_select_rep::selection_get () {
  if (!selection_active_any ()) return "";
  if (selection_active_table ()) {
    int row1, col1, row2, col2;
    path fp= selection_get_subtable (row1, col1, row2, col2);
    return table_get_subtable (fp, row1, col1, row2, col2);
  }
  else {
    path start, end;
    // cout << "Selecting...\n";
    selection_get (start, end);
    // cout << "Between paths: " << start << " and " << end << "\n";
    tree t= ::compute_selection (et, start, end);
    // cout << "Selection : " << t << "\n";
    return simplify_correct (t);
  }
}

path
edit_select_rep::selection_get_path () {
  path start, end;
  selection_get (start, end);
  return common (start, end);
}

/******************************************************************************
* Copy and paste
******************************************************************************/

void
edit_select_rep::selection_raw_set (string key, tree t) {
  (void) ::set_selection (key, t, "");
}

tree
edit_select_rep::selection_raw_get (string key) {
  tree t; string s;
  (void) ::get_selection (key, t, s);
  return t;
}

void
edit_select_rep::selection_set_start (path p) {
  bool flag= selection_active_any ();
  if (rp < p) start_p= p;
  else start_p= tp;
  if (path_less_eq (end_p, start_p) || (!flag)) end_p= start_p;
  notify_change (THE_SELECTION);
}

void
edit_select_rep::selection_set_end (path p) {
  if (rp < p) end_p= p;
  else end_p= tp;
  if (path_less_eq (end_p, start_p)) start_p= end_p;
  notify_change (THE_SELECTION);
}

void
edit_select_rep::selection_set (string key, tree t, bool persistant) {
  selecting= shift_selecting= false;
  string mode= get_env_string (MODE);
  string lan = get_env_string (MODE_LANGUAGE (mode));
  tree sel= tuple ("texmacs", t, mode, lan);
/*TODO: add mode="graphics" somewhere in the context of the <graphics>
    tag. To be done when implementing the different embeddings for
    nicely copying graphics into text, text into graphics, etc.
 */
  string s;
  if (key == "primary") {
    if (selection_export == "verbatim") t= exec_texmacs (t, tp);
    if (selection_export == "html") t= exec_html (t, tp);
    if (selection_export == "latex") t= exec_latex (t, tp);
    if ((selection_export == "latex") && (mode == "math"))
      t= tree (WITH, "mode", "math", t);
    s= tree_to_generic (t, selection_export * "-snippet");
    s= selection_encode (lan, s);
  }
  if (::set_selection (key, sel, s) && !persistant)
    selection_cancel ();
}

void
edit_select_rep::selection_set (tree t) {
  selection_set ("primary", t);
}

void
edit_select_rep::selection_copy (string key) {
  if (inside_active_graphics ()) {
    tree t= as_tree (eval ("(graphics-copy)"));
    selection_set (key, t);
    return;
  }
  if (selection_active_any ()) {
    path old_tp= tp;
    selection sel; selection_get (sel);
    go_to (sel->end);
    tree t= selection_get ();
    go_to (sel->start);
    selection_set (key, t);
    go_to (old_tp);
  }
}

void
edit_select_rep::selection_paste (string key) {
  tree t; string s;
  (void) ::get_selection (key, t, s);
  if (inside_active_graphics ()) {
    if (is_tuple (t, "texmacs", 3))
      call ("graphics-paste", t[1]);
    return;
  }
  if (is_tuple (t, "extern", 1)) {
    string mode= get_env_string (MODE);
    string lan = get_env_string (MODE_LANGUAGE (mode));
    if ((selection_import == "latex") && (mode == "prog")) mode= "verbatim";
    if ((selection_import == "latex") && (mode == "math")) mode= "latex-math";
    if ((selection_import == "html") && (mode == "prog")) mode= "verbatim";
    string fm= selection_import * "-snippet";
    tree doc= generic_to_tree (selection_decode(lan, as_string(t[1])), fm);
    if (is_func (doc, DOCUMENT, 1)) doc= doc[0]; // temporary fix
    insert_tree (doc);
  }
  if (is_tuple (t, "texmacs", 3)) {
    string mode= get_env_string (MODE);
    string lan = get_env_string (MODE_LANGUAGE (mode));
    if ((mode == "prog") && (t[2] == "math")) {
      tree in= tuple (lan, t[1]);
      tree r= stree_to_tree (call ("plugin-math-input", tree_to_stree (in)));
      insert_tree (r);
    }
    else {
      if ((t[2] != mode) && (t[2] != "src") && (mode != "src") &&
	  ((t[2] == "math") || (mode == "math")))
	insert_tree (tree (WITH, copy (MODE), copy (t[2]), ""), path (2, 0));
      if (is_func (t[1], TFORMAT) || is_func (t[1], TABLE)) {
	int row, col;
	path fp= search_format (row, col);
	if (nil (fp)) insert_tree (compound (copy (TABULAR), t[1]));
	else table_write_subtable (fp, row, col, t[1]);
      }
      else insert_tree (t[1]);
    }
  }
}

void
edit_select_rep::selection_clear (string key) {
  ::clear_selection (key);
}

void
edit_select_rep::selection_cancel () {
  selecting= shift_selecting= false;
  if (end_p == start_p) return;
  end_p= start_p;
  notify_change (THE_SELECTION);
}

void
edit_select_rep::selection_set_import (string fm) {
  selection_import= fm;
}

void
edit_select_rep::selection_set_export (string fm) {
  selection_export= fm;
}

string
edit_select_rep::selection_get_import () {
  return selection_import;
}

string
edit_select_rep::selection_get_export () {
  return selection_export;
}

/******************************************************************************
* Cutting the selection
******************************************************************************/

void
edit_select_rep::cut (path p) {
  cut (start (et, p), end (et, p));
}

void
edit_select_rep::cut (path p1, path p2) {
  path p = common (p1, p2);
  tree st= subtree (et, p);
  raw_cut (p1, p2);
  if (!is_func (st, TFORMAT) &&
      !is_func (st, TABLE) &&
      !is_func (st, ROW) &&
      !is_document (subtree (et, p)) &&
      is_concat (subtree (et, path_up (p))))
    correct_concat (path_up (p));
}

void
edit_select_rep::raw_cut (path p1, path p2) {
  if (p2 == p1) return;
  path p = common (p1, p2);
  tree t = subtree (et, p);
  int  n = N(p);
  int  i1= p1[n];
  int  i2= p2[n];

  if (is_document (t) || is_concat (t)) {
    path q1= copy (p); q1 << path (i1, end (t[i1]));
    path q2= copy (p); q2 << path (i2, start (t[i2]));
    raw_cut (q2, p2);
    if (i2>i1+1) remove (p * (i1+1), i2-i1-1);
    raw_cut (p1, q1);
    if (is_concat (t)) correct_concat (p);
    else remove_return (p * i1);
    return;
  }

  if (is_func (t, TFORMAT) || is_func (t, TABLE) || is_func (t, ROW)) {
    path fp= ::table_search_format (et, p);
    tree st= subtree (et, fp);
    int row1, col1, row2, col2;
    table_search_coordinates (st, tail (p1, N(fp)), row1, col1);
    table_search_coordinates (st, tail (p2, N(fp)), row2, col2);
    if (row1>row2) { int tmp= row1; row1= row2; row2= tmp; }
    if (col1>col2) { int tmp= col1; col1= col2; col2= tmp; }

    int i, j;
    for (i=row1; i<=row2; i++)
      for (j=col1; j<=col2; j++) {
        path cp= fp * ::table_search_cell (st, i, j);
        if (is_func (subtree (et, cp), CELL, 1)) cp= cp * 0;
        assign (cp, "");
      }
    path cp= fp * ::table_search_cell (st, row1, col1);
    go_to (cp * path (0, 0));

    if (is_func (st, TFORMAT))
      table_del_format (fp, row1+1, col1+1, row2+1, col2+1, "");
    return;
  }

  if (is_compound (t) && (!is_format (t))) {
    assign (p, "");
    return;
  }

  if ((N(p1) != (N(p)+1)) || (N(p2) != (N(p)+1))) {
    cerr << "t = " << t << "\n";
    cerr << "p = " << p << "\n";
    cerr << "p1= " << p1 << "\n";
    cerr << "p2= " << p2 << "\n";
    fatal_error ("invalid cut", "edit_select_rep::raw_cut");
  }

  if (is_atomic (t)) {
    int pos= last_item (p1);
    int nr = last_item (p2)-pos;
    if (nr>0) remove (p1, nr);
  }
  else {
    if ((last_item (p1) != 0) || (last_item (p2) != 1)) {
      cerr << "t = " << t << "\n";
      cerr << "p = " << p << "\n";
      cerr << "p1= " << p1 << "\n";
      cerr << "p2= " << p2 << "\n";
      fatal_error ("invalid object cut", "edit_select_rep::raw_cut");
    }
    assign (p, "");
  }
}

void
edit_select_rep::selection_cut (string key) {
  if (inside_active_graphics ()) {
    tree t= as_tree (eval ("(graphics-cut)"));
    selection_set (key, t);
    return;
  }
  if (!selection_active_any ()) return;
  if (selection_active_table ()) {
    path p1= start_p, p2= end_p;
    tree sel= selection_get ();
    selection_set (key, sel);
    cut (p1, p2);
  }
  else {
    path p1, p2;
    selection_get (p1, p2);
    go_to (p2);
    if (p2 == p1) return;

    tree sel= compute_selection (et, p1, p2);
    // cout << "Selection " << sel << "\n";
    selection_set (key, simplify_correct (sel));
    // cout << "Selected  " << sel << "\n";
    cut (p1, p2);
  }
}

tree
edit_select_rep::selection_get_cut () {
  tree t= selection_get ();
  selection_cut ();
  return t;
}

void
edit_select_rep::selection_move () {
  observer pos= position_new (tp);
  tree t= selection_get_cut ();
  go_to (position_get (pos));
  insert_tree (t);
  position_delete (pos);
}
