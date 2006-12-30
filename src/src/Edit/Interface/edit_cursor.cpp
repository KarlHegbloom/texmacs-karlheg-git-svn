
/******************************************************************************
* MODULE     : cursor.cpp
* DESCRIPTION: cursor handling
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "edit_cursor.hpp"
#include "iterator.hpp"
#include "tm_buffer.hpp"
#include "tree_traverse.hpp"
#include "drd_mode.hpp"
#include "analyze.hpp"

/******************************************************************************
* Constructor and destructor
******************************************************************************/

edit_cursor_rep::edit_cursor_rep ():
  cu (0, 0), mv (0, 0), mv_status (0) {}
edit_cursor_rep::~edit_cursor_rep () {}
cursor& edit_cursor_rep::the_cursor () { return cu; }
cursor& edit_cursor_rep::the_ghost_cursor () { return mv; }

/******************************************************************************
* Cursor movement
******************************************************************************/

#define DELTA (1<<23)

static bool searching_forwards;

path
edit_cursor_rep::make_cursor_accessible (path p, bool forwards) {
  path start_p= p;
  bool inverse= false;
  if (get_init_string (MODE) == "src")
    set_access_mode (DRD_ACCESS_SOURCE);
  else set_access_mode (DRD_ACCESS_NORMAL);
  while (!is_accessible_cursor (et, p) && !in_source ()) {
    path pp;
    if (forwards ^ inverse)
      pp= rp * next_valid (subtree (et, rp), p - rp);
    else
      pp= rp * previous_valid (subtree (et, rp), p - rp);
    if (pp == p) {
      if (inverse) break;
      else { p= start_p; inverse= true; }
    }
    else p= pp;
  }
  return p;
}

path
edit_cursor_rep::tree_path (SI x, SI y, SI delta) {
  path p= correct_cursor (et, eb->find_tree_path (x, y, delta));
  return make_cursor_accessible (p, searching_forwards);
}

bool
edit_cursor_rep::cursor_move_sub (SI& x0, SI& y0, SI& d0, SI dx, SI dy) {
  searching_forwards= dx == 1 || dy == -1;
  int i,d;
  path ref_p= tree_path (x0, y0, d0);
  if (ref_p != tp) {
    tp= ref_p;
    return true;
  }
  
  // cout << "ref_p = " << ref_p << "\n";
  if (ref_p == tree_path (x0, y0, d0+ dx*DELTA)) {
    for (i=1; i<DELTA; i=i<<1)
      if (ref_p != tree_path (x0+ dx*i, y0+ dy*i, d0+ dx*DELTA)) break;
    if (i>=DELTA) return false;
    for (d=i>>2; d>=1; d=d>>1)
      if (ref_p != tree_path (x0+ dx*(i-d), y0+ dy*(i-d), d0+ dx*DELTA)) i-=d;

    x0 += dx*i;
    y0 += dy*i;
  }
  
  // cout << "path  = " << tree_path (x0, y0, d0) << "\n";
  if (dx!=0) {
    if (ref_p == tree_path (x0, y0, d0)) {
      for (i=1; i<DELTA; i=i<<1)
	if (ref_p != tree_path (x0, y0, d0+ dx*i)) break;
      if (i>=DELTA)
	fatal_error ("inconsistent cursor handling",
		     "edit_cursor_rep::cursor_move_sub");
      for (d=i>>2; d>=1; d=d>>1)
	if (ref_p != tree_path (x0, y0, d0+ dx*(i-d))) i-=d;
      d0 += dx*i;
    }
    else {
      for (i=1; i<DELTA; i=i<<1)
	if (ref_p == tree_path (x0, y0, d0- dx*i)) break;
      if (i<DELTA) {
	for (d=i>>2; d>=1; d=d>>1)
	  if (ref_p == tree_path (x0, y0, d0- dx*(i-d))) i-=d;
	i--;
	d0 -= dx*i;
      }
      else {  // exceptional case
	ref_p= tree_path (x0, y0, d0- dx*DELTA);
	for (i=1; i<DELTA; i=i<<1)
	  if (ref_p == tree_path (x0, y0, d0- dx*i)) break;
	for (d=i>>2; d>=1; d=d>>1)
	  if (ref_p == tree_path (x0, y0, d0- dx*(i-d))) i-=d;
	d0 -= dx*i;
      }
    }
  }

  tp= tree_path (x0, y0, d0);
  return true;
}

void
edit_cursor_rep::cursor_move (SI dx, SI dy) {
  cursor_move_sub (mv->ox, mv->oy, mv->delta, dx, dy);
}

/******************************************************************************
* Routines affecting both the cursor and the ghost cursor
******************************************************************************/

void
edit_cursor_rep::adjust_ghost_cursor (int status) {
  if (status==mv_status) {
    if (status!=HORIZONTAL) {
      mv->ox   = cu->ox;
      mv->delta= cu->delta;
    }
    if (status!=VERTICAL)
      mv->oy= cu->oy;
  }
}

void
edit_cursor_rep::notify_cursor_moved (int status) {
  mv_status= status;
  cu= eb->find_check_cursor (tp);
  notify_change (THE_CURSOR);
}

void
edit_cursor_rep::go_to (SI x, SI y) {
  if (has_changed (THE_TREE+THE_ENVIRONMENT)) return;
  tp= tree_path (x, y, 0);
  notify_cursor_moved (CENTER);
  mv->ox   = x;
  mv->oy   = y;
  mv->delta= 0;
}

void
edit_cursor_rep::go_left_physical () {
  if (has_changed (THE_TREE+THE_ENVIRONMENT)) return;
  adjust_ghost_cursor (VERTICAL);
  cursor_move (-1, 0);
  notify_cursor_moved (HORIZONTAL);
  select_from_cursor_if_active ();
}

void
edit_cursor_rep::go_right_physical () {
  if (has_changed (THE_TREE+THE_ENVIRONMENT)) return;
  adjust_ghost_cursor (VERTICAL);
  cursor_move (1, 0);
  notify_cursor_moved (HORIZONTAL);
  select_from_cursor_if_active ();
}

void
edit_cursor_rep::go_up () {
  if (has_changed (THE_TREE+THE_ENVIRONMENT)) return;
  adjust_ghost_cursor (HORIZONTAL);
  cursor_move (0, 1);
  notify_cursor_moved (VERTICAL);
  select_from_cursor_if_active ();
}

void
edit_cursor_rep::go_down () {
  if (has_changed (THE_TREE+THE_ENVIRONMENT)) return;
  adjust_ghost_cursor (HORIZONTAL);
  cursor_move (0, -1);
  notify_cursor_moved (VERTICAL);
  select_from_cursor_if_active ();
}

void
edit_cursor_rep::go_page_up () {
  if (has_changed (THE_TREE+THE_ENVIRONMENT)) return;
  if (mv->oy >= eb->y2) return;
  go_to (mv->ox, mv->oy+ get_window_height ());
  select_from_cursor_if_active ();
}

void
edit_cursor_rep::go_page_down () {
  if (has_changed (THE_TREE+THE_ENVIRONMENT)) return;
  if (mv->oy < eb->y1) return;
  go_to (mv->ox, mv->oy- get_window_height ());
  select_from_cursor_if_active ();
}

/******************************************************************************
* Adapt physical horizontal cursor movement to line breaking
******************************************************************************/

void
edit_cursor_rep::go_left () {
  if (has_changed (THE_TREE+THE_ENVIRONMENT)) return;
  path old_tp= copy (tp);
  go_left_physical ();
  if (tp != old_tp && inside_same (et, old_tp, tp, DOCUMENT)) return;
  path p= previous_valid (et, old_tp);
  if (rp < p) go_to (p);
  select_from_cursor_if_active ();
}

void
edit_cursor_rep::go_right () {
  if (has_changed (THE_TREE+THE_ENVIRONMENT)) return;
  path old_tp= copy (tp);
  go_right_physical ();
  if (tp != old_tp && inside_same (et, old_tp, tp, DOCUMENT)) return;
  path p= next_valid (et, old_tp);
  if (rp < p) go_to (p);
  select_from_cursor_if_active ();
}

void
edit_cursor_rep::go_start_line () {
  if (has_changed (THE_TREE+THE_ENVIRONMENT)) return;
  while (true) {
    cursor old_cu= copy (cu);
    cursor old_mv= copy (mv);
    path   old_tp= copy (tp);
    go_left_physical ();
    if (tp == old_tp || !more_inside (et, tp, old_tp, DOCUMENT)) {
      notify_cursor_moved (HORIZONTAL);
      cu= old_cu;
      mv= old_mv;
      tp= old_tp;
      select_from_cursor_if_active ();
      return;
    }
  }
}

void
edit_cursor_rep::go_end_line () {
  if (has_changed (THE_TREE+THE_ENVIRONMENT)) return;
  while (true) {
    cursor old_cu= copy (cu);
    cursor old_mv= copy (mv);
    path   old_tp= copy (tp);
    go_right_physical ();
    if (tp == old_tp || !more_inside (et, tp, old_tp, DOCUMENT)) {
      notify_cursor_moved (HORIZONTAL);
      cu= old_cu;
      mv= old_mv;
      tp= old_tp;
      select_from_cursor_if_active ();
      return;
    }
  }
}

/******************************************************************************
* Logical cursor changes
******************************************************************************/

void
edit_cursor_rep::adjust_cursor () {
  cursor mv= copy (cu);
  SI dx= PIXEL << 8, ddelta= 0;
  path p= tree_path (mv->ox, mv->oy, mv->delta);
  if (p != tp) {
    // cout << "Cursors don't match\n";
    while (dx != 0 || ddelta != 0) {
      // cout << "  " << tp << ", " << p << "\n";
      p= tree_path (mv->ox, mv->oy, mv->delta);
      int eps= (path_inf (p, tp)? 1: -1);
      if (p == tp) eps= (mv->ox < cu->ox? 1: -1);
      if (p == tp && mv->ox == cu->ox) eps= (mv->delta < cu->delta? 1: -1);
      if (dx > 0) {
	if (p != tp ||
	    tree_path (mv->ox + eps * dx, mv->oy, mv->delta) == tp)
	  mv->ox += eps * dx;
	dx >>= 1;
	if (dx == 0) ddelta= DELTA;
      }
      else if (ddelta > 0) {
	if (p != tp ||
	    tree_path (mv->ox, mv->oy, mv->delta + eps * ddelta) == tp)
	  mv->delta += eps * ddelta;
	ddelta >>= 1;
      }
    }
  }
  if (p == tp) cu= mv;
}

void
edit_cursor_rep::go_to_here () {
  cu= eb->find_check_cursor (tp);
  if (!cu->valid) {
    tp= super_correct (et, tp);
    cu= eb->find_check_cursor (tp);
  }
  if (!cu->valid) {
    tp= make_cursor_accessible (tp, false);
    cu= eb->find_check_cursor (tp);
  }
  if (cu->valid) adjust_cursor ();
  if (mv_status == DIRECT) mv= copy (cu);
  notify_change (THE_CURSOR);
}

void
edit_cursor_rep::go_to (path p) {
  if (rp <= p) {
    tp= p;
    mv_status= DIRECT;
    if (!has_changed (THE_TREE+THE_ENVIRONMENT)) {
      cu= eb->find_check_cursor (tp);
      if (cu->valid) adjust_cursor ();
      mv= copy (cu);
    }
    notify_change (THE_CURSOR);
  }
}

void
edit_cursor_rep::go_to_correct (path p) {
  p= correct_cursor (et, p);
  go_to (p);
}

void
edit_cursor_rep::go_to_start (path p) {
  go_to (start (et, p));
}

void
edit_cursor_rep::go_to_end (path p) {
  go_to (end (et, p));
}

void
edit_cursor_rep::go_to_border (path p, bool at_start) {
  if (at_start) go_to_start (p);
  else go_to_end (p);
}

void
edit_cursor_rep::go_start () {
  go_to (correct_cursor (et, rp * 0));
  select_from_cursor_if_active ();
}

void
edit_cursor_rep::go_end () {
  go_to (correct_cursor (et, rp * 1));
  select_from_cursor_if_active ();
}

void
edit_cursor_rep::go_start_paragraph () {
  path p= search_parent_upwards (DOCUMENT);
  go_to (start (et, p));
  select_from_cursor_if_active ();
}

void
edit_cursor_rep::go_end_paragraph () {
  path p= search_parent_upwards (DOCUMENT);
  go_to (end (et, p));
  select_from_cursor_if_active ();
}

void
edit_cursor_rep::go_start_of (tree_label what) {
  path p= search_upwards (what);
  if (!nil (p)) go_to (start (et, p));
}

void
edit_cursor_rep::go_end_of (tree_label what) {
  path p= search_upwards (what);
  if (!nil (p)) go_to (end (et, p));
}

void
edit_cursor_rep::go_start_with (string var, string val) {
  path p= search_upwards_with (var, val);
  if (!nil (p)) go_to (start (et, p));
}

void
edit_cursor_rep::go_end_with (string var, string val) {
  path p= search_upwards_with (var, val);
  if (!nil (p)) go_to (end (et, p));
}

/******************************************************************************
* Jumping to a label
******************************************************************************/

tree
edit_cursor_rep::get_labels () {
  tree r (TUPLE);
  hashmap<string,tree> h= buf->ref;
  if (buf->prj != NULL) {
    h= copy (buf->prj->ref);
    h->join (buf->ref);
  }
  iterator<string> it= iterate (h);
  while (it->busy ()) {
    string ref= it->next ();
    r << ref;
  }
  return r;
}

static path
search_label (tree t, string which) {
  if (is_atomic (t)) return path ();
  else if (t == tree (LABEL, which)) return path (1);
  else if (is_compound (t, "tag", 2) && t[0] == which)
    return path (1, start (t[1]));
  else {
    int i, n=N(t);
    for (i=0; i<n; i++) {
      path q= search_label (t[i], which);
      if (!nil (q)) return path (i, q);
    }
    return path ();
  }
}

void
edit_cursor_rep::show_cursor_if_hidden () {
  if (!is_accessible_cursor (et, tp) && !in_source ()) {
    eval ("(use-modules (utils edit variants))");
    eval ("(cursor-show-hidden)");
  }
}

void
edit_cursor_rep::go_to_label (string s) {
  path p= search_label (et, s);
  if (!nil (p)) {
    go_to (p);
    show_cursor_if_hidden ();
    return;
  }
  if (!nil (eb)) {
    p= eb->find_tag (s);
    if (!nil (p)) {
      go_to (p);
      show_cursor_if_hidden ();
      return;
    }
  }
  tree val= (buf->prj==NULL? buf->ref[s]: buf->prj->ref[s]);
  if (is_func (val, TUPLE, 3) && is_atomic (val[2])) {
    string extra= val[2]->label;
    if (starts (extra, "#")) {
      string part= extra (1, N (extra));
      int i= search_forwards (".", part);
      if (i >= 0) part= part (0, i);
      string show= "(show-hidden-part " * scm_quote (part) * ")";
      string jump= "(go-to-label " * scm_quote (s) * ")";
      eval_delayed ("(if " * show * " (delayed " * jump * "))");
    }
    else {
      url u= relative (buf->name, url (extra));
      if (u != buf->name) {
	string new_buf = scm_quote (as_string (u));
	string load_buf= "(load-buffer (url-system " * new_buf * "))";
	string jump_to = "(go-to-label " * scm_quote (s) * ")";
	eval_delayed ("(begin " * load_buf * " " * jump_to * ")");
      }
    }
  }
}
