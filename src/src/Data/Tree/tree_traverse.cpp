
/******************************************************************************
* MODULE     : tree_traverse.cpp
* DESCRIPTION: abstract cursor movement and tree traversal
* COPYRIGHT  : (C) 2005  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "tree_traverse.hpp"
#include "drd_std.hpp"
#include "analyze.hpp"
#include "hashset.hpp"

/******************************************************************************
* Accessability
******************************************************************************/

bool
is_accessible_child (tree t, int i) {
  return the_drd->is_accessible_child (t, i);
}

array<tree>
accessible_children (tree t) {
  array<tree> a;
  int i, n= N(t);
  for (i=0; i<n; i++)
    if (the_drd->is_accessible_child (t, i))
      a << t[i];
  return a;
}

/******************************************************************************
* Traversal of a tree
******************************************************************************/

static path
move_any (tree t, path p, bool forward) {
  path q = path_up (p);
  int  l = last_item (p);
  tree st= subtree (t, q);
  if (is_atomic (st)) {
    string s= st->label;
    ASSERT (l >= 0 && l <= N(s), "out of range");
    if (forward) {
      if (l<N(s)) {
	tm_char_forwards (s, l);
	return q * l;
      }
    }
    else {
      if (l>0) {
	tm_char_backwards (s, l);
	return q * l;
      }
    }
  }
  else if ((forward && l==0) || (!forward && l==1)) {
    int i, n= N(st);
    if (forward) {
      for (i=0; i<n; i++)
	if (the_drd->is_accessible_child (st, i))
	  return q * path (i, 0);
    }
    else {
      for (i=n-1; i>=0; i--)
	if (the_drd->is_accessible_child (st, i))
	  return q * path (i, right_index (st[i]));
    }
    return q * (1-l);
  }
  else if (is_nil (q)) return p;

  l = last_item (q);
  q = path_up (q);
  st= subtree (t, q);
  int i, n= N(st);
  if (forward) {
    for (i=l+1; i<n; i++)
      if (the_drd->is_accessible_child (st, i))
	return q * path (i, 0);
  }
  else {
    for (i=l-1; i>=0; i--)
      if (the_drd->is_accessible_child (st, i)) {
	return q * path (i, right_index (st[i]));
    }
  }
  return q * (forward? 1: 0);
}

path next_any (tree t, path p) {
  return move_any (t, p, true); }
path previous_any (tree t, path p) {
  return move_any (t, p, false); }

/******************************************************************************
* Traversal of a valid cursor positions inside a tree
******************************************************************************/

static path
move_valid (tree t, path p, bool forward) {
  ASSERT (is_inside (t, p), "invalid cursor");
  path q= p;
  while (true) {
    path r= move_any (t, q, forward);
    if (r == q) return p;
    if (valid_cursor (t, r)) return r;
    q= r;
  }
}

path next_valid (tree t, path p) {
  return move_valid (t, p, true); }
path previous_valid (tree t, path p) {
  return move_valid (t, p, false); }

/******************************************************************************
* Word based traversal of a tree
******************************************************************************/

static inline bool
is_iso_alphanum (char c) {
  return is_iso_alpha (c) || is_digit (c);
}

static bool
at_border (tree t, path p, bool forward) {
  tree st= subtree (t, path_up (p));
  int l= last_item (p), n= N(st);
  if (!is_concat (st) && !is_document (st)) return true;
  if ((forward && l!=n-1) || (!forward && l!=0)) return false;
  return at_border (t, path_up (p), forward);
}

static bool
next_is_word (tree t, path p) {
  tree st= subtree (t, path_up (p));
  int l= last_item (p), n= N(st);
  if (!is_concat (st) || l+1 >= n) return false;
  if (is_compound (st[l+1])) return true;
  return st[l+1] != "" && is_iso_alphanum (st[l+1]->label[0]);
}

static path
move_word (tree t, path p, bool forward) {
  while (true) {
    path q= move_valid (t, p, forward);
    int l= last_item (q);
    if (q == p) return p;
    tree st= subtree (t, path_up (q));
    if (is_atomic (st)) {
      string s= st->label;
      int n= N(s);
      if (s == "") return q;
      if (forward && l>0 &&
	  (is_iso_alphanum (s[l-1]) ||
	   (l==n && at_border (t, path_up (q), forward))) &&
	  (l==n || !is_iso_alphanum (s[l])))
	return q;
      if (!forward && l<n &&
	  (is_iso_alphanum (s[l]) ||
	   (l==0 && at_border (t, path_up (q), forward))) &&
	  (l==0 || !is_iso_alphanum (s[l-1])))
	return q;
      if (!forward && l==n && next_is_word (t, path_up (q)))
	return q;
    }
    else {
      if (forward && l==1) return q;
      if (!forward && l==0) return q;
      if (!forward && next_is_word (t, path_up (q)))
	return q;
    }
    p= q;
  }
}

path next_word (tree t, path p) {
  return move_word (t, p, true); }
path previous_word (tree t, path p) {
  return move_word (t, p, false); }

/******************************************************************************
* Node based traversal of a tree
******************************************************************************/

static path
move_node (tree t, path p, bool forward) {
  tree st= subtree (t, path_up (p));
  if (is_atomic (st)) {
    if (forward) p= path_up (p) * N (st->label);
    else p= path_up (p) * 0;
  }
  return move_valid (t, p, forward);
}

path next_node (tree t, path p) {
  return move_node (t, p, true); }
path previous_node (tree t, path p) {
  return move_node (t, p, false); }

/******************************************************************************
* Tag based traversal of a tree
******************************************************************************/

static bool
inside_same_argument (tree t, path p, path q, hashset<int> labs) {
  path c= common (p, q);
  path r= path_up (q);
  while (!is_nil (r) && (r != c)) {
    r= path_up (r);
    if (labs->contains ((int) L (subtree (t, r)))) return false;
  }
  return true;
}

static path
move_tag (tree t, path p, hashset<int> labs, bool forward) {
  path q= p;
  while (true) {
    path r= move_node (t, q, forward);
    if (r == q) return p;
    if (!inside_same_argument (t, p, r, labs)) return r;
    q= r;
  }
}

static hashset<int>
get_labels (scheme_tree t) {
  hashset<int> labs;
  if (is_atomic (t))
    labs->insert ((int) as_tree_label (t->label));
  else {
    int i, n= N(t);
    for (i=0; i<n; i++)
      if (is_atomic (t[i]))
	labs->insert ((int) as_tree_label (t[i]->label));
  }
  return labs;
}

path next_tag (tree t, path p, scheme_tree which) {
  return move_tag (t, p, get_labels (which), true); }
path previous_tag (tree t, path p, scheme_tree which) {
  return move_tag (t, p, get_labels (which), false); }

/******************************************************************************
* Traverse the children of a node
******************************************************************************/

static path
move_argument (tree t, path p, bool forward) {
  path q = path_up (p);
  int  l = last_item (p);
  tree st= subtree (t, q);
  int i, n= N(st);
  if (forward) {
    for (i=l+1; i<n; i++)
      if (the_drd->is_accessible_child (st, i))
	return start (t, q * i);
  }
  else {
    for (i=l-1; i>=0; i--)
      if (the_drd->is_accessible_child (st, i))
	return end (t, q * i);
  }
  return path ();
}

path next_argument (tree t, path p) {
  return move_argument (t, p, true); }
path previous_argument (tree t, path p) {
  return move_argument (t, p, false); }

/******************************************************************************
* Other routines
******************************************************************************/

static path
search_upwards (tree t, path p, tree_label which) {
  if (is_nil (p) || L (subtree (t, p)) == which) return p;
  else return search_upwards (t, path_up (p), which);
}

bool
inside_same (tree t, path p, path q, tree_label which) {
  return
    search_upwards (t, path_up (p), which) ==
    search_upwards (t, path_up (q), which);
}

bool
more_inside (tree t, path p, path q, tree_label which) {
  return
    search_upwards (t, path_up (q), which) <=
    search_upwards (t, path_up (p), which);
}
