
/******************************************************************************
* MODULE     : clean_copy.hpp
* DESCRIPTION: maintain a clean copy of the edit tree
* COPYRIGHT  : (C) 2006  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef CLEAN_COPY_H
#define CLEAN_COPY_H
#include "tree.hpp"
#include "path.hpp"

tree clean_assign (tree t, path p, tree u);
tree clean_insert (tree t, path p, tree u);
tree clean_remove (tree t, path p, int nr);
tree clean_split (tree t, path p);
tree clean_join (tree t, path p);
tree clean_assign_node (tree t, path p, tree_label op);
tree clean_insert_node (tree t, path p, tree u);
tree clean_remove_node (tree t, path p);

void global_notify_assign (path p, tree u);
void global_notify_insert (path p, tree u);
void global_notify_remove (path p, int nr);
void global_notify_split (path p);
void global_notify_join (path p);
void global_notify_assign_node (path p, tree_label op);
void global_notify_insert_node (path p, tree u);
void global_notify_remove_node (path p);

tree global_get_subtree (path p);
void global_trace_subtree (path p);

#endif // defined CLEAN_COPY_H
