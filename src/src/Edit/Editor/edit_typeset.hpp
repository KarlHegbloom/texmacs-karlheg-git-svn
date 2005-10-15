
/******************************************************************************
* MODULE     : edit_typeset.hpp
* DESCRIPTION: the typeset structure for the mathematical editor
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#ifndef EDIT_TYPESET_H
#define EDIT_TYPESET_H
#include "env.hpp"
#include "typesetter.hpp"
#include "editor.hpp"
#include "hashset.hpp"

class document_rep;
class edit_typeset_rep: virtual public editor_rep {
protected:
  tree the_style;                         // document style
  hashmap<path,hashmap<string,tree> > cur; // environment at different paths
  hashmap<string,tree> pre;               // environment after styles
  hashmap<string,tree> init;              // environment changes w.r.t. style
  hashmap<string,tree> fin ;              // environment changes w.r.t. doc
  edit_env env;                           // the environment for typesetting
  typesetter ttt;                         // the (not) yet typesetted document

protected:
  typesetter           get_typesetter ();
  tree                 get_style ();
  void                 set_style (tree t);
  hashmap<string,tree> get_init ();
  hashmap<string,tree> get_fin ();
  void                 set_init (hashmap<string,tree> init= tree ("?"));
  void                 add_init (hashmap<string,tree> init);
  void                 set_fin (hashmap<string,tree> fin);
  void                 set_base_name (url name);

public:
  edit_typeset_rep ();
  ~edit_typeset_rep ();
  void clear_local_info ();

  SI       as_length (string l);
  string   add_lengths (string l1, string l2);
  string   multiply_length (double x, string l);
  bool     is_length (string s);
  double   divide_lengths (string l1, string l2);

  void     drd_update ();
  bool     defined_at_cursor (string var_name);
  bool     defined_at_init (string var_name);
  bool     defined_in_init (string var_name);
  tree     get_env_value (string var_name, path p);
  tree     get_env_value (string var_name);
  tree     get_init_value (string var_name);
  string   get_env_string (string var_name);
  string   get_init_string (string var_name);
  int      get_env_int (string var_name);
  int      get_init_int (string var_name);
  double   get_env_double (string var_name);
  double   get_init_double (string var_name);
  language get_env_language ();

  tree     exec (tree t, hashmap<string,tree> env, bool expand_refs= true);
  tree     exec_texmacs (tree t, path p);
  tree     exec_html (tree t, path p);
  tree     exec_html (tree t);
  tree     exec_latex (tree t, path p);
  tree     exec_latex (tree t);
  tree     texmacs_exec (tree t);

  void     init_env (string var, tree by);
  void     init_default (string var);
  void     init_style ();
  void     init_style (string style);
  void     init_add_package (string package);
  void     init_remove_package (string package);

  void     typeset_style_use_cache (tree style);
  void     typeset_preamble ();
  void     typeset_prepare ();
  void     typeset_invalidate_env ();
  void     typeset_exec_until (path p);
  void     typeset_invalidate_all ();
  void     typeset (SI& x1, SI& y1, SI& x2, SI& y2);

  friend class tm_window_rep;
  friend class tm_data_rep;
  friend class tm_project_rep;
  friend class tm_server_rep;
};

#endif // defined EDIT_TYPESET_H
