
/******************************************************************************
* MODULE     : concater.hpp
* DESCRIPTION: Typesetting concatenations in two stages.
*                - produce an array of line items
*                - handle brackets and scripts
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#ifndef CONCATER_H
#define CONCATER_H
#include "typesetter.hpp"
#include "Format/line_item.hpp"
#include "Boxes/construct.hpp"

#define MODE_JUSTIFY   0
#define MODE_LEFT      1
#define MODE_CENTER    2
#define MODE_RIGHT     3

class concater_rep {
  edit_env              env;        // the environment
  array<line_item>      a;          // the line items

  // useful subroutines
  void print (int type, box b);
  void control (tree t, path ip);
  void marker (path ip);
  void ghost (string s, path ip);
  void ghost (string s, path ip, color col);
  void flag_ok (string s, path ip, color col);
  void flag (string s, path ip, color col);
  void print (space spc);
  void penalty_min (int p);
  void penalty_max (int p);
  void with_limits (int status);

  // textual markup
  void typeset_substring (string s, path ip, int pos);
  void typeset_string (string s, path ip);
  void typeset_document (tree t, path ip);
  void typeset_paragraph (tree t, path ip);
  void typeset_surround (tree t, path ip);
  void typeset_concat (tree t, path ip);
  void typeset_hspace (tree t, path ip);
  void typeset_space (tree t, path ip);
  void typeset_move (tree t, path ip);
  void typeset_resize (tree t, path ip);
  void typeset_float (tree t, path ip);
  void typeset_repeat (tree t, path ip);
  void typeset_formatting (tree t, path ip, string var);
  void typeset_decorated_box (tree t, path ip);

  // mathematical markup
  void typeset_group (tree t, path ip);
  void typeset_left (tree t, path ip);
  void typeset_middle (tree t, path ip);
  void typeset_right (tree t, path ip);
  void typeset_bigop (tree t, path ip);
  void typeset_lprime (tree t, path ip);
  void typeset_rprime (tree t, path ip);
  void typeset_below (tree t, path ip);
  void typeset_above (tree t, path ip);
  void typeset_script (tree t, path ip, bool right);
  void typeset_frac (tree t, path ip);
  void typeset_sqrt (tree t, path ip);
  void typeset_wide (tree t, path ip, bool above);
  void typeset_neg (tree t, path ip);
  void typeset_tree (tree t, path ip);
  void typeset_table (tree t, path ip);

  // disactivated markup
  void typeset_blue (tree t, path ip);
  void typeset_src_open (tree t, path ip, string extra);
  void typeset_src_middle (tree t, path ip, int i);
  void typeset_src_close (tree t, path ip);
  void typeset_src_args (tree t, path ip);
  void typeset_src_tag (tree t, path ip);
  void typeset_inactive (tree t, path ip);
  void typeset_error (tree t, path ip);

  // active macro mechanisms
  void typeset_assign (tree t, path ip);
  void typeset_with (tree t, path ip);
  void typeset_compound (tree t, path ip);
  void typeset_auto (tree t, path ip, tree macro);
  void typeset_include (tree t, path ip);
  void typeset_drd_props (tree t, path ip);
  void typeset_eval (tree t, path ip);
  void typeset_value (tree t, path ip);
  void typeset_argument (tree t, path ip);
  void typeset_eval_args (tree t, path ip);
  void typeset_mark (tree t, path ip);
  void typeset_dynamic (tree t, path ip);
  void typeset_executable (tree t, path ip);
  void typeset_rewrite (tree t, path ip);

  // miscellaneous active markup
  void typeset_if (tree t, path ip);
  void typeset_var_if (tree t, path ip);
  void typeset_case (tree t, path ip);
  void typeset_label (tree t, path ip);
  void typeset_reference (tree t, path ip, int type);
  void typeset_write (tree t, path ip);
  void typeset_specific (tree t, path ip);
  void typeset_hyperlink (tree t, path ip);
  void typeset_action (tree t, path ip);
  void typeset_tag (tree t, path ip);
  void typeset_meaning (tree t, path ip);
  void typeset_flag (tree t, path ip);

  // graphical markup
  void typeset_graphics (tree t, path ip);
  void typeset_superpose (tree t, path ip);
  void typeset_text_at (tree t, path ip);
  void typeset_point (tree t, path ip);
  void typeset_line (tree t, path ip, bool close);
  void typeset_spline (tree t, path ip, bool close);
  void typeset_var_spline (tree t, path ip);
  void typeset_cspline (tree t, path ip);
  void typeset_fill (tree t, path ip);
  void typeset_postscript (tree t, path ip);

  // postprocessing of brackets and scripts
  int  prec (int i);
  int  succ (int i);
  void pre_glue ();
  void glue (box b, int ref, int arg);
  void glue (box b, int ref, int arg1, int arg2);
  void clean_and_correct ();
  void handle_scripts (int start, int end);
  void handle_matching (int start, int end);
  void handle_brackets ();
  void kill_spaces ();

public:
  concater_rep (edit_env env);
  void typeset (tree t, path ip);
  void finish ();

  friend class liner_rep;
  friend class par_line_rep;
  friend class typesetter_rep;
  friend class document_rep;

  friend box              typeset_as_concat (edit_env env, tree t, path ip);
  friend array<line_item> typeset_concat (edit_env env, tree t, path ip);
  friend array<line_item> typeset_concat_range (edit_env env, tree t, path ip,
						int i1, int i2);
  friend array<line_item> typeset_marker (edit_env env, path ip);
  friend box              typeset_as_grid (edit_env env, tree t, path ip,
					   tree aspect);
};

typedef concater_rep* concater;

#endif // defined CONCATER_H
