
/******************************************************************************
* MODULE     : editor.hpp
* DESCRIPTION: abstract TeXmacs editors
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#ifndef EDITOR_H
#define EDITOR_H
#include "typesetter.hpp"
#include "Widget/attribute_widget.hpp"
#include "server.hpp"
#include "scheme.hpp"
#include "drd_info.hpp"
#define TEXMACS_COPYRIGHT (string("(c) 1999-2003 by Joris van der Hoeven"))

#define THE_CURSOR 1
#define THE_FOCUS 2
#define THE_TREE 4
#define THE_ENVIRONMENT 8
#define THE_SELECTION 16
#define THE_DECORATIONS 32
#define THE_EXTENTS 64
#define THE_AUTOMATIC_SIZE 128

class tm_buffer_rep;
typedef tm_buffer_rep* tm_buffer;
class server_rep;

class editor_rep: public attribute_widget_rep {
public:
  server_rep* sv;   // the underlying texmacs server

protected:
  tm_buffer   buf;  // the underlying buffer
  drd_info    drd;  // the drd for the buffer
  tree&       et;   // all TeXmacs trees
  box         eb;   // box translation of tree
  path        rp;   // path to the root of the document in et
  path        tp;   // path of cursor in tree

  /* exchanging information with the interface */
  virtual SI      get_window_height () = 0;
  virtual void    get_selection (path& start, path& end) = 0;
  virtual void    set_selection (path start, path end) = 0;
  virtual cursor& the_cursor () = 0;
  virtual cursor& the_ghost_cursor () = 0;

  /* exchanging information with the typesetter */
  virtual typesetter           get_typesetter () = 0;
  virtual tree                 get_style () = 0;
  virtual void                 set_style (tree t) = 0;
  virtual hashmap<string,tree> get_init () = 0;
  virtual hashmap<string,tree> get_fin () = 0;
  virtual void                 set_init (hashmap<string,tree> H= tree ("?"))=0;
  virtual void                 add_init (hashmap<string,tree> H) = 0;
  virtual void                 set_fin (hashmap<string,tree> H) = 0;
  virtual void                 set_base_name (url name) = 0;

  /* exchanging property information */
  virtual void   set_bool_property (string what, bool val) = 0;
  virtual void   set_int_property (string what, int val) = 0;
  virtual void   set_string_property (string what, string val) = 0;
  virtual bool   get_bool_property (string what) = 0;
  virtual int    get_int_property (string what) = 0;
  virtual string get_string_property (string what) = 0;

protected:
  /* protected routines from edit_typeset */
  virtual void   typeset_preamble () = 0;
  virtual void   typeset_invalidate_env () = 0;
  virtual void   typeset (SI& x1, SI& y1, SI& x2, SI& y2) = 0;

  /* protected subroutines for deletion of content */
  virtual void back_prime (tree t, path p, bool forward) = 0;
  virtual void back_in_wide (tree t, path p, bool forward) = 0;
  virtual void back_in_tree (tree t, path p, bool forward) = 0;
  virtual void back_table (path p, bool forward) = 0;
  virtual void back_in_table (tree t, path p, bool forward) = 0;
  virtual void back_monolithic (path p) = 0;
  virtual void back_general (path p, bool forward) = 0;
  virtual void back_in_with (tree t, path p, bool forward) = 0;
  virtual void back_in_general (tree t, path p, bool forward) = 0;

  /* other protected subroutines */
  virtual path tree_path (SI x, SI y, SI delta) = 0;
  virtual void apply_changes () = 0;
  virtual void correct_concat (path p, int done=0) = 0;
  virtual path search_format (int& row, int& col) = 0;
  virtual void table_bound (path fp, int& i1, int& j1, int& i2, int& j2) = 0;
  virtual tree table_get_subtable (path p, int i1, int j1, int i2, int j2) = 0;
  virtual void table_write_subtable (path fp, int row, int col, tree subt) = 0;
  virtual void table_del_format (path fp, int I1, int J1,
				 int I2, int J2, string var) = 0;

public:
  editor_rep ();
  editor_rep (server_rep* sv, display dis, tm_buffer buf);
  inline virtual ~editor_rep () {}

  /* public routines from edit_interface */
  virtual void suspend () = 0;
  virtual void resume () = 0;
  virtual void notify_change (int changed) = 0;
  virtual bool has_changed (int question) = 0;
  virtual bool kbd_get_command (string cmd_s, string& help, command& cmd) = 0;
  virtual void full_screen_mode (bool flag) = 0;
  virtual void before_menu_action () = 0;
  virtual void after_menu_action () = 0;
  virtual int  get_input_mode () = 0;
  virtual void set_input_mode (int mode) = 0;
  virtual void set_input_normal () = 0;
  virtual bool in_normal_mode () = 0;
  virtual bool in_search_mode () = 0;
  virtual bool in_replace_mode () = 0;
  virtual bool in_spell_mode () = 0;
  virtual void key_press (string key) = 0;
  virtual void emulate_keyboard (string keys, string action= "") = 0;
  virtual void show_keymaps () = 0;
  virtual bool complete_try () = 0;
  virtual void complete_start (string prefix, array<string> compls) = 0;
  virtual bool complete_keypress (string key) = 0;
  virtual void mouse_any (string s, SI x, SI y, time_t t) = 0;
  virtual void mouse_click (SI x, SI y) = 0;
  virtual bool mouse_extra_click (SI x, SI y) = 0;
  virtual void mouse_drag (SI x, SI y) = 0;
  virtual void mouse_select (SI x, SI y) = 0;
  virtual void mouse_paste (SI x, SI y) = 0;
  virtual void mouse_adjust (SI x, SI y) = 0;
  virtual void mouse_scroll (SI x, SI y, bool up) = 0;
  virtual cursor get_cursor () = 0;
  virtual void set_message (string l, string r= "") = 0;
  virtual void interactive (scheme_tree args, object cmd) = 0;

  /* public routines from edit_cursor */
  virtual path current_position () = 0;
  virtual void go_to (SI x, SI y) = 0;
  virtual void go_left () = 0;
  virtual void go_right () = 0;
  virtual void go_up () = 0;
  virtual void go_down () = 0;
  virtual void go_page_up () = 0;
  virtual void go_page_down () = 0;
  virtual void go_to (path p) = 0;
  virtual void go_to_correct (path p) = 0;
  virtual void go_to_start (path p) = 0;
  virtual void go_to_end (path p) = 0;
  virtual void go_to_border (path p, bool at_start) = 0;
  virtual void go_to_here () = 0;
  virtual void go_start () = 0;
  virtual void go_end () = 0;
  virtual void go_start_of (string what) = 0;
  virtual void go_end_of (string what) = 0;
  virtual void go_start_with (string var, string val) = 0;
  virtual void go_end_with (string var, string val) = 0;
  virtual void go_start_line () = 0;
  virtual void go_end_line () = 0;
  virtual void go_to_label (string s) = 0;
  virtual tree get_labels () = 0;

  /* public routines from edit_graphics */
  virtual bool   inside_graphics () = 0;
  virtual frame  find_frame () = 0;
  virtual void   find_limits (point& lim1, point& lim2) = 0;
  virtual point  adjust (point p) = 0;
  virtual tree   find_point (point p) = 0;
  virtual bool   mouse_graphics (string s, SI x, SI y, time_t t) = 0;

  /* public routines from edit_typeset */
  virtual void     clear_local_info () = 0;
  virtual SI       decode_length (string l) = 0;
  virtual string   add_lengths (string l1, string l2) = 0;
  virtual string   multiply_length (double x, string l) = 0;
  virtual bool     is_length (string s) = 0;
  virtual double   divide_lengths (string l1, string l2) = 0;
  virtual void     drd_update () = 0;
  virtual bool     defined_at_cursor (string var_name) = 0;
  virtual bool     defined_at_init (string var_name) = 0;
  virtual bool     defined_in_init (string var_name) = 0;
  virtual tree     get_env_value (string var_name, path p) = 0;
  virtual tree     get_env_value (string var_name) = 0;
  virtual tree     get_init_value (string var_name) = 0;
  virtual string   get_env_string (string var_name) = 0;
  virtual string   get_init_string (string var_name) = 0;
  virtual int      get_env_int (string var_name) = 0;
  virtual int      get_init_int (string var_name) = 0;
  virtual double   get_env_double (string var_name) = 0;
  virtual double   get_init_double (string var_name) = 0;
  virtual language get_env_language () = 0;
  virtual tree     exec_texmacs (tree t, path p= 0) = 0;
  virtual tree     exec_html (tree t, path p) = 0;
  virtual tree     exec_html (tree t) = 0;
  virtual void     init_style () = 0;
  virtual void     init_style (string style) = 0;
  virtual void     init_extra_style (string package, bool check= false) = 0;
  virtual void     init_env (string var, tree by) = 0;
  virtual void     init_default (string var) = 0;
  virtual void     typeset_invalidate_all () = 0;

  /* public routines from edit_modify */
  virtual void assign (path p, tree u) = 0;
  virtual void insert (path p, tree u) = 0;
  virtual void remove (path p, int nr) = 0;
  virtual void split (path p) = 0;
  virtual void join (path p) = 0;
  virtual void ins_unary (path p, tree_label op) = 0;
  virtual void rem_unary (path p) = 0;
  virtual void finished (path p) = 0;
  virtual void notify_assign (path p, tree u) = 0;
  virtual void notify_insert (path p, tree u) = 0;
  virtual void notify_remove (path p, int nr) = 0;
  virtual void notify_split (path p) = 0;
  virtual void notify_join (path p) = 0;
  virtual void notify_ins_unary (path p, tree_label op) = 0;
  virtual void notify_rem_unary (path p) = 0;
  virtual void post_notify (path p) = 0;
  virtual void undo () = 0;
  virtual void redo () = 0;
  virtual void assign_diff (path p, tree u) = 0;
  virtual int  position_new () = 0;
  virtual void position_delete (int i) = 0;
  virtual void position_set (int i, path p) = 0;
  virtual path position_get (int i) = 0;

  /* public routines from edit_text */
  virtual void correct (path p) = 0;
  virtual bool insert_return () = 0;
  virtual void remove_return (path p) = 0;
  virtual void insert_tree (tree t, path p_in_t) = 0;
  virtual void insert_tree (tree t) = 0;
  virtual void remove_text (bool forward) = 0;
  virtual void remove_structure (bool forward) = 0;
  virtual void remove_structure_upwards () = 0;

  virtual void make_space (tree t) = 0;
  virtual void make_space (string w) = 0;
  virtual void make_space (string w, string y1, string y2) = 0;
  virtual void make_hspace (string s) = 0;
  virtual void make_hspace (string smin, string sdef, string smax) = 0;
  virtual void make_vspace_before (string s) = 0;
  virtual void make_vspace_before (string smin, string sdef, string smax) = 0;
  virtual void make_vspace_after (string s) = 0;
  virtual void make_vspace_after (string smin, string sdef, string smax) = 0;
  virtual void make_htab (string spc) = 0;
  virtual void make_move (string x, string y) = 0;
  virtual void make_resize (string x1, string y1, string x2, string y2) = 0;
  virtual void make_insertion (string s) = 0;
  virtual void position_insertion (string what, bool flag) = 0;
  virtual void make_postscript (string file_name, bool link,
				string w, string h,
				string x1, string y1,
				string x2, string y2) = 0;

  /* public routines from edit_math */
  virtual void make_group () = 0;
  virtual void make_lprime (string s) = 0;
  virtual void make_rprime (string s) = 0;
  virtual void make_below () = 0;
  virtual void make_above () = 0;
  virtual void make_script (bool sup, bool right) = 0;
  virtual void make_fraction () = 0;
  virtual void make_sqrt () = 0;
  virtual void make_var_sqrt () = 0;
  virtual void make_wide (string wide) = 0;
  virtual void make_wide_under (string wide) = 0;
  virtual void make_neg () = 0;
  virtual void make_tree () = 0;

  virtual bool inside_tree () = 0;
  virtual void branch_insert (bool at_right) = 0;
  virtual void branch_delete (bool forward) = 0;

  /* public routines from edit_table */
  virtual void   make_table (int nr_rows=1, int nr_cols=1) = 0;
  virtual void   make_subtable (int nr_rows=1, int nr_cols=1) = 0;
  virtual void   table_disactivate () = 0;
  virtual void   table_extract_format () = 0;
  virtual void   table_insert_row (bool forward) = 0;
  virtual void   table_insert_column (bool forward) = 0;
  virtual void   table_delete_row (bool forward) = 0;
  virtual void   table_delete_column (bool forward) = 0;
  virtual int    table_nr_rows () = 0;
  virtual int    table_nr_columns () = 0;
  virtual int    table_which_row () = 0;
  virtual int    table_which_column () = 0;
  virtual path   table_search_cell (int row, int col) = 0;
  virtual void   table_go_to (int row, int col) = 0;
  virtual void   table_set_format (string var, string val) = 0;
  virtual string table_get_format (string var) = 0;
  virtual void   table_del_format (string var= "") = 0;
  virtual void   table_format_center () = 0;
  virtual void   table_row_decoration (bool forward) = 0;
  virtual void   table_column_decoration (bool forward) = 0;
  virtual void   set_cell_mode (string mode) = 0;
  virtual string get_cell_mode () = 0;
  virtual void   cell_set_format (string var, string val) = 0;
  virtual string cell_get_format (string var) = 0;
  virtual void   cell_del_format (string var= "") = 0;
  virtual void   cell_multi_paragraph (bool flag) = 0;
  virtual bool   cell_is_multi_paragraph () = 0;
  virtual void   table_test () = 0;

  /* public routines from edit_dynamic */
  virtual bool in_source () = 0;
  virtual path find_dynamic (path p) = 0;
  virtual void make_compound (tree_label l, int n=-1) = 0;
  virtual void activate () = 0;
  virtual void go_to_argument (path p, bool start_flag) = 0;
  virtual void insert_argument (path p, bool forward) = 0;
  virtual void insert_argument (bool forward) = 0;
  virtual void remove_argument (path p, bool forward) = 0;
  virtual void make_with (string var, string val) = 0;
  virtual void insert_with (path p, string var, tree val) = 0;
  virtual void remove_with (path p, string var) = 0;
  virtual void make_mod_active (tree_label l) = 0;
  virtual void make_style_with (string var, string val) = 0;
  virtual void make_hybrid () = 0;
  virtual bool activate_latex () = 0;
  virtual void activate_hybrid (bool with_args_hint) = 0;
  virtual void activate_symbol () = 0;
  virtual void activate_compound () = 0;
  virtual bool make_return_before () = 0;
  virtual bool make_return_after () = 0;
  virtual void temp_proof_fix () = 0;

  /* public routines from edit_process */
  virtual void process_mutators () = 0;
  virtual path get_mutator_path () = 0;
  virtual time_t get_mutator_time () = 0;
  virtual void invalidate_mutators () = 0;
  virtual void insert_mutator (tree body, string cmd) = 0;

  virtual void make_session (string lan, string session) = 0;
  virtual void start_input (string lan, string session, path p) = 0;
  virtual void process_input () = 0;
  virtual void start_output () = 0;
  virtual void session_message (string l, string r) = 0;
  virtual void session_use_math_input (bool flag) = 0;
  virtual bool session_is_using_math_input () = 0;
  virtual int  status_connection () = 0;
  virtual bool busy_connection () = 0;
  virtual void interrupt_connection () = 0;
  virtual void stop_connection () = 0;

  virtual void session_go_up () = 0;
  virtual void session_go_down () = 0;
  virtual void session_go_left () = 0;
  virtual void session_go_right () = 0;
  virtual void session_go_page_up () = 0;
  virtual void session_go_page_down () = 0;
  virtual void session_remove (bool forward) = 0;
  virtual void session_insert_text_field () = 0;
  virtual void session_insert_input_below () = 0;
  virtual void session_insert_input_above () = 0;
  virtual void session_fold_input () = 0;
  virtual void session_remove_input (bool forward) = 0;
  virtual void session_remove_all_outputs () = 0;
  virtual void session_remove_previous_output () = 0;
  virtual void session_split () = 0;
  virtual bool session_complete_try () = 0;

  virtual void generate_bibliography (string bib, string sty, string fname)= 0;
  virtual void generate_table_of_contents (string toc) = 0;
  virtual void generate_index (string idx) = 0;
  virtual void generate_glossary (string glo) = 0;
  virtual void generate_aux (string which= "") = 0;
  virtual bool get_save_aux () = 0;

  /* public routines from edit_select */
  virtual void select (path p) = 0;
  virtual void select (path start, path end) = 0;
  virtual void select_all () = 0;
  virtual void select_line () = 0;
  virtual void select_from_cursor () = 0;
  virtual void select_from_cursor_if_active () = 0;
  virtual void select_from_keyboard (bool flag) = 0;
  virtual void select_from_shift_keyboard () = 0;
  virtual void select_enlarge () = 0;
  virtual void select_enlarge_environmental () = 0;

  virtual bool selection_active_any () = 0;
  virtual bool selection_active_normal () = 0;
  virtual bool selection_active_table () = 0;
  virtual bool selection_active_small () = 0;
  virtual bool selection_active_enlarging () = 0;

  virtual void selection_raw_set (string key, tree t) = 0;
  virtual tree selection_raw_get (string key) = 0;
  virtual path selection_get_subtable (int& i1, int& j1, int& i2, int& j2) = 0;
  virtual void selection_get (selection& sel) = 0;
  virtual void selection_get (path& start, path& end) = 0;
  virtual path selection_get_start () = 0;
  virtual path selection_get_end () = 0;
  virtual void selection_set (string key, tree t, bool persistant= false) = 0;
  virtual void selection_set (tree t) = 0;
  virtual void selection_set_start (path p= path()) = 0;
  virtual void selection_set_end (path p= path()) = 0;
  virtual void selection_copy (string key= "primary") = 0;
  virtual void selection_paste (string key= "primary") = 0;
  virtual void selection_clear (string key= "primary") = 0;
  virtual void selection_cancel () = 0;
  virtual void selection_set_import (string fm) = 0;
  virtual void selection_set_export (string fm) = 0;
  virtual string selection_get_import () = 0;
  virtual string selection_get_export () = 0;

  virtual tree selection_get () = 0;
  virtual void selection_cut (string key= "primary") = 0;
  virtual tree selection_get_cut () = 0;
  virtual void selection_move () = 0;
  virtual void cut (path p) = 0;
  virtual void cut (path start, path end) = 0;

  /* public routines from edit_replace */
  virtual bool inside (string what) = 0;
  virtual bool inside (tree_label l) = 0;
  virtual bool inside_with (string var, string val) = 0;
  virtual string inside_which (tree t) = 0;
  virtual path search_upwards (string what) = 0;
  virtual path search_upwards (tree_label l) = 0;
  virtual path search_parent_upwards (tree_label l) = 0;
  virtual path search_parent_upwards (tree_label l, int& last) = 0;
  virtual path search_upwards_with (string var, string val) = 0;
  virtual path search_upwards_in_set (tree t) = 0;
  virtual path search_previous_compound (path init, string which) = 0;
  virtual path search_next_compound (path init, string which) = 0;
  virtual void search_start (bool forward= true) = 0;
  virtual void search_button_next () = 0;
  virtual bool search_keypress (string s) = 0;
  virtual void replace_start (tree what, tree by, bool forward= true) = 0;
  virtual bool replace_keypress (string s) = 0;
  virtual void spell_start () = 0;
  virtual void spell_replace (string by) = 0;
  virtual bool spell_keypress (string s) = 0;

  /* public routines from edit_main */
  virtual void set_property (scheme_tree what, scheme_tree val) = 0;
  virtual scheme_tree get_property (scheme_tree what) = 0;
  virtual void clear_buffer () = 0;
  virtual void new_window () = 0;
  virtual void clone_window () = 0;
  virtual void tex_buffer () = 0;
  virtual url  get_name () = 0;
  virtual void focus_on_this_editor () = 0;
  virtual void notify_page_change () = 0;
  virtual void print (url ps_name, bool to_file, int first, int last) = 0;
  virtual void print_to_file (url ps_name,
			      string first="1", string last="1000000") = 0;
  virtual void print_buffer (string first="1", string last="1000000") = 0;
  virtual void export_ps (url ps_name,
			  string first="1", string last="1000000") = 0;
  virtual void footer_eval (string s) = 0;
  virtual tree the_line () = 0;
  virtual tree the_buffer () = 0;
  virtual tree the_subtree (path p) = 0;
  virtual path the_path () = 0;
  virtual void show_tree () = 0;
  virtual void show_env () = 0;
  virtual void show_path () = 0;
  virtual void show_cursor () = 0;
  virtual void show_selection () = 0;
  virtual void show_meminfo () = 0;
  virtual void edit_special () = 0;
  virtual void edit_test () = 0;

  friend class tm_window_rep;
  friend class tm_data_rep;
  friend class tm_server_rep;
  friend class server_command_rep;
};

class editor {
  EXTEND_NULL(widget,editor);
public:
  inline bool operator == (editor w) { return rep == w.rep; }
  inline bool operator != (editor w) { return rep != w.rep; }
};
EXTEND_NULL_CODE(widget,editor);

editor new_editor (server_rep* sv, tm_buffer buf);

#define SERVER(cmd) {                    \
  tm_view temp_vw= sv->get_view (false); \
  focus_on_this_editor ();               \
  sv->cmd;                               \
  sv->set_view (temp_vw);                \
}

#endif // defined EDITOR_H
