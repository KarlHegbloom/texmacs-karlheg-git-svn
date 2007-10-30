
/******************************************************************************
* MODULE     : edit_interface.hpp
* DESCRIPTION: the interface for TeXmacs
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#ifndef EDIT_INTERFACE_H
#define EDIT_INTERFACE_H
#include "editor.hpp"
#include "timer.hpp"
#include "widget.hpp"
#include "Widkit/Event/attribute_event.hpp"

#define INPUT_NORMAL      0
#define INPUT_SEARCH      1
#define INPUT_REPLACE     2
#define INPUT_SPELL       3
#define INPUT_COMPLETE    4

string MODE_LANGUAGE (string mode);

class edit_interface_rep: virtual public editor_rep {
protected:
  int           env_change;    // which things have been changed ?
  time_t        last_change;   // time of last processed change
  time_t        last_update;   // time of last update of menu, icons and footer
  bool          do_animate;    // are we in an animation ?
  time_t        next_animate;  // time for next animation
  bool          full_screen;   // full screen mode ?
  bool          got_focus;     // do we have keyboard focus ?
  string        sh_s;          // current string for shorthands
  int           sh_len;        // length of translation
  widget        popup_win;     // the current popup window
  string        message_l;     // a left message to display
  string        message_r;     // a right message to display
  string        last_l;        // last displayed left message
  string        last_r;        // last displayed right message
  int           sfactor;       // the current shrinking factor
  SI            pixel;         // sfactor*PIXEL
  rectangles    copy_always;   // for wiping out cursor
  int           input_mode;    // INPUT_NORMAL, INPUT_SEARCH, INPUT_REPLACE

protected:
  time_t        last_click;    // last click on left mouse button
  SI            last_x, last_y;
  bool          start_drag, start_right_drag;
  bool          dragging, right_dragging;
  SI            start_x, start_y;
  SI            end_x, end_y;
  bool          made_selection;
  bool          table_selection;
  rectangles    selection_rects;
  rectangles    env_rects;
  cursor        oc;
  bool          temp_invalid_cursor;
  array<string> completions;
  string        completion_prefix;
  int           completion_pos;
  ps_device     shadow;
  SI            vx1, vy1, vx2, vy2;
  rectangles    stored_rects;
  ps_device     stored;
  rectangles    locus_new_rects;
  rectangles    locus_rects;
  list<string>  active_ids;
  int           cur_sb, cur_wb;
  SI            cur_wx, cur_wy;

public:
  edit_interface_rep ();
  ~edit_interface_rep ();
  operator tree ();
  void suspend ();
  void resume ();
  void get_size (SI& wx, SI& wy);

  /* routines for dealing with shrinked coordinates */
  int  get_pixel_size ();
  void set_shrinking_factor (int sf);
  void invalidate (SI x1, SI y1, SI x2, SI y2);
  void invalidate (rectangles rs);
  void update_visible ();
  SI   get_window_height ();
  void scroll_to (SI x, SI y1);
  void set_extents (SI x1, SI y1, SI x2, SI y2);

  /* repainting the window */
  void draw_text (ps_device dev, rectangles& l);
  void draw_surround (ps_device dev, rectangle r);
  void draw_context (ps_device dev, rectangle r);
  void draw_env (ps_device dev);
  void draw_cursor (ps_device dev);
  void draw_selection (ps_device dev);
  void draw_graphics (ps_device dev);
  void draw_pre (ps_device dev, rectangle r);
  void draw_post (ps_device dev, rectangle r);
  void draw_with_shadow (rectangle r);
  void draw_with_stored (rectangle r);

  /* handle changes */
  void notify_change (int changed);
  bool has_changed (int question);
  int  idle_time (int event_type= ANY_EVENT);
  int  change_time ();
  void apply_changes ();
  void animate ();

  /* miscellaneous */
  void compute_env_rects (path p, rectangles& rs, bool recurse);
  void cursor_visible ();
  void selection_visible ();
  void full_screen_mode (bool flag);
  void before_menu_action ();
  void after_menu_action ();

  /* keyboard handling */
  int  get_input_mode ();
  void set_input_mode (int mode);
  void set_input_normal ();
  bool in_normal_mode ();
  bool in_search_mode ();
  bool in_replace_mode ();
  bool in_spell_mode ();
  bool kbd_get_command (string which, string& help, command& cmd);
  bool try_shortcut (string comb);
  void key_press (string key);
  void emulate_keyboard (string keys, string action= "");
  bool complete_try ();
  void complete_message ();
  void complete_start (string prefix, array<string> compls);
  bool complete_keypress (string key);
  unsigned int get_kbd_modifiers ();

  /* mouse handling */
  void mouse_any (string s, SI x, SI y, time_t t);
  void mouse_click (SI x, SI y);
  bool mouse_extra_click (SI x, SI y);
  void mouse_drag (SI x, SI y);
  void mouse_select (SI x, SI y);
  void mouse_paste (SI x, SI y);
  void mouse_adjust (SI x, SI y);
  void mouse_scroll (SI x, SI y, bool up);
  cursor get_cursor ();
  void set_pointer (string name);
  void set_pointer (string curs_name, string mask_name);
  void update_active_loci ();

  /* the footer */
  string compute_text_footer (tree st);
  string compute_operation_footer (tree st);
  string compute_compound_footer (tree t, path p);
  bool   set_latex_footer (tree st);
  bool   set_hybrid_footer (tree st);
  void   set_left_footer (string l);
  void   append_left_footer (string& s, string env_var);
  void   set_left_footer ();
  void   set_right_footer (string r);
  void   set_right_footer ();
  void   set_footer ();
  void   set_message (string l, string r= "", bool temp= false);
  void   recall_message ();

  /* event handlers */
  void handle_get_size_hint (SI& w, SI& h);
  void handle_notify_resize (SI w, SI h);
  void handle_keypress (string key, time_t t);
  void handle_keyboard_focus (bool has_focus, time_t t);
  void handle_mouse (string kind, SI x, SI y, time_t t, int status);
  void handle_set_shrinking_factor (int sf);
  void handle_clear (SI x1, SI y1, SI x2, SI y2);
  void handle_repaint (SI x1, SI y1, SI x2, SI y2);

  friend class interactive_command_rep;
  friend class tm_window_rep;
  friend class tm_project_rep;
};

#endif // defined EDIT_INTERFACE_H
