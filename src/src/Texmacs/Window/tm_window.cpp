
/******************************************************************************
* MODULE     : tm_window.cpp
* DESCRIPTION: Main TeXmacs windows
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "tm_window.hpp"
#include "message.hpp"

int geometry_w= 800, geometry_h= 600;
int geometry_x= 0  , geometry_y= 0;

widget texmacs_window_widget (widget wid, tree geom);
widget make_menu_widget (object menu);

/******************************************************************************
* Meta editor constructor and destructor
******************************************************************************/

static int tm_window_serial= 0;

tm_window_rep::tm_window_rep (widget wid2, tree geom):
  win (texmacs_window_widget (wid2, geom)),
  wid (wid2), id (create_window_id ()),
  serial (tm_window_serial++),
  texmacs_menu (NULL), texmacs_icon_menu (NULL),
  text_ptr (NULL)
{
  sfactor= get_server () -> get_default_shrinking_factor ();
}

tm_window_rep::~tm_window_rep () {
  if (texmacs_menu != NULL) delete[] texmacs_menu;
  if (texmacs_icon_menu != NULL) delete[] texmacs_icon_menu;
  destroy_window_id (id);
}

/******************************************************************************
* Creation of TeXmacs window
******************************************************************************/

widget
texmacs_window_widget (widget wid, tree geom) {
  int W, H;
  int w= geometry_w, h= geometry_h;
  int x= geometry_x, y= geometry_y;
  if (is_tuple (geom) && N (geom) >= 2) {
    w= as_int (geom[0]);
    h= as_int (geom[1]);
  }
  gui_root_extents (W, H); W /= PIXEL; H /= PIXEL;
  if (x < 0) x= W + x + 1 - w;
  if (y < 0) y= H + y + 1 - h;
  widget win= plain_window_widget (wid, "TeXmacs");
  set_size (win, w*PIXEL, h*PIXEL);
  set_position (win, x*PIXEL, (-y)*PIXEL);
  return win;
}

/******************************************************************************
* Meta mathods
******************************************************************************/

void
tm_window_rep::set_window_name (string s) {
  set_name (wid, s);
}

void
tm_window_rep::map () {
  set_visibility (win, true);
}

void
tm_window_rep::unmap () {
  set_visibility (win, false);
}

/******************************************************************************
* Menus
******************************************************************************/

void
tm_window_rep::menu_main (string menu) {
  if (texmacs_menu == NULL) texmacs_menu= new object[1];
  object xmenu= call ("menu-expand", eval ("'" * menu));
  if (xmenu == texmacs_menu[0]) return;
  texmacs_menu[0]= xmenu;
  widget w= make_menu_widget (xmenu);
  ::set_main_menu (wid, w);
}

void
tm_window_rep::menu_icons (int which, string menu) {
  if ((which<0) || (which>2)) return;
  if (texmacs_icon_menu == NULL) texmacs_icon_menu= new object[3];
  object xmenu= call ("menu-expand", eval ("'" * menu));
  if (xmenu == texmacs_icon_menu[which]) return;
  texmacs_icon_menu[which]= xmenu;
  widget w= make_menu_widget (xmenu);
  if      (which == 0) set_main_icons (wid, w);
  else if (which == 1) set_context_icons (wid, w);
  else if (which == 2) set_user_icons (wid, w);
}

void
tm_window_rep::set_header_flag (bool flag) {
  set_header_visibility (wid, flag);
}

void
tm_window_rep::set_icon_bar_flag (int which, bool flag) {
  if      (which == 0) set_main_icons_visibility (wid, flag);
  else if (which == 1) set_context_icons_visibility (wid, flag);
  else if (which == 2) set_user_icons_visibility (wid, flag);
}

bool
tm_window_rep::get_header_flag () {
  return get_header_visibility (wid);
}

bool
tm_window_rep::get_icon_bar_flag (int which) {
  if      (which == 0) return get_main_icons_visibility (wid);
  else if (which == 1) return get_context_icons_visibility (wid);
  else if (which == 2) return get_user_icons_visibility (wid);
  else return false;
}

/******************************************************************************
* The canvas
******************************************************************************/

void
tm_window_rep::set_shrinking_factor (int sf) {
  sfactor= sf;
  ::set_shrinking_factor (wid, sf);
}

int
tm_window_rep::get_shrinking_factor () {
  return sfactor;
}

void
tm_window_rep::get_visible (SI& x1, SI& y1, SI& x2, SI& y2) {
  get_visible_part (wid, x1, y1, x2, y2);
}

void
tm_window_rep::get_extents (SI& x1, SI& y1, SI& x2, SI& y2) {
  ::get_extents (wid, x1, y1, x2, y2);
}

void
tm_window_rep::set_extents (SI x1, SI y1, SI x2, SI y2) {
  ::set_extents (wid, x1, y1, x2, y2);
}

void
tm_window_rep::set_scrollbars (int i) {
  ::set_scrollbars_visibility (wid, i);
}

void
tm_window_rep::get_scroll_pos (SI& x, SI& y) {
  get_scroll_position (wid, x, y);
}

void
tm_window_rep::set_scroll_pos (SI x, SI y) {
  set_scroll_position (wid, x, y);
}

/******************************************************************************
* The footer as a status bar
******************************************************************************/

bool
tm_window_rep::get_footer_flag () {
  return get_footer_visibility (wid);
}

void
tm_window_rep::set_footer_flag (bool flag) {
  set_footer_visibility (wid, flag);
}

void
tm_window_rep::set_left_footer (string s) {
  ::set_left_footer (wid, s);
}

void
tm_window_rep::set_right_footer (string s) {
  ::set_right_footer (wid, s);
}

/******************************************************************************
* Interactive commands on the footer
******************************************************************************/

class ia_command_rep: public command_rep {
  tm_window_rep* win;
public:
  ia_command_rep (tm_window_rep* win2): win (win2) {}
  void apply () { win->interactive_return (); }
  ostream& print (ostream& out) { return out << "tm_window command"; }
};

bool
tm_window_rep::get_interactive_mode () {
  return ::get_interactive_mode (wid);
}

void
tm_window_rep::set_interactive_mode (bool flag) {
  ::set_interactive_mode (wid, flag);
}

void
tm_window_rep::interactive (string name, string type, array<string> def,
			    string& s, command cmd)
{
  if (get_interactive_mode ()) { s= "cancel"; return; }
  text_ptr = &s;
  call_back= cmd;
  widget tw = text_widget (name, black, false, "english");
  widget inp= input_text_widget (new ia_command_rep (this), type, def);
  set_interactive_prompt (wid, tw);
  set_interactive_input (wid, inp);
  set_interactive_mode (true);
}

void
tm_window_rep::interactive_return () {
  *text_ptr= get_interactive_input (wid);
  text_ptr= NULL;
  set_interactive_mode (false);
  call_back ();
}
