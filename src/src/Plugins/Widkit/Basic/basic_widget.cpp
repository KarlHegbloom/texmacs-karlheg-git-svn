
/******************************************************************************
* MODULE     : basic_widget.cpp
* DESCRIPTION: Basic widgets can handle the most common events
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "Widkit/basic_widget.hpp"
#include "display.hpp"
#include "window.hpp"

SI get_dx(gravity grav, int w);
SI get_dy(gravity grav, int h);

/******************************************************************************
* Constructors and destructors
******************************************************************************/

basic_widget_rep::basic_widget_rep (gravity grav):
  wk_widget_rep (array<wk_widget> (0), array<string> (0), grav),
  ptr_focus (-1) {}
basic_widget_rep::basic_widget_rep (
  array<wk_widget> a, gravity grav):
    wk_widget_rep (a, array<string> (N(a)), grav), ptr_focus (-1) {}
basic_widget_rep::basic_widget_rep (
  array<wk_widget> a2, array<string> name2, gravity grav2):
    wk_widget_rep (a2, name2, grav2), ptr_focus (-1) {}

/******************************************************************************
* Generating events in local coordinates
******************************************************************************/

event
basic_widget_rep::emit_position (SI rx, SI ry, SI w, SI h, gravity grav) {
  return ::emit_position (ox+ rx, oy+ ry, w, h, grav);
}

event
basic_widget_rep::emit_invalidate (SI x1, SI y1, SI x2, SI y2) {
  return ::emit_invalidate (ox+ x1, oy+ y1, ox+ x2, oy+ y2);
}

event
basic_widget_rep::emit_mouse (mouse_event ev) {
  return ::emit_mouse (ev, ev->type, ox+ ev->x, oy+ ev->y);
}

event
basic_widget_rep::emit_mouse (mouse_event ev, string type) {
  return ::emit_mouse (ev, type, ox+ ev->x, oy+ ev->y);
}

event
basic_widget_rep::emit_mouse (mouse_event ev, string type, SI x, SI y) {
  return ::emit_mouse (ev, type, ox+x, oy+y);
}

event
basic_widget_rep::emit_clear (SI x1, SI y1, SI x2, SI y2) {
  return ::emit_clear (ox+ x1, oy+ y1, ox+ x2, oy+ y2);
}

event
basic_widget_rep::emit_repaint (SI x1, SI y1, SI x2, SI y2, bool& stop) {
  return ::emit_repaint (ox+ x1, oy+ y1, ox+ x2, oy+ y2, stop);
}

event
basic_widget_rep::emit_find_child (SI x, SI y, int& which) {
  return ::emit_find_child (ox+ x, oy+ y, which);
}

/******************************************************************************
* Exchanging information with the widget
******************************************************************************/

void
basic_widget_rep::handle_get_size (get_size_event ev) {
  if (N(a)>0) {
    int i;
    SI w= 0, h= 0;
    for (i=0; i<N(a); i++) {
      SI ww= ev->w, hh= ev->h;
      a[i] << get_size (ww, hh, ev->mode);
      w= max (w, ww);
      h= max (h, hh);
    }
    ev->w= w;
    ev->h= h;
  }
}

void
basic_widget_rep::handle_get_widget (get_widget_event ev) {
  int i;
  for (i=0; i<N(a); i++)
    if (name[i] == ev->which) {
      ev->w= a[i];
      return;
    }
  fatal_error ("Could not get widget attribute " * ev->which);
}

void
basic_widget_rep::handle_set_widget (set_widget_event ev) {
  int i;
  for (i=0; i<N(a); i++)
    if (name[i] == ev->which) { a[i]= ev->w; return; }
  fatal_error ("Could not set widget attribute " * ev->which);
}

void
basic_widget_rep::handle_set_language (set_language_event ev) {
  int i;
  for (i=0; i<N(a); i++) a[i] << ev;
  ev->resize= true;
}

/******************************************************************************
* Handling structure events
******************************************************************************/

void
basic_widget_rep::handle_attach_window (attach_window_event ev) {
  if ((win!=NULL) && (ev->win!=NULL) && (win!=ev->win))
    fatal_error ("Widget already attached to another window",
		 "basic_widget_rep::handle_attach_window");
  else win= ev->win;
  int i;
  for (i=0; i<N(a); i++) a[i] << emit_attach_window (win);
}

void
basic_widget_rep::handle_position (position_event ev) { (void) ev;
  int i;
  for (i=0; i<N(a); i++)
    a[i] << emit_position (0, 0, w, h, grav);
}

void
basic_widget_rep::handle_move (move_event ev) { (void) ev; }

void
basic_widget_rep::handle_resize (resize_event ev) { (void) ev;
  this << emit_update ();
}

void
basic_widget_rep::handle_destroy (destroy_event ev) { (void) ev;
}

/******************************************************************************
* Handling input/output events
******************************************************************************/

void
basic_widget_rep::handle_keypress (keypress_event ev) { (void) ev; }

void
basic_widget_rep::handle_keyboard_focus (keyboard_focus_event ev) {(void) ev;}

void
basic_widget_rep::handle_mouse (mouse_event ev) {
  string type= ev->type;
  SI     x= ev->x, y= ev->y;
  int    focus;

  this << emit_find_child (x, y, focus);
  if (type == "leave") focus=-1;
  if (focus != ptr_focus) {
    if ((ptr_focus >= 0) && (ptr_focus < N(a)))
      a[ptr_focus] << emit_mouse (ev, "leave");
    ptr_focus= focus;
    if ((ptr_focus >= 0) && (ptr_focus < N(a)))
      a[ptr_focus] << emit_mouse (ev, "enter");
    if ((type == "move") || (type == "enter") || (type == "leave")) return;
  }
  if ((ptr_focus >= 0) && (ptr_focus < N(a)))
    a[ptr_focus] << emit_mouse (ev);
}

void
basic_widget_rep::handle_alarm (alarm_event ev) {
  cout << "Alarm: " << ev->message << "\n";
}

void
basic_widget_rep::handle_clear (clear_event ev) {
  win->set_background (the_display->white);
  win->clear (ev->x1, ev->y1, ev->x2, ev->y2);
}

void
basic_widget_rep::handle_repaint (repaint_event ev) { (void) ev; }

/******************************************************************************
* Handling requests and miscellaneous events
******************************************************************************/

void
basic_widget_rep::handle_update (update_event ev) { (void) ev;
  if (attached ()) {
    this << emit_attach_window (win);
    this << emit_reposition ();
    this << emit_invalidate_all ();
  }
}

void
basic_widget_rep::handle_invalidate (invalidate_event ev) {
  if (ev->all_flag) win->invalidate (x1()-ox, y1()-oy, x2()-ox, y2()-oy);
  else win->invalidate (ev->x1, ev->y1, ev->x2, ev->y2);
}

void
basic_widget_rep::handle_keyboard_grab (keyboard_grab_event ev) { (void) ev; }

void
basic_widget_rep::handle_mouse_grab (mouse_grab_event ev) {
  if (ev->flag) the_display->grab_pointer (this);
  else {
    /*
    if (the_display->has_grab_pointer (this))
      fatal_error ("Widget does not have pointer grab",
		   "basic_widget_rep::handle_mouse_grab");
    */
    the_display->ungrab_pointer ();
  }
}

void
basic_widget_rep::handle_request_alarm (request_alarm_event ev) { (void) ev; }

void
basic_widget_rep::handle_find_child (find_child_event ev) {
  int& i= ev->which;
  for (i=0; i<N(a); i++)
    if ((ev->x >= a[i]->x1()-ox) && (ev->x < a[i]->x2()-ox) &&
	(ev->y >= a[i]->y1()-oy) && (ev->y < a[i]->y2()-oy)) return;
  i= -1;
}

/******************************************************************************
* The main event dispatcher
******************************************************************************/

void
test_round (wk_widget w, string var, SI num) {
  if (num != ((num>>8)<<8))
    w->fatal_error ("Bad rounding of " * var * "=" * as_string (num),
		    "test_round", "basic_widget.cpp");
}

static void
test_window_attached (event ev, wk_widget w) {
  if (!w->attached ()) {
    cerr << "\n" << HRULE << "\n";
    cerr << ev << " was sent to\n" << w;
    cerr << HRULE << "\n";
    fatal_error ("widget was not yet attached to window",
		 "basic_widget::handle", "basic_diwget.cpp");
  }
}

bool
basic_widget_rep::handle (event ev) {
  if (DEBUG_EVENTS) cout << "TeXmacs] " << ev << "\n";
  // " ---> " << wk_widget (this) << "\n";
  if (attached ()) win->set_origin (ox, oy);
  switch (ev->type) {
  case GET_SIZE_EVENT:
    handle_get_size (ev);
    return true;
  case GET_WIDGET_EVENT:
    handle_get_widget (ev);
    return true;
  case SET_WIDGET_EVENT:
    handle_set_widget (ev);
    return true;
  case SET_LANGUAGE_EVENT:
    handle_set_language (ev);
    return true;
  case ATTACH_WINDOW_EVENT:
    handle_attach_window (ev);
    return true;
  case POSITION_EVENT: {
    position_event e (ev);
    if (!e->flag) {
      ox= e->ox+ get_dx (grav, e->w)- get_dx (e->grav, e->w);
                 test_round (this, "ox", ox);
      oy= e->oy+ get_dy (grav, e->h)- get_dy (e->grav, e->h);
                 test_round (this, "oy", oy);
      w = e->w ; test_round (this, "w", w);
      h = e->h ; test_round (this, "h", h);
    }
    ev= emit_reposition ();
    handle_position (ev);
    return true;
  }
  case MOVE_EVENT:
    handle_move (ev);
    return true;
  case RESIZE_EVENT:
    handle_resize (ev);
    return true;
  case DESTROY_EVENT:
    handle_destroy (ev);
    return true;
  case KEYPRESS_EVENT:
    test_window_attached (ev, this);
    handle_keypress (ev);
    return true;
  case KEYBOARD_FOCUS_EVENT:
    test_window_attached (ev, this);
    handle_keyboard_focus (ev);
    return true;
  case MOUSE_EVENT: {
    test_window_attached (ev, this);
    mouse_event e (ev);
    ev= ::emit_mouse (e, e->type, e->x - ox, e->y - oy);
    handle_mouse (ev);
    return true;
  }
  case ALARM_EVENT: {
    alarm_event e (ev);
    handle_alarm (e);
    return true;
  }
  case CLEAR_EVENT: {
    test_window_attached (ev, this);
    clear_event e (ev);
    SI rx1= max (e->x1, x1())- ox;
    SI ry1= max (e->y1, y1())- oy;
    SI rx2= min (e->x2, x2())- ox;
    SI ry2= min (e->y2, y2())- oy;
    if ((rx2 > rx1) && (ry2 > ry1)) {
      event ev= ::emit_clear (rx1, ry1, rx2, ry2);
      win->clip (rx1, ry1, rx2, ry2);
      handle_clear (ev);
      win->unclip ();
    }
    return true;
  }
  case REPAINT_EVENT: {
    test_window_attached (ev, this);
    repaint_event e (ev);
    SI rx1= max (e->x1, x1())- ox;
    SI ry1= max (e->y1, y1())- oy;
    SI rx2= min (e->x2, x2())- ox;
    SI ry2= min (e->y2, y2())- oy;

    if ((rx2 > rx1) && (ry2 > ry1)) {
      event ev= ::emit_repaint (rx1, ry1, rx2, ry2, e->stop);
      win->clip (rx1, ry1, rx2, ry2);
      handle_repaint (ev);
      win->unclip ();
    }

    int i;
    ev= emit_repaint (rx1, ry1, rx2, ry2, e->stop);
    for (i=0; i<N(a); i++) a[i] << ev;
    return true;
  }
  case UPDATE_EVENT:
    test_window_attached (ev, this);
    handle_update (ev);
    return true;
  case INVALIDATE_EVENT: {
    test_window_attached (ev, this);
    invalidate_event e (ev);
    if (!e->all_flag)
      ev= ::emit_invalidate (e->x1- ox, e->y1- oy, e->x2- ox, e->y2- oy);
    handle_invalidate (ev);
    return true;
  }
  case KEYBOARD_GRAB_EVENT:
    handle_keyboard_grab (ev);
    return true;
  case MOUSE_GRAB_EVENT:
    handle_mouse_grab (ev);
    return true;
  case REQUEST_ALARM_EVENT:
    handle_request_alarm (ev);
    return true;
  case FIND_CHILD_EVENT: {
    find_child_event e (ev);
    ev= ::emit_find_child (e->x- ox, e->y- oy, e->which);
    handle_find_child (ev);
    return true;
  }
  }
  return false;
}
