
/******************************************************************************
* MODULE     : x_window.cpp
* DESCRIPTION: Windows under X11
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "X11/x_window.hpp"
#include "message.hpp"

extern int nr_windows;

hashmap<Window,pointer> Window_to_window (NULL);

/******************************************************************************
* Creation and deletion of an x_window
******************************************************************************/

void
x_window_rep::set_hints (SI min_w, SI min_h, SI max_w, SI max_h) {
  XSizeHints* size_hints;
  XWMHints*   wm_hints;
  XClassHint* class_hints;
  if (!(size_hints= XAllocSizeHints ()))
    fatal_error ("out of memory (X server)", "set_attributes");
  if (!(wm_hints= XAllocWMHints ()))
    fatal_error ("out of memory (X server)", "set_attributes");
  if (!(class_hints= XAllocClassHint ()))
    fatal_error ("out of memory (X server)", "set_attributes");

  XTextProperty Window_Name;
  XTextProperty Icon_Name;
  if (XStringListToTextProperty (&name, 1, &Window_Name) == 0)
    fatal_error ("out of memory (X server)", "set_attributes");
  if (XStringListToTextProperty (&name, 1, &Icon_Name) == 0)
    fatal_error ("out of memory (X server)", "set_attributes");

  // int start_1= texmacs_time ();
  if (!gui->xpm_pixmap->contains ("TeXmacs.xpm"))
    xpm_initialize ("TeXmacs.xpm");
  Pixmap pm= (Pixmap) gui->xpm_pixmap ["TeXmacs.xpm"];
  // cout << "Getting pixmap required " << (texmacs_time ()-start_1) << " ms\n";

  // int start_2= texmacs_time ();
  size_hints->flags       = PPosition | PSize | PMinSize | PMaxSize;
  size_hints->min_width   = min_w;
  size_hints->min_height  = min_h;
  size_hints->max_width   = max_w;
  size_hints->max_height  = max_h;
  wm_hints->initial_state = NormalState;
  wm_hints->input         = true;
  wm_hints->icon_pixmap   = pm;
  wm_hints->flags         = StateHint | IconPixmapHint | InputHint;
  class_hints->res_name   = name;
  class_hints->res_class  = name;

  XSetWMProperties (
    dpy,
    win,
    &Window_Name,
    &Icon_Name,
    gui->argv,
    gui->argc,
    size_hints,
    wm_hints,
    class_hints
  );
  // cout << "Setting hints required " << (texmacs_time ()-start_2) << " ms\n";
}

void
x_window_rep::initialize () {
  SI min_w= Min_w, min_h= Min_h;
  SI def_w= Def_w, def_h= Def_h;
  SI max_w= Max_w, max_h= Max_h;

  dpy= gui->dpy;
  gc = gui->gc;
  full_screen_flag= false;
  
  // int start_1= texmacs_time ();
  set_origin (0, 0);
  decode (def_w, def_h); def_h= -def_h;
  decode (min_w, min_h); min_h= -min_h;
  decode (max_w, max_h); max_h= -max_h;
  // cout << "Size computation required " << (texmacs_time ()-start_1) << " ms\n";

  // int start_2= texmacs_time ();
  unsigned long valuemask= CWOverrideRedirect | CWSaveUnder;
  //unsigned long valuemask= CWOverrideRedirect | CWSaveUnder | CWBackingStore;
  XSetWindowAttributes setattr;
  setattr.override_redirect= (name==NULL);
  setattr.save_under       = True; // (name==NULL);
  // setattr.backing_store    = Always;
  // FIXME: backing store does not seem to work correctly
  if (win_w == 0) win_w= def_w;
  if (win_h == 0) win_h= def_h;
  if ((win_x+ win_w) > gui->screen_width) win_x= gui->screen_width- win_w;
  if (win_x < 0) win_x= 0;
  if ((win_y+ win_h) > gui->screen_height) win_y= gui->screen_height- win_h;
  if (win_y < 0) win_y=0;
  win= XCreateWindow (dpy, gui->root, win_x, win_y, win_w, win_h, 0,
		      gui->depth, InputOutput, CopyFromParent,
		      valuemask, &setattr);
  x_drawable_rep::win= (Drawable) win;
  // cout << "XWindow creation required " << (texmacs_time ()-start_2) << " ms\n";

  // cout << "Hints: " << min_w << ", " << min_h << " --- "
  // << max_w << ", " << max_h << "\n";
  if (name == NULL) name= const_cast<char*> ("popup");
  if (the_name == "") the_name= name;
  set_hints (min_w, min_h, max_w, max_h);

  unsigned long ic_mask= 0;
  ic_ok= false;
  if (gui->im_ok) {
    ic= XCreateIC (gui->im,
		   XNInputStyle, XIMPreeditNothing | XIMStatusNothing,
		   XNClientWindow, win,
		   NULL);
    if (ic == NULL)
      cout << "TeXmacs] Warning: couldn't create input context\n";
    else {
      ic_ok= true;
      XGetICValues (ic, XNFilterEvents, &ic_mask, NULL);
    }
  }

  XSelectInput (dpy, win,
		ExposureMask | StructureNotifyMask |
		SubstructureNotifyMask | FocusChangeMask |
		PointerMotionMask | EnterWindowMask | LeaveWindowMask |
		ButtonPressMask | ButtonReleaseMask |
		KeyPressMask | ic_mask);

  Atom wm_protocols     = XInternAtom(dpy, "WM_PROTOCOLS", 1);
  Atom wm_delete_window = XInternAtom(dpy, "WM_DELETE_WINDOW", 1);
  XSetWMProtocols (dpy, win, &wm_protocols, 1);
  XSetWMProtocols (dpy, win, &wm_delete_window, 1);

  nr_windows++;
  Window_to_window (win)= (void*) this;
  set_identifier (w, (int) win);
  notify_position (w, 0, 0);
  notify_size (w, Def_w, Def_h);
}

x_window_rep::x_window_rep (widget w2, x_gui gui2, char* n2,
			    SI min_w, SI min_h, SI def_w, SI def_h,
			    SI max_w, SI max_h):
  x_drawable_rep (gui2), window_rep (), w (w2), gui (gui2), name (n2),
  Min_w (min_w), Min_h (min_h), Def_w (def_w), Def_h (def_h),
  Max_w (max_w), Max_h (max_h),
  win_x (0), win_y (0), win_w (Def_w/PIXEL), win_h (Def_h/PIXEL),
  kbd_focus (w.rep), has_focus (false)
{
  initialize ();
  gui->created_window (win);
}

x_window_rep::~x_window_rep () {
  set_identifier (w, 0);

  XEvent report;
  while (XCheckWindowEvent (dpy, win, 0xffffffff, &report));

  if (ic_ok) XDestroyIC (ic);
  Window_to_window->reset (win);
  nr_windows--;
  XDestroyWindow (dpy, win);
  gui->deleted_window (win);
}

widget
x_window_rep::get_widget () {
  return w;
}

void
x_window_rep::get_extents (int& w, int& h) {
  w= win_w;
  h= win_h;
}

Window
get_Window (widget w) {
  int id= get_identifier (w);
  if (id == 0) {
    cerr << "\nwidget = " << w << "\n";
    fatal_error ("widget is not attached to a window", "get_Window");
  }
  return (Window) id;
}

x_window
get_x_window (widget w) {
  int id= get_identifier (w);
  if (id == 0) return NULL;
  else return (x_window) Window_to_window[(Window) id];
}

int
get_identifier (window w) {
  if (w == NULL) return 0;
  else return (int) (((x_window) w) -> win);
}

window
get_window (int id) {
  if (id == 0) return NULL;
  else return (window) ((x_window) Window_to_window[(Window) id]);
}

/******************************************************************************
* Window apping and appearance
******************************************************************************/

void
x_window_rep::get_position (SI& x, SI& y) {
#ifdef OS_WIN32
  XGetWindowPos (dpy, win, &win_x, &win_y);
  x=  win_x*PIXEL;
  y= -win_y*PIXEL;
#else
  int xx, yy;
  Window ww;
  bool b;
  b=  XTranslateCoordinates (dpy, win, gui->root, 0, 0, &xx, &yy, &ww);
  x=  xx*PIXEL;
  y= -yy*PIXEL;
#endif
}

void
x_window_rep::get_size (SI& ww, SI& hh) {
  ww= win_w*PIXEL;
  hh= win_h*PIXEL;
}

void
x_window_rep::set_position (SI x, SI y) {
  x= x/PIXEL;
  y= -y/PIXEL;
  if ((x+ win_w) > gui->screen_width) x= gui->screen_width- win_w;
  if (x<0) x=0;
  if ((y+ win_h) > gui->screen_height) y= gui->screen_height- win_h;
  if (y<0) y=0;
  XMoveWindow (dpy, win, x, y);
}

void
x_window_rep::set_size (SI w, SI h) {
  h=-h; decode (w, h);
  XResizeWindow (dpy, win, w, h);
}

void
x_window_rep::set_name (string name) {
  char* s= as_charp (name);
  XStoreName (dpy, win, s);
  XSetIconName (dpy, win, s);
  delete[] s;
  the_name= name;
}

string
x_window_rep::get_name () {
  return the_name;
}

void
x_window_rep::set_visibility (bool flag) {
  if (flag) XMapRaised (dpy, win);
  else XUnmapWindow (dpy, win);  
}

void
x_window_rep::set_full_screen (bool flag) {
  if (full_screen_flag == flag) return;
  string old_name= get_name ();
  if (old_name == "")
    old_name= as_string (name);
  if (flag) {
    save_win= win;
    name= NULL;
    save_x= win_x; save_y= win_y;
    save_w= win_w; save_h= win_h;
    initialize ();
    XMoveResizeWindow (dpy, win, 0, 0,
		       gui->screen_width, gui->screen_height);
    move_event   (0, 0);
    resize_event (gui->screen_width, gui->screen_height);
    set_visibility (true);
    XSetInputFocus (dpy, win, PointerRoot, CurrentTime);
  }
  else {
    set_visibility (false);
    Window_to_window->reset (win);
    nr_windows--;
    XDestroyWindow (dpy, win);
    win= save_win;
    set_visibility (false);
    Window_to_window->reset (win);
    nr_windows--;
    XDestroyWindow (dpy, win);
    name= as_charp (old_name);
    win_x= save_x; win_y= save_y;
    win_w= save_w; win_h= save_h;
    initialize ();
    set_visibility (true);
    XMoveResizeWindow (dpy, win, save_x, save_y, save_w, save_h);
    resize_event (save_w, save_h);
    move_event   (save_x, save_y);
  }
  set_name (old_name);
  full_screen_flag= flag;
}

void
x_window_rep::move_event (int x, int y) {
  bool flag= (win_x!=x) || (win_y!=y);
  win_x= x; win_y= y;
  if (flag) notify_position (w, win_x*PIXEL, win_y*PIXEL);
}

void
x_window_rep::resize_event (int ww, int hh) {
  bool flag= (win_w!=ww) || (win_h!=hh);
  win_w= ww; win_h= hh;
  if (flag) notify_size (w, win_w*PIXEL, win_h*PIXEL);
}

void
x_window_rep::destroy_event () {
  send_destroy (w);
}

/******************************************************************************
* Event handling
******************************************************************************/

void
x_window_rep::invalidate_event (int x1, int y1, int x2, int y2) {
  invalid_regions= invalid_regions | rectangles (rectangle (x1, y1, x2, y2));
}

void
x_window_rep::key_event (string key) {
  send_keyboard (kbd_focus, key);
}

void
x_window_rep::focus_in_event () {
  if (ic_ok) XSetICFocus (ic);
  has_focus= true;
  notify_keyboard_focus (kbd_focus, true);
  gui->focussed_window (win);
}

void
x_window_rep::focus_out_event () {
  if (ic_ok) XUnsetICFocus (ic);
  has_focus= false;
  notify_keyboard_focus (kbd_focus, false);
}

void
x_window_rep::mouse_event (string ev, int x, int y, time_t t) {
  if (nil (gui->grab_ptr) || (get_x_window (gui->grab_ptr->item) == NULL)) {
    set_origin (0, 0);
    encode (x, y);
    send_mouse (w, ev, x, y, gui->state, t);
  }
  else {
    x_window grab_win= get_x_window (gui->grab_ptr->item);
    if (this != grab_win) {
      x += win_x - grab_win->win_x;
      y += win_y - grab_win->win_y;
      // return;
    }
    set_origin (0, 0);
    encode (x, y);
    send_mouse (gui->grab_ptr->item, ev, x, y, gui->state, t);
  }
}

void
x_window_rep::repaint_invalid_regions () {
  //if (!nil (invalid_regions)) cout << invalid_regions << "\n";
  //else { cout << "."; cout.flush (); }
  rectangles new_regions;
  if (!nil (invalid_regions)) {
    rectangle lub= least_upper_bound (invalid_regions);
    if (area (lub) < 1.2 * area (invalid_regions))
      invalid_regions= rectangles (lub);
  }
  while (!nil (invalid_regions)) {
    set_origin (0, 0);
    rectangle r= copy (invalid_regions->item);
    encode (r->x1, r->y1);
    encode (r->x2, r->y2);
    x_drawable_rep::set_clipping (r->x1, r->y2, r->x2, r->y1);
    send_repaint (w, r->x1, r->y2, r->x2, r->y1);
    if (interrupted ())
      new_regions= rectangles (invalid_regions->item, new_regions);
    invalid_regions= invalid_regions->next;
  }
  invalid_regions= new_regions;
}

void
x_window_rep::set_keyboard_focus (widget wid, bool get_focus) {
  if (!get_focus)
    fatal_error ("Explicit loss of keyboard focus not yet implemented",
		 "x_window_rep::set_keyboard_focus");
  if (has_focus && (kbd_focus != wid.rep)) {
    notify_keyboard_focus (kbd_focus, false);
    notify_keyboard_focus (wid, true);
  }
  kbd_focus= wid.rep;
}

bool
x_window_rep::get_keyboard_focus (widget wid) {
  return has_focus && kbd_focus == wid.rep;
}

void
x_window_rep::set_mouse_grab (widget wid, bool get_grab) {
  if (get_grab) gui->obtain_mouse_grab (wid);
  else gui->release_mouse_grab ();
}

bool
x_window_rep::get_mouse_grab (widget w) {
  return gui->has_mouse_grab (w);
}

void
x_window_rep::set_mouse_pointer (widget wid, string name, string mask) {
  if (mask == "") gui->set_mouse_pointer (wid, name);
  else gui->set_mouse_pointer (wid, name, mask);
}

/******************************************************************************
* Delayed messages
******************************************************************************/

message_rep::message_rep (widget wid2, string s2, time_t t2):
  wid (wid2), s (s2), t (t2) {}
message::message (widget wid, string s, time_t t):
  rep (new message_rep (wid, s, t)) {}

ostream&
operator << (ostream& out, message m) {
  return out << "message " << m->s << " to " << m->wid
	     << "at time " << m->t << "\n";
}

static list<message>
insert_message (list<message> l, widget wid, string s, time_t cur, time_t t) {
  if (nil (l)) return list<message> (message (wid, s, t));
  time_t ref= l->item->t;
  if ((t-cur) <= (ref-cur)) return list<message> (message (wid, s, t), l);
  return list<message> (l->item, insert_message (l->next, wid, s, cur, t));
}

void
x_window_rep::delayed_message (widget wid, string s, time_t delay) {
  time_t ct= texmacs_time ();
  the_gui->messages= insert_message (the_gui->messages, wid, s, ct, ct+ delay);
}

/******************************************************************************
* Routines concerning regions in a window
******************************************************************************/

void
x_window_rep::translate (SI x1, SI y1, SI x2, SI y2, SI dx, SI dy) {
  SI X1= x1+ dx;
  SI Y2= y2+ dy;
  decode (x1, y1);
  decode (x2, y2);
  decode (X1, Y2);
  dx= X1- x1;
  dy= Y2- y2;

  XEvent report;
  while (XCheckWindowEvent (dpy, win, ExposureMask, &report))
    gui->process_event (this, &report);

  rectangles region (rectangle (x1, y2, x2, y1));
  rectangles invalid_intern= invalid_regions & region;
  rectangles invalid_extern= invalid_regions - invalid_intern;
  invalid_intern = ::translate (invalid_intern, dx, dy) & region;
  invalid_regions= invalid_extern | invalid_intern;

  if (x1<x2 && y2<y1)
    XCopyArea (dpy, win, win, gc, x1, y2, x2-x1, y1-y2, X1, Y2);
}

void
x_window_rep::invalidate (SI x1, SI y1, SI x2, SI y2) {
  outer_round (x1, y1, x2, y2);
  decode (x1, y1);
  decode (x2, y2);
  invalidate_event (x1, y2, x2, y1);
}

bool
x_window_rep::repainted () {
  return nil (invalid_regions);
}

/******************************************************************************
* Interface
******************************************************************************/

window
popup_window (widget w, string name, SI min_w, SI min_h,
	      SI def_w, SI def_h, SI max_w, SI max_h)
{
  char* _name= as_charp (name);
  window win= new x_window_rep (w, the_gui, NULL,
				min_w, min_h, def_w, def_h, max_w, max_h);
  delete[] _name;
  return win;
}

window
plain_window (widget w, string name, SI min_w, SI min_h,
	      SI def_w, SI def_h, SI max_w, SI max_h)
{
  char* _name= as_charp (name);
  window win= new x_window_rep (w, the_gui, _name,
				min_w, min_h, def_w, def_h, max_w, max_h);
  delete[] _name;
  return win;
}
