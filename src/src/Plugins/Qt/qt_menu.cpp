
/******************************************************************************
* MODULE     : qt_menu.h
* DESCRIPTION: QT menu proxies
* COPYRIGHT  : (C) 2007  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "qt_menu.hpp"
#include "qt_utilities.hpp"
#include "qt_renderer.hpp"
#include "qt_simple_widget.hpp"
#include "qt_basic_widgets.hpp"
#include "QTMMenuHelper.hpp"
#include "QTMStyle.hpp"
#include "analyze.hpp" 
#include "widget.hpp" 
#include "message.hpp"
#include "promise.hpp"
//#import "TMView.h"
#include <QtGlobal>
#include <QPointer>
#include <QBitmap>
#include <QGridLayout>
#include <QToolButton>
#include <QWidgetAction>
#include <QEvent>
#include <QStyleOptionMenuItem>
#include <QKeySequence>

extern char  *slot_name(slot s); // from qt_widget.cpp

class qt_menu_rep: public qt_widget_rep {
public:
  bool flag;
  QPointer<QAction> item;
  qt_menu_rep (QAction* _item): flag (false), item (_item) {}
  ~qt_menu_rep () {}

  virtual void send (slot s, blackbox val);
  virtual widget make_popup_widget (); 
  virtual widget popup_window_widget (string s); 
  virtual QAction* as_qaction ();
};

QAction*
qt_menu_rep::as_qaction() { 
  // FIXME: the convention is that as_qaction give ownership of
  // the action to the caller. However in this case we do not want
  // to replicate the action so we must be sure to be called only once.
  if (flag) cout << "THIS MUST NOT HAPPEN (CALLED TWICE)!!\n"; 
  flag = true; 
  return item;
}

widget
qt_menu_rep::make_popup_widget () {
  return this;
}

widget
qt_menu_rep::popup_window_widget (string s) {
  item->menu()->setWindowTitle (to_qstring (s));
  return this;
}

void
qt_menu_rep::send (slot s, blackbox val) {
  if (DEBUG_EVENTS)
    cout << "qt_menu_rep::send " << slot_name(s) << LF;
  switch (s) {
  case SLOT_POSITION:
    if (type_box (val) != type_helper<coord2>::id)
      fatal_error ("type mismatch", "SLOT_POSITION");
    break;
  case SLOT_VISIBILITY:
    {	
      check_type<bool> (val, "SLOT_VISIBILITY");
      bool flag = open_box<bool> (val);
      (void) flag;
    }	
    break;
  case SLOT_MOUSE_GRAB:
    {	
      check_type<bool> (val, "SLOT_MOUSE_GRAB");
      bool flag = open_box<bool> (val);
      (void) flag;
      // [NSMenu popUpContextMenu:[item submenu] withEvent:[NSApp currentEvent] forView:( (qt_view_widget_rep*)(the_keyboard_focus.rep))->view ];
      if (item->menu ())
	     item->menu()->exec (QCursor::pos ());
    }	
    break;
  default:
    fatal_error ("cannot handle slot type", "qt_menu_rep::send");
  }
}

/******************************************************************************
* Widgets for the construction of menus
******************************************************************************/

widget
horizontal_menu (array<widget> arr) {
  // a horizontal menu made up of the widgets in a
  QAction* act= new QAction ("Menu", NULL);
  QMenu* m= new QMenu ();
  for (int i = 0; i < N(arr); i++) {
    if (is_nil (arr[i])) break;
    QAction* a= concrete (arr[i]) -> as_qaction ();
    m->addAction (a);
    a->setParent (m);
  }
  act->setMenu (m);
  return new qt_menu_rep (act);	
}

widget
vertical_menu (array<widget> a) {
  // a vertical menu made up of the widgets in a
  return horizontal_menu (a);
}

#if 0
class QTMAuxMenu: public QMenu {
public:
  QTMAuxMenu (): QMenu() {}
  void myInitStyleOption (QStyleOptionMenuItem *option, const QAction *action) const {
    initStyleOption(option,action);
  }
};
#endif

class QTMToolButton: public QToolButton {
public:
  QTMToolButton (QWidget* parent = 0): QToolButton(parent) {}  
  void mouseReleaseEvent(QMouseEvent *event);
  void mousePressEvent(QMouseEvent *event);
  void paintEvent(QPaintEvent *event);
};

void
QTMToolButton::mousePressEvent (QMouseEvent* event) {
  // this one triggers the action and toggles the button
  QToolButton::mousePressEvent (event);
  // this one forwards the event to the parent
  // (which eventually is the menu)
  QWidget::mousePressEvent (event);  
}

void
QTMToolButton::mouseReleaseEvent (QMouseEvent* event) {
  // this one triggers the action and untoggles the button
  QToolButton::mouseReleaseEvent (event);
  // this one forwards the event to the parent 
  // (which eventually is the menu which then close itself)
  QWidget::mouseReleaseEvent (event);
}

void
QTMToolButton::paintEvent(QPaintEvent* event) {
  (void) event;
  QPainter p (this);
#if 0
  QStyleOptionMenuItem option;
  QAction *action = defaultAction ();
  QTMAuxMenu m;
  m.myInitStyleOption (&option, action);
  option.rect = rect ();
  QRect r = rect ();
  style()->drawControl (QStyle::CE_MenuItem, &option, &p, this);
#else
  defaultAction()->icon().paint (&p, rect ());
#endif
}

class QTMTileAction: public QWidgetAction {
  QVector <QAction*> actions;
  int cols;
public:
  QTMTileAction (QWidget* parent, array<widget>& arr, int _cols):
    QWidgetAction (parent), cols (_cols)
  {
    actions.reserve(N(arr));
    for(int i = 0; i < N(arr); i++) {
      if (is_nil(arr[i])) break;
      QAction *act = concrete(arr[i])->as_qaction();
      act->setParent(this);
      actions.append(act);
    };      
  }
  QWidget* createWidget(QWidget* parent);
  // virtual void activate (ActionEvent event) {
  //   cout << "TRIG\n"; QWidgetAction::activate (event); } 
};

// FIXME: QTMTileAction::createWidget is called twice:
// the first time when the action is added to the menu,
// the second when from the menu it is transferred to the toolbar.
// This is weird since the first widget does not ever use
// the widget so it results in a waste of time.

QWidget*
QTMTileAction::createWidget(QWidget* parent) {
  if (DEBUG_EVENTS) cout << "QTMTileAction::createWidget\n";
  QWidget* wid= new QWidget (parent);
  QGridLayout* l= new QGridLayout (wid);
  wid->setLayout (l);
  l->setSizeConstraint (QLayout::SetFixedSize);
  l->setHorizontalSpacing (0);
  l->setVerticalSpacing (0);
  l->setContentsMargins (0, 0, 0, 0);
  int row= 0, col= 0;
  for (int i=0; i < actions.count(); i++) {
    QAction* sa= actions[i];
    QToolButton* tb= new QTMToolButton (wid);
    tb->setDefaultAction (sa);
    l->addWidget (tb, row, col);
    col++;
    if (col >= cols) { col = 0; row++; }
  }
  return wid;
}

widget
tile_menu (array<widget> a, int cols) { 
  // a menu rendered as a table of cols columns wide & made up of widgets in a
  (void) cols; 
#if 1
  QWidgetAction* act= new QTMTileAction (NULL, a, cols);  
  return new qt_menu_rep (act);
#else
  return horizontal_menu (a); 
#endif
}

widget
menu_separator (bool vertical) {
  // a horizontal or vertical menu separator
  (void) vertical;
  QAction* a= new QAction (NULL);
  a->setSeparator (true);
  return new qt_menu_rep (a); 
}

widget
menu_group (string name, string lan) {
  // a menu group; the name should be greyed and centered
  (void) lan;
  QAction* a= new QAction (to_qstring (name), NULL);
  a->setEnabled (false);
  return new qt_menu_rep (a);
}

widget
pulldown_button (widget w, promise<widget> pw) {
  // a button w with a lazy pulldown menu pw
  QAction* a= concrete (w) -> as_qaction ();
  QTMLazyMenu* lm= new QTMLazyMenu (pw.rep);
  a->setMenu (lm);
  return new qt_menu_rep (a);
}

widget
pullright_button (widget w, promise<widget> pw) {
  // a button w with a lazy pullright menu pw
  return pulldown_button (w, pw);
}

QAction*
qt_text_widget_rep::as_qaction () {
  return new QAction (to_qstring_utf8 (str), NULL);
}

QAction*
qt_image_widget_rep::as_qaction () {
  QAction* a= new QAction (NULL);
  QPixmap* img= the_qt_renderer () -> xpm_image (image);
  QIcon icon (*img);
  a->setIcon (icon);  
  return a;
}

QAction*
qt_balloon_widget_rep::as_qaction() {
  QAction* a= concrete(text)->as_qaction();
  a->setToolTip (to_qstring (((qt_text_widget_rep*) hint.rep) -> str));
  return a;
}

string
conv (const string ks) {
  string r(ks);
  r = replace (r, "C-", "Ctrl+");
  r = replace (r, "M-", "Meta+");
  r = replace (r, "A-", "Alt+");
  r = replace (r, "S-", "Shift+");
  r = replace (r, " ", ",");
  return r;
}

widget
menu_button (widget w, command cmd, string pre, string ks, bool ok) {
  // a command button with an optional prefix (o, * or v) and
  // keyboard shortcut; if ok does not hold, then the button is greyed
  QAction* a= NULL;
  a= concrete(w)->as_qaction();
  QTMCommand* c= new QTMCommand (cmd.rep);
  c->setParent (a);
  QObject::connect (a, SIGNAL (triggered ()), c, SLOT (apply ()),
		    Qt::QueuedConnection);
  if (N(ks) > 0) {
    string qtks = conv (ks);
    QKeySequence qks (to_qstring (qtks));
    if (DEBUG_EVENTS)
      cout << "ks: " << ks << " " << qks.toString().toAscii().data() << "\n";
    a->setShortcut (qks);
  }
  // FIXME: implement complete prefix handling
  a->setEnabled (ok? true: false);
  a->setCheckable (pre != ""? true: false);
  a->setChecked (pre != ""? true: false);
  if (pre == "v") {}
  else if (pre == "*") {}
    // [mi setOnStateImage:[NSImage imageNamed:@"TMStarMenuBullet"]];
  else if (pre == "o") {}
  return new qt_menu_rep(a);
}

widget
balloon_widget (widget w, widget help)  { 
  // given a button widget w, specify a help balloon which should be displayed
  // when the user leaves the mouse pointer on the button for a small while
  return new qt_balloon_widget_rep (w, help);
}

widget
text_widget (string s, color col, bool tsp, string lan) {
  // a text widget with a given color, transparency and language
  string t= qt_translate (s);
  if (t == "Help") t= "Help ";
  return new qt_text_widget_rep (t, col, tsp, lan);
}

widget
xpm_widget (url file_name) {
  // return widget ();
  // a widget with an X pixmap icon
  return new qt_image_widget_rep(file_name);
}

QMenu*
to_qmenu(widget w) {
  QAction *a = concrete(w)->as_qaction();
  QMenu *m = a->menu(); // the menu has no parent
  delete a;
  return m;
}

QAction*
to_qaction(widget w) {
  return concrete(w)->as_qaction();
}

QPixmap
impress (simple_widget_rep* wid) {
  if (wid) {
    int width, height;
    wid->handle_get_size_hint (width, height);
    QSize s = QSize (width/PIXEL, height/PIXEL);
    QPixmap pxm(s);
    QRect rect = QRect (0, 0, s.width(), s.height());
    //cout << "impress (" << s.width() << "," << s.height() << ")\n";
    pxm.fill (Qt::transparent);
    the_qt_renderer()->begin (&pxm);
    the_qt_renderer()->set_clipping
      (rect.x() * PIXEL, -(rect.y() + rect.height()) * PIXEL, 
       (rect.x() + rect.width()) * PIXEL, -rect.y() * PIXEL);
    wid->handle_repaint
      (rect.x() * PIXEL, -(rect.y() + rect.height()) * PIXEL,
       (rect.x() + rect.width()) * PIXEL, -rect.y() * PIXEL);
    the_qt_renderer()->end();
    return pxm;
  }
  else {
    // return arbitrary image...
    QPixmap pxm (10, 10);
    return pxm;
  }
}

QAction*
simple_widget_rep::as_qaction () {
  QAction* a= new QAction (NULL);
  QPixmap pxm (impress (this));
  QIcon icon (pxm);
  a->setIcon (icon);
  return a;
}

void
QTMLazyMenu::force () {
  if (!forced) {
    widget w= pm->eval ();
    qt_menu_rep* wid= (qt_menu_rep*) (w.rep); 
    QMenu* menu2= wid->item->menu ();
    replaceActions (this, menu2);
    delete (wid->item);
    wid->item= NULL;
    pm= NULL;
    forced= true;
  }
}
