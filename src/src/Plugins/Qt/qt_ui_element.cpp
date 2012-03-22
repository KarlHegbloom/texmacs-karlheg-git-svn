/******************************************************************************
 * MODULE     : qt_ui_element.cpp
 * DESCRIPTION: User interface proxies
 * COPYRIGHT  : (C) 2010  Massimiliano Gubinelli
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#include "qt_ui_element.hpp"
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
#include "scheme.hpp"
//#import "TMView.h"
#include <QtGui>

#include "QTMWindow.hpp"
#include "QTMGuiHelper.hpp"
#include "qt_gui.hpp"
#include "qt_window_widget.hpp"
#include "qt_menu.hpp"

#include "../../Style/Evaluate/evaluate_main.hpp" // required for as_length(string)


const char *ui_type_string[]= {
  "horizontal_menu", "vertical_menu", "horizontal_list", "vertical_list",
  "tile_menu", "minibar_menu", "menu_separator", "menu_group", 
  "pulldown_button", "pullright_button", "menu_button",
  "balloon_widget", "text_widget", "xpm_widget", "toggle_widget",
  "enum_widget", "choice_widget", "scrollable_widget",
  "hsplit_widget", "vsplit_widget", 
  "aligned_widget", "tabs_widget", "wrapped_widget", "refresh_widget",
  "glue_widget"
};


/******************************************************************************
 * Auxiliary classes
 ******************************************************************************/

/*!
 * We use this class to properly initialize style options for our QWidgets
 * which have to blend into QMenus
 *   see #QTBUG-1993.
 *   see #QTBUG-7707.
 */
class QTMAuxMenu: public QMenu {
public:
  QTMAuxMenu (): QMenu() {}
  
  void myInitStyleOption (QStyleOptionMenuItem *option) const {
    QAction action (NULL);
    initStyleOption(option,&action);
  }
};


/*! QTMMenuButton is a custom button appropriate for menus
 *
 * We need to subclass QToolButton for two reasons
 *  1) custom appearence
 *  2) if used in QWidgetAction the menu do not disappear upon triggering the
 *     button. See QTBUG-10427.
 */
class QTMMenuButton: public QToolButton {
  QStyleOptionMenuItem option;
public:
  QTMMenuButton (QWidget* parent = 0): QToolButton(parent) {
    QTMAuxMenu m;
    m.myInitStyleOption (&option);
    setAttribute (Qt::WA_Hover);
  }  
  void mouseReleaseEvent (QMouseEvent *event);
  void mousePressEvent (QMouseEvent *event);
  void paintEvent (QPaintEvent *event);
};

void
QTMMenuButton::mousePressEvent (QMouseEvent* event) {
  // this one triggers the action and toggles the button
  QToolButton::mousePressEvent (event);
  // this one forwards the event to the parent
  // (which eventually is the menu)
  QWidget::mousePressEvent (event);
}

void
QTMMenuButton::mouseReleaseEvent (QMouseEvent* event) {
  // this one triggers the action and untoggles the button
  QToolButton::mouseReleaseEvent (event);
  // this one forwards the event to the parent
  // (which eventually is the menu which then close itself)
  QWidget::mouseReleaseEvent (event);
}

void
QTMMenuButton::paintEvent (QPaintEvent* event) {
  (void) event;
  QPainter p (this);
  
  // initialize the options
  QStyleOptionToolButton buttonOpt;
  initStyleOption (&buttonOpt);
  QRect r = rect ();
  option.rect = r;
  option.state = QStyle::State_Enabled |
  ( buttonOpt.state & QStyle::State_MouseOver ? 
   QStyle::State_Selected : QStyle::State_None ); 
  // draw the control background as a menu item
  style () -> drawControl (QStyle::CE_MenuItem, &option, &p, this); 
  // draw the icon with a bit of inset.
  r.adjust (2,2,-2,-2);
  defaultAction ()-> icon ().paint (&p, r);
}


class QTMMenuWidget: public QWidget {
  QStyleOptionMenuItem option;
public:
  QTMMenuWidget (QWidget* parent = 0): QWidget(parent) {
    QTMAuxMenu m;
    m.myInitStyleOption (&option);
  }
  void paintEvent(QPaintEvent *event);
};

void
QTMMenuWidget::paintEvent(QPaintEvent* event) {
  (void) event;
  QPainter p (this);
  option.rect = rect ();
  //QRect r = rect ();
  style()->drawControl (QStyle::CE_MenuEmptyArea, &option, &p, this);
  QWidget::paintEvent(event);
}


class QTMUIButton: public QToolButton {
public:
  QTMUIButton (QWidget* parent = 0): QToolButton(parent) {}
  void paintEvent(QPaintEvent *event);
};


void
QTMUIButton::paintEvent(QPaintEvent* event) {
  (void) event;
  QPainter p (this);
  defaultAction()->icon().paint (&p, rect ());
}


QTMWidgetAction::QTMWidgetAction (widget _wid, QObject *parent)
: QWidgetAction (parent), wid (_wid) { 
  QObject::connect (the_gui->gui_helper, SIGNAL(refresh()), this, SLOT(doRefresh()));
}

QTMWidgetAction::~QTMWidgetAction() {
}


void 
QTMWidgetAction::doRefresh() {
#if 0
  if (N(str)) {
    string t= tm_var_encode (str);
    if (t == "Help") t= "Help ";
    setText (to_qstring (t));
  }
#endif
}

QWidget * 
QTMWidgetAction::createWidget ( QWidget * parent ) {
  QWidget *qw = concrete(wid)->as_qwidget();
  qw->setParent(parent);
  return qw;
}


class QTMTileAction: public QWidgetAction {
  QVector <QAction*> actions;
  int cols;
public:
  QTMTileAction (QWidget* parent, array<widget>& arr, int _cols)
  : QWidgetAction (parent), cols (_cols)
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

/*!
 * FIXME: QTMTileAction::createWidget is called twice:
 * the first time when the action is added to the menu,
 * the second when from the menu it is transferred to the toolbar.
 * This is weird since the first widget does not ever use
 * the widget so it results in a waste of time.
 */
QWidget*
QTMTileAction::createWidget(QWidget* parent) {
  if (DEBUG_QT) 
    cout << "QTMTileAction::createWidget\n";
  QWidget* wid= new QTMMenuWidget (parent);
  QGridLayout* l= new QGridLayout (wid);
  // wid->setAutoFillBackground(true);
  // wid->setBackgroundRole(QPalette::Base);
  wid->setLayout (l);
  l->setSizeConstraint (QLayout::SetFixedSize);
  l->setHorizontalSpacing (2);
  l->setVerticalSpacing (2);
  l->setContentsMargins (4, 0, 4, 0);
  int row= 0, col= 0;
  for (int i=0; i < actions.count(); i++) {
    QAction* sa= actions[i];
    QToolButton* tb= new QTMMenuButton (wid);
    tb->setDefaultAction (sa);
    QObject::connect(tb, SIGNAL(released()), this, SLOT(trigger()));
    //  tb->setStyle (qtmstyle ());
    l->addWidget (tb, row, col);
    col++;
    if (col >= cols) { col = 0; row++; }
  }
  return wid;
}

class QTMMinibarAction : public QWidgetAction {
  QVector <QAction*> actions;
public:
  QTMMinibarAction (QWidget* parent, array<widget>& arr)
  : QWidgetAction (parent)
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

/*!
 * FIXME: QTMMinibarAction::createWidget is called twice:
 * the first time when the action is added to the menu,
 * the second when from the menu it is transferred to the toolbar.
 * This is weird since the first widget does not ever use
 * the widget so it results in a waste of time. 
 */
QWidget*
QTMMinibarAction::createWidget(QWidget* parent) {
  if (DEBUG_QT) cout << "QTMMinibarAction::createWidget\n";
  QWidget* wid= new QWidget (parent);
  QBoxLayout* l= new QBoxLayout (QBoxLayout::LeftToRight, wid);
  wid->setLayout (l);
  //  l->setSizeConstraint (QLayout::SetFixedSize);
  l->setContentsMargins (0, 0, 0, 0);
  l->setSpacing(0);
  for (int i=0; i < actions.count(); i++) {
    QAction* sa= actions[i];
    if (QWidgetAction * wa = qobject_cast<QWidgetAction*>(sa)) {
      QWidget *w = wa->requestWidget(wid);
      l->addWidget(w);
    } else if ((sa->text().isNull())&&(sa->icon().isNull())) {
      l->addSpacing(8);
    } else {
      QToolButton *tb = new QToolButton(wid);
      
      //HACK: texmacs does not use the checked state of the action
      // if the action is checkable then it means that it should be
      // checked
      sa->setChecked(sa->isCheckable());
      
      tb->setDefaultAction(sa);
      tb->setAutoRaise(true);
      tb->setPopupMode (QToolButton::InstantPopup);
      tb->setStyle(qtmstyle());
      //  tb->setIconSize(QSize(12,12));
      QFont f = tb->font();
      f.setPixelSize(10);
      tb->setFont(f);
      l->addWidget(tb);
    }
  }
  return wid;
}


/******************************************************************************
 * qt_plain_window_widget_rep
 ******************************************************************************/

/*! Models the simplest top-level window possible
 *
 * When any simple TeXmacs widgets needs promotion into a TeXmacs window, we
 * encapsulate it into a qt_plain_window_widget. This class handles the necessary
 * slots, in particular all those already handled by qt_view_widget_rep. It also
 * marks the enclosed QWidget as a "texmacs_window_widget" using QObject::setProperty().
 * This will be checked by qt_view_widget_rep::read()'s SLOT_WINDOW.
 *
 * qt_ui_element_rep::plain_window_widget should use this.
 */
class qt_plain_window_widget_rep: public qt_view_widget_rep {
  // QWidget* view;   // inherited
  command quit;
public:

  qt_plain_window_widget_rep (QWidget* w, command q) 
  : qt_view_widget_rep(w), quit(q) {
    ASSERT(w != NULL, "Null QWidgets cannot be promoted to windows");
    w->setProperty ("texmacs_window_widget", QVariant::fromValue ((void*) this));
    
    // We already do this before instantiating qt_plain_window_widget_rep...
    //QTMCommand* qcmd = new QTMCommand (quit);
    //qcmd->setParent (w);
    //QObject::connect (w, SIGNAL (destroyed()), qcmd, SLOT (apply()));
  };
  ~qt_plain_window_widget_rep () {};
  
  virtual void send (slot s, blackbox val);
};


void
qt_plain_window_widget_rep::send (slot s, blackbox val) {
  if (DEBUG_QT)
    cout << "qt_plain_widget_rep::send " << slot_name(s) << LF;
  switch (s) {
    case SLOT_POSITION:
    {
      TYPE_CHECK (type_box (val) == type_helper<coord2>::id);
      coord2 p= open_box<coord2> (val);
      if (view) {
        QPoint pt = to_qpoint (p);
        pt.ry() += 40;
        // to avoid window under menu bar on MAC when moving at (0,0)
        if (DEBUG_QT) 
          cout << "Moving to (" << pt.x() << "," 
          << pt.y() << ")" << LF;
        view->move (pt);
      }
    }
      break;
    case SLOT_VISIBILITY:
    {   
      check_type<bool> (val, "SLOT_VISIBILITY");
      bool flag = open_box<bool> (val);
      if (flag)
        view->show();
      else 
        view->hide();
    }   
      break;
    case SLOT_REFRESH:
      the_gui->gui_helper->emitTmSlotRefresh();
      break;  
    default:
      qt_view_widget_rep::send (s,val);
  }
}


/******************************************************************************
 * Ad-hoc command_rep derivates for different UI elements in qt_ui_element_rep
 ******************************************************************************/

/*! Ad-hoc command to be used to simulate keypresses
 * 
 * \sa qt_ui_element, , qt_ui_element_rep::as_qaction
 */

class qt_key_command_rep: public command_rep {
  string ks; 
  
public:
  qt_key_command_rep(string ks_) : ks(ks_) { }
  
  void apply () {
    if (N(ks)) { 
      QTMWidget *w = qobject_cast<QTMWidget*>(qApp->focusWidget());
      if (w && w->tm_widget()) {
        if (DEBUG_QT) cout << "shortcut: " << ks << LF;
        the_gui -> process_keypress (w->tm_widget(), ks, texmacs_time());
      }
    }
  }
  
  tm_ostream& print (tm_ostream& out) { return out << "qt_key_command_rep"; }
};


/*! Ad-hoc command to be used with toggle widgets.
 * The command associated with a qt_ui_element::toggle_widget has as a parameter the state
 * of the QCheckBox. Since it is assumed everywhere else that commands injected into
 * the gui's queue accept no parameters, and changes would be too big, we choose to
 * encapsulate the original command in a new one which will execute the first with 
 * its argument.
 * \sa qt_ui_element, , qt_ui_element_rep::as_qwidget, qt_ui_element_rep::toggle_widget
 */
class qt_toggle_command_rep: public command_rep {
  QPointer<QCheckBox> qwid;
  command cmd; 
  
public:
  qt_toggle_command_rep(QCheckBox* w, command c) : qwid(w), cmd(c) { }
  void apply () { if(qwid) cmd (list_object (object (qwid->isChecked()))); }

  tm_ostream& print (tm_ostream& out) { return out << "Toggle"; }
};

/*! Ad-hoc command to be used with enum widgets.
 * The command associated with a qt_ui_element::enum_widget has one parameter. For the
 * reason to be of this class, see \sa qt_toggle_command_rep .
 * \sa qt_ui_element, , qt_ui_element_rep::as_qwidget, qt_ui_element_rep::enum_widget
 */
class qt_enum_command_rep: public command_rep {
  QPointer<QComboBox> qwid;
  command cmd; 
  
public:
  qt_enum_command_rep(QComboBox* w, command c) : qwid(w), cmd(c) {}
  void apply () { 
    if (qwid)
      cmd (list_object (object (from_qstring(qwid->currentText()))));
  }
  
  tm_ostream& print (tm_ostream& out) { return out << "Enum"; }
};

/*! Ad-hoc command to be used with choice widgets.
 * The command associated with a qt_ui_element::choice_widget has one parameter. (a
 * list of selected items).
 * For the reason to be of this class, see \sa qt_toggle_command_rep.
 * \sa qt_ui_element, , qt_ui_element_rep::as_qwidget, qt_ui_element_rep::choice_widget
 */
class qt_choice_command_rep: public command_rep {
  QPointer<QListWidget> qwid;
  command cmd;
  bool multiple;  //<! Whether multiple choices are allowed in the widget.
  
public:
  qt_choice_command_rep(QListWidget* w, command c, bool m) : qwid(w), cmd(c), multiple(m) {}
  void apply () { 
    if (qwid) {
      QList<QListWidgetItem*> items = qwid->selectedItems();
      array<string> selected;
      for(int i = 0; i < items.size(); ++i)
        selected << from_qstring (items[i]->text());
      object l= null_object ();
      if(multiple)
        for (int i = N(selected)-1; i >= 0; --i)
          l= cons (selected[i], l);
      else if(N(selected)>0)  //Do not return a list with the item if only one
        l= selected[0];
      cmd (list_object (l));
    }
  }
  
  tm_ostream& print (tm_ostream& out) { return out << "Choice"; }
};


/******************************************************************************
 * qt_ui_element_rep
 ******************************************************************************/

qt_ui_element_rep::~qt_ui_element_rep()
{
  if (cachedAction) delete cachedAction;
}


widget 
qt_ui_element_rep::make_popup_widget () {
  return tm_new<qt_menu_rep>(as_qaction());
}


widget 
qt_ui_element_rep::popup_window_widget (string s)  {
  return concrete(make_popup_widget())->popup_window_widget(s);
}


QMenu *
qt_ui_element_rep::get_qmenu() {
  if (!cachedAction) {
    cachedAction = as_qaction();
  }
  return (cachedAction ? cachedAction->menu() : NULL);
}


widget 
qt_ui_element_rep::plain_window_widget (string s, command quit)  {
  QLayoutItem *li     = as_qlayoutitem();
  QTMPlainWindow* win = new QTMPlainWindow();
  win->setWindowTitle (to_qstring (s));  
  win->setLayout(li->layout());
  
  QTMCommand* qtmcmd = new QTMCommand(quit);
  qtmcmd->setParent(win);
  QObject::connect(win, SIGNAL(closed()), qtmcmd, SLOT(apply()));
  
  // FIXME: using qt_plain_window_widget causes a crash when closing "secondary"
  // windows. (example widget6 in menu-test.scm)
  return tm_new<qt_window_widget_rep>(win, quit);
}

qt_ui_element_rep::operator tree () {
  if (type == refresh_widget) {
    return tree (TUPLE, "refresh", open_box<string> (load));
  } else {
    return tree();
  }
}


QAction* 
qt_ui_element_rep::as_qaction () {
  switch (type) {
    case horizontal_menu:
    case vertical_menu:
    case horizontal_list:
    case vertical_list:
    {
      typedef array<widget> T;
      array<widget> arr = open_box<T> (load);
      
      // a horizontal menu made up of the widgets in a
      QAction* act= new QTMAction (NULL);
      act->setText("Menu");
      QMenu* m= new QMenu ();
      for (int i = 0; i < N(arr); i++) {
        if (is_nil (arr[i])) break;
        QAction* a= concrete (arr[i]) -> as_qaction ();
        m->addAction (a);
        a->setParent (m);
      }
      act->setMenu (m);
      return act;
    }
      break;
      
    case tile_menu: 
    {
      typedef pair<array<widget>, int> T;
      T x = open_box<T>(load);
      array<widget> a = x.x1;
      int cols = x.x2;
      
      // a menu rendered as a table of cols columns wide & made up of widgets in a
      QWidgetAction* act= new QTMTileAction (NULL, a, cols);
      return act;
    }
      break;
      
    case minibar_menu: 
    {
      typedef array<widget> T;
      array<widget> arr = open_box<T> (load);

      QWidgetAction* act= new QTMMinibarAction (NULL, arr);
      return act;
    }
      break;
      
    case menu_separator: 
    {
      typedef bool T;
      bool vertical = open_box<T> (load);
      // a horizontal or vertical menu separator
      (void) vertical;
      QAction* a= new QTMAction (NULL);
      a->setSeparator (true);
      return a;
    }
      break;
    case glue_widget:
    {
      QTMAction* a = new QTMAction();
      a->setEnabled(false);
      return a;
    }
      break;
    case menu_group: 
    {
      typedef pair<string, int> T;
      T x = open_box<T>(load);
      string name = x.x1;
      int style = x.x2;

      // a menu group; the name should be greyed and centered
      QAction* a= new QTMAction (NULL);
      a->setText(to_qstring(tm_var_encode ((name))));
      a->setEnabled (false);
      if (style == WIDGET_STYLE_MINI) {
        QFont f = a->font();
        f.setPointSize(10);
        a->setFont(f);
      }  
      return a;
    }
      break;
      
    case pulldown_button:
    case pullright_button:
    {
      typedef pair<widget, promise<widget> > T;
      T x = open_box<T>(load);
      widget w = x.x1;
      promise<widget> pw = x.x2;
      
      // a button w with a lazy pulldown menu pw
      QAction* a= concrete (w) -> as_qaction ();
      QTMLazyMenu* lm= new QTMLazyMenu (pw);
      QMenu *old_menu = a->menu();
      a->setMenu (lm);
      a->setEnabled(true);
      if (old_menu) {
        cout << "this should not happen\n";
        delete old_menu;
      }
      return a;
    }
      break;
      
    case menu_button:
    {
      typedef quintuple<widget, command, string, string, int> T;
      T x = open_box<T>(load);
      widget w = x.x1;
      command cmd = x.x2;
      string pre = x.x3;
      string ks = x.x4;
      int style = x.x5;
      
      // a command button with an optional prefix (o, * or v) and
      // keyboard shortcut; if ok does not hold, then the button is greyed
      bool ok= (style & WIDGET_STYLE_INERT) == 0;
      QAction* a= NULL;
      a= concrete(w)->as_qaction();
#ifdef Q_WS_MAC
      if (search_forwards (" ", ks) != -1) ks= "";
#endif
      QTMCommand* c;
      if (N(ks) > 0) {
        QKeySequence qks = to_qkeysequence (ks);
        if (DEBUG_QT)
          cout << "ks: " << ks << " " << qks.toString().toAscii().data() << "\n";
        a->setShortcut (qks);
        command key_cmd = tm_new<qt_key_command_rep>(ks);
        c= new QTMCommand (key_cmd);
      } else {
        c= new QTMCommand (cmd);
      }
      c->setParent (a);
        
      // NOTE: this used to be a Qt::QueuedConnection, but the slot would not
      // be called if in a contextual menu
      QObject::connect (a, SIGNAL (triggered ()), c, SLOT (apply ()));    
  
      // FIXME: implement complete prefix handling
      a->setEnabled (ok? true: false);
      
      bool check = (pre != "") || (style & WIDGET_STYLE_PRESSED);
      
      a->setCheckable (check? true: false);
      a->setChecked (check? true: false);
      if (pre == "v") {}
      else if (pre == "*") {}
      // [mi setOnStateImage:[NSImage imageNamed:@"TMStarMenuBullet"]];
      else if (pre == "o") {}
      return a;
    }
      break;
      
    case balloon_widget:
    {
      typedef pair<widget, widget> T;
      T x = open_box<T>(load);
      widget text = x.x1;
      widget help = x.x2;
      
      // given a button widget w, specify a help balloon which should be displayed
      // when the user leaves the mouse pointer on the button for a small while
      QAction* a= concrete(text)->as_qaction();
      {
        typedef quartet<string, int, color, bool> T1;
        T1 x = open_box<T1>(static_cast<qt_ui_element_rep*>(help.rep)->load);
        string str = x.x1;
        a->setToolTip (to_qstring (str));
        // HACK: force displaying of the tooltip (needed for items in the QMenuBar)
        QObject::connect(a, SIGNAL(hovered()), a, SLOT(showToolTip()));
      }
      return a;
    }
      break;
      
    case text_widget:
    {
      typedef quartet<string, int, color, bool> T;
      T x = open_box<T>(load);
      string str = x.x1;
      int style = x.x2;
      //color col = x.x3;
      //bool tsp = x.x4;
      
      // a text widget with a given color and transparency

      QTMAction* a= new QTMAction (NULL);
      string t= tm_var_encode (str);
      if (t == "Help") t= "Help "; // HACK to avoid MacOS autodetection of the Help menu?
      a->setText(to_qstring (t));
      a->str = str;
      if (style == WIDGET_STYLE_MINI) {
        QFont f = a->font();
        f.setPointSize(10);
        a->setFont(f);
      }
      return a;
    }
      break;
      
    case xpm_widget:
    {
      url image = open_box<url>(load);

      // return widget ();
      // a widget with an X pixmap icon
      QAction* a= new QTMAction (NULL);
      QPixmap* img= the_qt_renderer () -> xpm_image (image);
      QIcon icon (*img);
      a->setIcon (icon);
      return a;
    }
      break;
      
    default:
      ;
  }
  
  return NULL;
}


QLayoutItem *
qt_ui_element_rep::as_qlayoutitem () {
  //cout << "as_qlayoutitem " << ui_type_string[type] << LF;

  switch (type) {
    case horizontal_menu:
    case vertical_menu:
    case horizontal_list:
    case vertical_list:
    {
      typedef array<widget> T;
      array<widget> arr = open_box<T> (load);
      
      // a horizontal/vertical menu made up of the widgets in a
      QLayout *l;
      if ((type == horizontal_list) || (type==horizontal_menu))
        l =  new QHBoxLayout();
      else
        l =  new QVBoxLayout();

      for (int i = 0; i < N(arr); i++) {
        if (is_nil (arr[i])) break;
        QLayoutItem* li= concrete (arr[i]) -> as_qlayoutitem ();
        if (li) l->addItem(li); // ownership transferred
      }
      return l;
    }
      break;

    // a menu rendered as a table of 'cols' columns wide & made up of widgets in
    // the array 'a'
    case tile_menu: 
    {
      typedef pair<array<widget>, int> T;
      T x = open_box<T>(load);
      array<widget> a = x.x1;
      int cols = x.x2;
            
      QGridLayout* l= new QGridLayout ();
      l->setSizeConstraint (QLayout::SetFixedSize);
      l->setHorizontalSpacing (2);
      l->setVerticalSpacing (2);
      l->setContentsMargins (4, 0, 4, 0);
      int row= 0, col= 0;
      for (int i=0; i < N(a); i++) {
        QLayoutItem *li = concrete(a[i])->as_qlayoutitem();
        l->addItem(li, row, col);
        col++;
        if (col >= cols) { col = 0; row++; }
      }
      return l;
    }
      break;

    //  a table with two columns
    case aligned_widget:       
    {
      typedef quartet<SI, SI, SI, SI> T1;
      typedef triple<array<widget>, array<widget>, T1 > T;
      T x = open_box<T>(load);
      array<widget> lhs = x.x1;
      array<widget> rhs = x.x2;
      T1 y = x.x3;

      // FIXME: lpad and rpad ignored.
      SI hsep = y.x1; SI vsep = y.x2; SI lpad = y.x3; SI rpad = y.x4;
      
      if (N(lhs) != N(rhs)) FAILED("aligned_widget: N(lhs) != N(rhs) ");

      QGridLayout* l= new QGridLayout ();
      l->setSizeConstraint (QLayout::SetMinimumSize);
      l->setHorizontalSpacing (6+hsep/PIXEL);
      l->setVerticalSpacing (2+vsep/PIXEL);
      for (int i=0; i < N(lhs); i++) {
        QLayoutItem* lli = concrete(lhs[i])->as_qlayoutitem();
        QLayoutItem* rli = concrete(rhs[i])->as_qlayoutitem();
        if (lli) l->addItem(lli, i, 0, 1, 1, Qt::AlignRight);
        if (rli) l->addItem(rli, i, 1, 1, 1, Qt::AlignLeft);
      }
      return l;
    }
      break;
      
      
    case minibar_menu: 
    {
      typedef array<widget> T;
      array<widget> arr = open_box<T> (load);
      QBoxLayout* l= new QBoxLayout (QBoxLayout::LeftToRight);
      l->setContentsMargins (0, 0, 0, 0);
      l->setSpacing(0);
      for (int i=0; i < N(arr); i++) {
        QLayoutItem *li = concrete(arr[i])->as_qlayoutitem();
        l->addItem(li);
      }
      return l;
    }
      break;
      
    case menu_separator: 
    {
      typedef bool T;
      bool vertical = open_box<T> (load);
      QSizePolicy::Policy hpolicy = vertical ? QSizePolicy::Fixed 
                                             : QSizePolicy::Preferred;
      QSizePolicy::Policy vpolicy = vertical ? QSizePolicy::Preferred 
                                             : QSizePolicy::Fixed;
      return new QSpacerItem (1, 1, hpolicy, vpolicy);
    }
      break;
      
    case menu_group: 
    {
      typedef pair<string, int> T;
      T x = open_box<T>(load);
      string name = x.x1;
      int style = x.x2;
      
      (void) style;
      // a menu group; the name should be greyed and centered
      return NULL;
    }
      break;
      
    case pulldown_button:
    case pullright_button:
    case menu_button:
    case text_widget:
    case xpm_widget:
    case toggle_widget:
    case enum_widget:
    case choice_widget:
    case scrollable_widget:
    case hsplit_widget:
    case vsplit_widget:
    case tabs_widget:
    case wrapped_widget:
    case refresh_widget:
    {
      QWidget *w = this->as_qwidget();
      return new QWidgetItem(w);
    }
      break;
      
    case balloon_widget:
    {
      typedef pair<widget, widget> T;
      T x = open_box<T>(load);
      widget text = x.x1;
      widget help = x.x2;
      
      // given a button widget w, specify a help balloon which should be displayed
      // when the user leaves the mouse pointer on the button for a small while
      QLayoutItem* li= concrete(text)->as_qlayoutitem();
      if (li->widget())
      {
        typedef quartet<string, int, color, bool> T;
        T x = open_box<T>(static_cast<qt_ui_element_rep*>(help.rep)->load);
        string str = x.x1;
        li->widget()->setToolTip (to_qstring (str));
      }
      return li;
    }
      break;

    case glue_widget:
    {
      typedef quartet<bool, bool, SI, SI> T;
      T x = open_box<T>(load);
      QSizePolicy::Policy hpolicy = x.x1 ? QSizePolicy::Preferred 
                                         : QSizePolicy::Fixed;
      QSizePolicy::Policy vpolicy = x.x2 ? QSizePolicy::Preferred
                                         : QSizePolicy::Fixed;
      return new QSpacerItem (x.x3, x.x4, hpolicy, vpolicy);
    }
      break;
    default:
      ;
  }
  
  return NULL;
}


QWidget *
qt_ui_element_rep::as_qwidget () {
  //cout << "as_qwidget " << ui_type_string[type] << LF;

  switch (type) {
    case horizontal_menu:
    case vertical_menu:
    case horizontal_list:
    case vertical_list:
    case tile_menu: 
    case minibar_menu: 
    case aligned_widget: 
    {
      QLayoutItem *li = this->as_qlayoutitem();
      QWidget *w = new QWidget();
      if (QLayout *l = li->layout()) {
        // note that the QLayout is the same object as the QLayoutItem 
        // so no need to free li
        w->setLayout(l);
      } else {
        cout << "qt_ui_element_rep::as_qwidget : invalid situation" << LF;
      }
      return w;
    }
      break;
      
    case menu_separator: 
    case menu_group:
    case glue_widget:
    {
      return NULL;
    }
      break;
      
    case pulldown_button:
    case pullright_button:
    {
      typedef pair<widget, promise<widget> > T;
      T x = open_box<T>(load);
      widget w = x.x1;
      promise<widget> pw = x.x2;
      
      // a button w with a lazy pulldown menu pw
      
      QAction* a= concrete (this) -> as_qaction ();
      QToolButton *b = new QTMUIButton();
      a->setParent(b);
      b->setDefaultAction(a);
      return b;
    }
      break;
      
    case menu_button:
    {
      typedef quintuple<widget, command, string, string, int> T;
      T x = open_box<T>(load);
      widget w = x.x1;
      command cmd = x.x2;
      string pre = x.x3;
      string ks = x.x4;
      int style = x.x5;
      
      
      // a command button with an optional prefix (o, * or v) and
      // keyboard shortcut; if ok does not hold, then the button is greyed
      QAction* a= concrete(this)->as_qaction();
      QToolButton *b = (style & WIDGET_STYLE_BUTTON) ? new QToolButton() : new QTMUIButton();
      b->setDefaultAction(a);
      a->setParent(b);
      return b;
    }
      break;
      
    case balloon_widget:
    {
      typedef pair<widget, widget> T;
      T x = open_box<T>(load);
      widget text = x.x1;
      widget help = x.x2;
      
      // given a button widget w, specify a help balloon which should be displayed
      // when the user leaves the mouse pointer on the button for a small while
      QWidget* w= concrete(text)->as_qwidget();
      if (w)
      {
        typedef quartet<string, int, color, bool> T1;
        T1 x = open_box<T1>(static_cast<qt_ui_element_rep*>(help.rep)->load);
        string str = x.x1;
        w->setToolTip (to_qstring (str));
      }
      return w;
    }
      break;
      
    case text_widget:
    {
      typedef quartet<string, int, color, bool> T;
      T x = open_box<T>(load);
      string str    = x.x1;
      QString style = to_qstylesheet(x.x2, x.x3);
      
      // a text widget with a given color and transparency
      QLabel *w = new QLabel();
#if 0
      //FIXME: implement refresh when changing language
      QTMAction* a= new QTMAction (NULL);
      //a->str = str;
#endif
      string t= tm_var_encode (str);
      if (t == "Help") t= "Help ";
      //w->setTextFormat(Qt::RichText);
      w->setText(to_qstring (t));
      w->setStyleSheet(style);
      return w;
    }
      break;
      
    case xpm_widget:
    {
      url image = open_box<url>(load);
      
      // return widget ();
      // a widget with an X pixmap icon
      QLabel* l= new QLabel (NULL);
      QPixmap* img= the_qt_renderer () -> xpm_image (image);
      QIcon icon (*img);
      l->setPixmap (*img);
      return l;
    }
      break;

    case toggle_widget:
    { 
      typedef triple<command, bool, int > T;
      T x = open_box<T>(load);
      command cmd = x.x1;
      bool check  = x.x2;
      QString style = to_qstylesheet(x.x3);
      
      QCheckBox* w  = new QCheckBox (NULL);  
      w->setCheckState(check ? Qt::Checked : Qt::Unchecked);
      w->setStyleSheet(style);
      command tcmd = tm_new<qt_toggle_command_rep> (w, cmd);
      QTMCommand* c = new QTMCommand (tcmd);
      c->setParent (w);
      QObject::connect (w, SIGNAL (stateChanged(int)), c, SLOT (apply()));

      return w;
    }
      break;
      
    case enum_widget:
    {
      typedef quintuple<command, array<string>, string, int, string> T;
      T x = open_box<T>(load);
      command cmd        = x.x1;
      QStringList values = to_qstringlist(x.x2);
      QString value      = to_qstring(x.x3);
      QString style      = to_qstylesheet(x.x4);
            
      QComboBox* w = new QComboBox(NULL);
      w->setEditable(value.isEmpty() || values.last().isEmpty());  // weird convention?!
      if (values.last().isEmpty())
        values.removeLast();
      
      w->addItems(values);
      int index = w->findText(value, Qt::MatchFixedString | Qt::MatchCaseSensitive);
      if (index != -1)
        w->setCurrentIndex(index);
   
      // The QComboBox must be already filled to calculate relative widths
      QSize size= qt_decode_length(x.x5, w);
      w->setMinimumSize(size);
      // QComboBox::AdjustToContentsOnFirstShow would fix the size. Better?
      w->setSizeAdjustPolicy(QComboBox::AdjustToContents);
      w->setStyleSheet(style);
      
      command ecmd = tm_new<qt_enum_command_rep> (w, cmd);
      QTMCommand* c = new QTMCommand (ecmd);
      c->setParent (w);
      // NOTE: with QueuedConnections, the slots are sometimes not invoked.
      QObject::connect (w, SIGNAL (currentIndexChanged(int)), c, SLOT (apply()));
      
      return w;
    }
      break;
      
    case choice_widget:
    {
      typedef quartet<command, array<string>, array<string>, bool > T;
      T x = open_box<T>(load);
      command cmd = x.x1;
      QStringList items  = to_qstringlist(x.x2);
      QStringList chosen = to_qstringlist(x.x3);
      bool multiple_sel  = x.x4;
      
      QListWidget* w = new QListWidget();
      w->addItems(items);

      if (multiple_sel)
        w->setSelectionMode(QAbstractItemView::ExtendedSelection);  // Support CTRL and SHIFT multiple selections.
      else
        w->setSelectionMode(QAbstractItemView::SingleSelection);
      
      for (int i = 0; i < chosen.size(); ++i) {
        QListWidgetItem* item = w->item(i);
        item->setSelected(chosen.contains(item->text(), Qt::CaseSensitive));  // Qt::CaseSensitive is the default anyway
      }
      
      command ecmd = tm_new<qt_choice_command_rep> (w, cmd, multiple_sel);
      QTMCommand* qcmd = new QTMCommand (ecmd);
      qcmd->setParent (w);
      QObject::connect (w, SIGNAL (itemSelectionChanged()), qcmd, SLOT (apply()));//, Qt::QueuedConnection);
      
      return w;      
    }
      break;
      
    case scrollable_widget:
    {
      typedef pair<widget, int> T;
      T x = open_box<T>(load);
      widget wid    = x.x1;
      QString style = to_qstylesheet(x.x2);
      
      QScrollArea* scroll = new QScrollArea();
      scroll->setBackgroundRole(QPalette::NoRole);
      scroll->setStyleSheet(style);
      QWidget* w = concrete(wid)->as_qwidget();
      scroll->setWidget(w);
    
      // FIXME????
      // "Note that You must add the layout of widget before you call this function; 
      //  if you add it later, the widget will not be visible - regardless of when you show() the scroll area.
      //  In this case, you can also not show() the widget later."
      return scroll;
    }
      break;
      
    case hsplit_widget:
    case vsplit_widget:
    {
      typedef pair<widget, widget> T;
      T x = open_box<T>(load);
      widget w1 = x.x1;
      widget w2 = x.x2;
      
      QWidget* qw1 = concrete(w1)->as_qwidget();
      QWidget* qw2 = concrete(w2)->as_qwidget();
      QSplitter* split = new QSplitter();
      split->setOrientation(type == hsplit_widget ? Qt::Horizontal : Qt::Vertical);
      split->addWidget(qw1);
      split->addWidget(qw2);
      
      return split;
    }
      break;

    case tabs_widget:
    {
      typedef pair<array<widget>, array<widget> > T;
      T x = open_box<T>(load);
      array<widget> tabs = x.x1;
      array<widget> bodies = x.x2;
      
      QTMTabWidget* tw = new QTMTabWidget ();
      
      int i;
      for (i = 0; i < N(tabs); i++) {
        if (is_nil (tabs[i])) break;
        QWidget* prelabel = concrete (tabs[i]) -> as_qwidget();
        QLabel* label = qobject_cast<QLabel*> (prelabel);
        QWidget* body = concrete (bodies[i]) -> as_qwidget();
        tw->addTab(body, label ? label->text() : "");
        delete prelabel;
      }
      if (i>0) tw->resizeOthers(0);   // Force the automatic resizing

      return tw;
    }

    case wrapped_widget:
    {
      typedef pair<widget, command> T;
      T x = open_box<T>(load);
      widget w = x.x1;
      command cmd = x.x2;
      
      QWidget* qw = concrete(w)->as_qwidget();
      QTMCommand* c = new QTMCommand (cmd);
      c->setParent (qw);
      QObject::connect (qw, SIGNAL (destroyed()), c, SLOT (apply()));//, Qt::QueuedConnection);
      
      return qw;
    }
      break;
    case refresh_widget:
    {
      string tmwid = open_box<string> (load);
      QWidget* rw  = new QTMRefreshWidget (tmwid);
      return rw;
    }
      break;
    default:
      ;
  }
  
  return NULL;
}


/******************************************************************************
* TeXmacs interface
******************************************************************************/

widget horizontal_menu (array<widget> arr) { 
  return qt_ui_element_rep::create (qt_ui_element_rep::horizontal_menu, arr); }
widget vertical_menu (array<widget> arr)  { 
  return qt_ui_element_rep::create (qt_ui_element_rep::vertical_menu, arr); }
widget horizontal_list (array<widget> arr) { 
  return qt_ui_element_rep::create (qt_ui_element_rep::horizontal_list, arr); }
widget vertical_list (array<widget> arr) { 
  return qt_ui_element_rep::create (qt_ui_element_rep::vertical_list, arr); }
widget aligned_widget (array<widget> lhs, array<widget> rhs, SI hsep, SI vsep, SI lpad, SI rpad) { 
  typedef quartet<SI, SI, SI, SI> T1;
  typedef triple<array<widget>, array<widget>, T1> T;
  return tm_new <qt_ui_element_rep> (qt_ui_element_rep::aligned_widget, 
                                     close_box (T (lhs,rhs, T1 (hsep, vsep, lpad, rpad)))); 
}
widget tabs_widget (array<widget> tabs, array<widget> bodies) {
  return qt_ui_element_rep::create (qt_ui_element_rep::tabs_widget, tabs, bodies); }
widget wrapped_widget (widget w, command cmd) {
  return qt_ui_element_rep::create (qt_ui_element_rep::wrapped_widget, w, cmd); }
widget tile_menu (array<widget> a, int cols) { 
  return qt_ui_element_rep::create (qt_ui_element_rep::tile_menu, a, cols); }
widget minibar_menu (array<widget> arr) { 
  return qt_ui_element_rep::create (qt_ui_element_rep::minibar_menu, arr); }
widget menu_separator (bool vertical) { 
  return qt_ui_element_rep::create (qt_ui_element_rep::menu_separator, vertical); }
widget menu_group (string name, int style) { 
  return qt_ui_element_rep::create (qt_ui_element_rep::menu_group , name, style); }
widget pulldown_button (widget w, promise<widget> pw) { 
  return qt_ui_element_rep::create (qt_ui_element_rep::pulldown_button, w, pw); }
widget pullright_button (widget w, promise<widget> pw) { 
  return qt_ui_element_rep::create (qt_ui_element_rep::pullright_button, w, pw); }
widget menu_button (widget w, command cmd, string pre, string ks, int style) { 
  return qt_ui_element_rep::create (qt_ui_element_rep::menu_button, w, cmd, pre, ks, style); }
widget balloon_widget (widget w, widget help) { 
  return qt_ui_element_rep::create (qt_ui_element_rep::balloon_widget, w, help); }
widget text_widget (string s, int style, color col, bool tsp) { 
  return qt_ui_element_rep::create (qt_ui_element_rep::text_widget, s, style, col, tsp); }
widget xpm_widget (url file_name) { 
  return qt_ui_element_rep::create (qt_ui_element_rep::xpm_widget, file_name); }
widget toggle_widget (command cmd, bool on, int style) { 
  return qt_ui_element_rep::create (qt_ui_element_rep::toggle_widget, cmd, on, style); }
widget enum_widget (command cmd, array<string> vals, string val, int style, string width) { 
  return qt_ui_element_rep::create (qt_ui_element_rep::enum_widget, cmd, vals, val, style, width); }
widget choice_widget (command cmd, array<string> vals, array<string> chosen) { 
  return qt_ui_element_rep::create(qt_ui_element_rep::choice_widget, cmd, vals, chosen, true); }
widget choice_widget (command cmd, array<string> vals, string cur) {
  array<string> chosen (1);
  chosen[0]= cur;
  return qt_ui_element_rep::create(qt_ui_element_rep::choice_widget, cmd, vals, chosen, false); }
widget user_canvas_widget (widget wid, int style) { 
  return qt_ui_element_rep::create(qt_ui_element_rep::scrollable_widget, wid, style); }
widget resize_widget (widget w, int style, string w1, string h1,
                      string w2, string h2, string w3, string h3) {
  (void) w; (void) style; (void) w1; (void) h1; (void) w2; (void) h2; 
  (void) w3; (void) h3;
  //FIXME: add a meaningul semantics
  return w;
}
widget hsplit_widget (widget l, widget r) { 
  return qt_ui_element_rep::create(qt_ui_element_rep::hsplit_widget, l, r); }
widget vsplit_widget (widget t, widget b) { 
  return qt_ui_element_rep::create(qt_ui_element_rep::vsplit_widget, t, b); }
widget refresh_widget (string tmwid) {
  return qt_ui_element_rep::create(qt_ui_element_rep::refresh_widget, tmwid); }
widget ink_widget (command cb) {
  (void) cb;
  FAILED ("not yet implemented"); }

