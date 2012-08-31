
/******************************************************************************
* MODULE     : QTMMenuHelper.hpp
* DESCRIPTION: QT Texmacs menu helper classes
* COPYRIGHT  : (C) 2008 Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef QTMMENUHELPER_HPP
#define QTMMENUHELPER_HPP

#include "message.hpp"
#include "promise.hpp"
#include <QObject>
#include <QAction>
#include <QMenu>
#include <QWidgetAction>
#include <QLineEdit>
#include <QTabWidget>
#include <QLayout>
#include <QLineEdit>

#include "qt_gui.hpp"
#include "qt_dialogues.hpp"


/*! Handles TeXmacs commands in the QT way.

 Most TeXmacs widgets accept one command_rep as an argument. This is a scheme
 closure which will usually  be executed when the widget is closed, in the
 case of dialogs, or activated in the case of checkboxes, combo boxes, etc.
 This means connecting them to signals emmitted by our QWidgets and that's the
 purpose of this wrapping class. Furthermore, this commands must be processed
 in a separate queue. The slot apply() takes care of that.

 To use this class, one typically takes some given command "cmd" and does the
 following:
 
    QTMCommand* qtmcmd = new QTMCommand(theQWidget, cmd);
    QObject::connect(theQWidget, SIGNAL(somethingHappened()), qtmcmd, SLOT(apply()));
 
 Since the slot in this class accepts no arguments, commands which require
 access to the QWidget must be subclassed from command_rep to accept the
 particular QWidget as a parameter. Then their invocation (which apply() will
 call) must access it.

 An alternative would be to subclass QTMCommand to add slots accepting arguments
 but making sure that the underlying command_rep is properly sent to the mentioned
 queue.
*/
class QTMCommand: public QObject {
  Q_OBJECT

protected:
  command cmd;

public:
  QTMCommand (QObject* parent, command _cmd): QObject(parent), cmd (_cmd) {}

public slots:
  void apply();
};

/*! HACK: remove me!
 
 This special QTMCommand applies its underlying texmacs command immediately
 and upon destruction. This is needed to circumvent some strange behaviour
 where children QObjects are destroyed before the destroyed() signal is
 emmitted. 
 
 In particular, only qt_ui_widget_rep needs this for the wrapped_widget because
 it wants to connect the command to the destroyed() signal and the QTMCommand
 has already been deleted, the signal is disconnected and apply() never called.
 A nasty hack in the destructor applies the command anyway...
*/
class QTMOnDestroyCommand: public QTMCommand {
  Q_OBJECT

public:
  QTMOnDestroyCommand (QObject* parent, command _cmd): QTMCommand(parent, _cmd) {}
  ~QTMOnDestroyCommand () { apply (); }
public slots:
  void apply() {
    if (DEBUG_QT) 
      cout << "QTMOnDestroyCommand::apply()\n";
    // Immediately apply!!
    if (! is_nil(cmd)) cmd ();
  }
};


/*!
 *
 */
class QTMLazyMenu: public QMenu {
  Q_OBJECT
  promise<widget> pm;

public:
  QTMLazyMenu (promise<widget> _pm) : pm (_pm) {
      QObject::connect (this, SIGNAL (aboutToShow ()), this, SLOT (force ()));
  }

public slots:
  void force();
};


/*! The basic action for items in TeXmacs' menubars.
 
 This custom action frees its menu if it does not already have an owner: this is
 part of the memory policy explained in qt_menu_rep.

 A timer is used because the toolTips are not shown for items in the
 menu bar, since no mouse related events are ever sent to QActions placed
 there under MacOSX. We must use the signal hover() and add a timer to avoid
 instantly displaying the tooltip. This implies however ugly tradeoffs,
 see doShowToolTip().
 */
class QTMAction : public QAction {
  Q_OBJECT
  
  QTimer* _timer;
  QPoint    _pos;
public:
  string str;
  
  QTMAction(QObject *parent = NULL);
  ~QTMAction();

public slots:
  void doRefresh();
  void showToolTip();   //<! Force the display of the tooltip (starts a timer)

protected slots:
  void doShowToolTip();  // Actually show it.
};


/*! A customized QLineEdit with special keyboard handling and styling. */
class QTMLineEdit : public QLineEdit {
  Q_OBJECT

public:
  string ww; // width of the parsed widget
  
  QTMLineEdit (QWidget *parent, string _ww, int style=0);
  virtual QSize	sizeHint () const ;
  
protected:
  void keyPressEvent (QKeyEvent *event);
  void focusInEvent (QFocusEvent *evenement);
};


/*! A class to keep a QLineEdit object and a qt_input_text_widget_rep object in
 sync.
 
 After certain events we store state information about the QLineEdit into the 
 qt_input_text_widget_rep: when the user has finished editing (i.e. has pressed
 enter), or has left the QLineEdit for instance.
 
 Actually we use this with a QTMLineEdit.
 */
class QTMInputTextWidgetHelper : public QObject {
  Q_OBJECT

  widget p_wid; /*!< A reference to the tm widget, always a qt_input_text_widget_rep */

  bool done;
  
public:
  
  QList<QLineEdit*> views;

  QTMInputTextWidgetHelper ( qt_input_text_widget_rep*  _wid ) 
    : QObject(NULL), p_wid(abstract(_wid)), done(false) { }
  ~QTMInputTextWidgetHelper();

  qt_input_text_widget_rep* wid () { 
    return static_cast<qt_input_text_widget_rep*>(p_wid.rep); }
  // useful cast
  
  void add (QLineEdit *);

public slots:
  void commit ();
  void leave ();
  void remove (QObject *);
  
  void doit ();
  
};

class QTMWidgetAction : public QWidgetAction {
  Q_OBJECT

  widget wid;
  
public:
  QTMWidgetAction(widget _wid, QObject *parent = NULL);
  ~QTMWidgetAction();
  
public slots:
  virtual void doRefresh();
  
protected:
  QWidget * createWidget ( QWidget * parent );
  
};

/*! Implements a QTabWidget which resizes itself to the currently displayed page. */
class QTMTabWidget : public QTabWidget {
  Q_OBJECT
public:
  QTMTabWidget(QWidget* p=NULL);
public slots:
  void resizeOthers(int index);
};



#endif // QTMMENUHELPER_HPP
