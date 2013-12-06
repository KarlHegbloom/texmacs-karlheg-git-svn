
/******************************************************************************
* MODULE     : QTMWidget.hpp
* DESCRIPTION: QT Texmacs widget class
* COPYRIGHT  : (C) 2008 Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef QTMWIDGET_HPP
#define QTMWIDGET_HPP

#include "qt_widget.hpp"
#include "QTMScrollView.hpp"
#include <QVariant>
#include <QSet>
#include <QLabel>

class qt_simple_widget_rep;
class basic_renderer_rep;

/*! The underlying QWidget for a qt_simple_widget_rep handles drawing for a 
    texmacs canvas, as well as keypresses, international input methods, etc.
 
 QTMWidget needs a valid qt_simple_widget_rep object to function properly, see
 set_tm_widget() for more on this.
 
 */
class QTMWidget: public QTMScrollView {
  Q_OBJECT

  mutable widget       tmwid;
  rectangles invalid_regions;
  QPixmap      backingPixmap;
  QLabel*           imwidget;
  QPoint          cursor_pos;
  long                    id;
  
public:

  static QSet<QTMWidget*> all_widgets;  // needed by qt_gui_rep::update()
  QPoint                  backing_pos;

  QTMWidget (QWidget* _parent=0, qt_simple_widget_rep* _tmwid=0);
  virtual ~QTMWidget ();
  
  void invalidate_rect (int x1, int y1, int x2, int y2);
  void invalidate_all ();
  bool is_invalid ();
  void repaint_invalid_regions ();

  void scrollContentsBy (int dx, int dy);
  void setCursorPos (QPoint pos) { cursor_pos = pos; }

  qt_simple_widget_rep* tm_widget () const;
  void              set_tm_widget (qt_simple_widget_rep* _tmwid);
  
  virtual QSize	sizeHint () const;

protected:
  virtual void paintEvent (QPaintEvent* event);
  virtual void focusInEvent (QFocusEvent* event);
  virtual void focusOutEvent (QFocusEvent* event);
  virtual void keyPressEvent (QKeyEvent* event);
  virtual void inputMethodEvent (QInputMethodEvent* event);
  virtual void mousePressEvent (QMouseEvent* event);
  virtual void mouseReleaseEvent (QMouseEvent* event);
  virtual void mouseMoveEvent (QMouseEvent* event);
  virtual bool event (QEvent *event);
  virtual void resizeEvent (QResizeEvent *event);
  virtual void resizeEventBis (QResizeEvent *e);

  virtual QVariant inputMethodQuery (Qt::InputMethodQuery query) const;

private:
  basic_renderer_rep *getRenderer();
};

#endif // QTMWIDGET_HPP
