
/******************************************************************************
* MODULE     : qt_simple_widget.hpp
* DESCRIPTION: QT simple widget class
* COPYRIGHT  : (C) 2008  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#ifndef QT_SIMPLE_WIDGET_HPP
#define QT_SIMPLE_WIDGET_HPP

#include "widget.hpp"

#include "qt_widget.hpp"

class simple_widget_rep: public qt_view_widget_rep {
public:
  simple_widget_rep ();
	
  virtual void handle_get_size_hint (SI& w, SI& h);
  virtual void handle_notify_resize (SI w, SI h);
  virtual void handle_keypress (string key, time_t t);
  virtual void handle_keyboard_focus (bool has_focus, time_t t);
  virtual void handle_mouse (string kind, SI x, SI y, int mods, time_t t);
  virtual void handle_set_shrinking_factor (int sf);
  virtual void handle_clear (SI x1, SI y1, SI x2, SI y2);
  virtual void handle_repaint (SI x1, SI y1, SI x2, SI y2);

	virtual void send (slot s, blackbox val);
	// send a message val to the slot s
  virtual blackbox query (slot s, int type_id);
	// obtain information of a given type from the slot s
  virtual widget read (slot s, blackbox index);
	// abstract read access (of type s) of a subwidget at position index
  virtual void write (slot s, blackbox index, widget w);
	// abstract write access (of type s) of a subwidget at position index
  virtual void notify (slot s, blackbox new_val);


	virtual QAction *as_qaction();

//  virtual TMMenuItem *as_menuitem();

};

#endif // defined QT_SIMPLE_WIDGET_HPP
