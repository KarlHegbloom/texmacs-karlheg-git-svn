
/******************************************************************************
* MODULE     : QTMWidget.cpp
* DESCRIPTION: QT Texmacs widget class
* COPYRIGHT  : (C) 2008 Massimiliano Gubinelli and Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "QTMWidget.hpp"
#include "qt_widget.hpp"
#include "qt_renderer.hpp"
#include "qt_gui.hpp"
#include "qt_utilities.hpp"
#include "qt_simple_widget.hpp"
#include "converter.hpp"

#include "config.h"
#include "message.hpp" 

#ifdef USE_CAIRO
#include "Cairo/cairo_renderer.hpp"
#include "Cairo/tm_cairo.hpp"

#if defined (Q_WS_X11)
#include <QX11Info>
extern Drawable qt_x11Handle (const QPaintDevice *pd);
extern const QX11Info *qt_x11Info (const QPaintDevice *pd);
#undef KeyPress  // conflict between QEvent::KeyPress and X11 defintion
#endif // Q_WS_X11

#endif // USE_CAIRO

#include <QEvent>
#include <QSet>
#include <QResizeEvent>
#include <QKeyEvent>
#include <QPaintEvent>
#include <QMouseEvent>
#include <QFocusEvent>

#define PIXEL 256

QSet<QTMWidget*> QTMWidget::all_widgets;


hashmap<int,string> qtkeymap (0);
hashmap<int,string> qtdeadmap (0);

inline void
scale (QPoint& point) {
  point.rx() *= PIXEL; point.ry() *= -PIXEL;
}

inline void
map (int code, string name) {
  qtkeymap (code) = name;
}
inline void
deadmap (int code, string name) {
  qtdeadmap (code) = name;
}

void
initkeymap () {
  map (Qt::Key_Space     , "space");
  map (Qt::Key_Return    , "return");
  map (Qt::Key_Tab       , "tab");
  map (Qt::Key_Backspace , "backspace");
  map (Qt::Key_Enter     , "enter");
  map (Qt::Key_Escape    , "escape");
  map (Qt::Key_Backspace , "backspace");
  map (Qt::Key_Up        , "up" );
  map (Qt::Key_Down      , "down" );
  map (Qt::Key_Left      , "left" );
  map (Qt::Key_Right     , "right" );
  map (Qt::Key_F1        , "F1" );
  map (Qt::Key_F2        , "F2" );
  map (Qt::Key_F3        , "F3" );
  map (Qt::Key_F4        , "F4" );
  map (Qt::Key_F5        , "F5" );
  map (Qt::Key_F6        , "F6" );
  map (Qt::Key_F7        , "F7" );
  map (Qt::Key_F8        , "F8" );
  map (Qt::Key_F9        , "F9" );
  map (Qt::Key_F10       , "F10" );
  map (Qt::Key_F11       , "F11" );
  map (Qt::Key_F12       , "F12" );
  map (Qt::Key_F13       , "F13" );
  map (Qt::Key_F14       , "F14" );
  map (Qt::Key_F15       , "F15" );
  map (Qt::Key_F16       , "F16" );
  map (Qt::Key_F17       , "F17" );
  map (Qt::Key_F18       , "F18" );
  map (Qt::Key_F19       , "F19" );
  map (Qt::Key_F20       , "F20" );
  map (Qt::Key_F21       , "F21" );
  map (Qt::Key_F22       , "F22" );
  map (Qt::Key_F23       , "F23" );
  map (Qt::Key_F24       , "F24" );
  map (Qt::Key_F25       , "F25" );
  map (Qt::Key_F26       , "F26" );
  map (Qt::Key_F27       , "F27" );
  map (Qt::Key_F28       , "F28" );
  map (Qt::Key_F29       , "F29" );
  map (Qt::Key_F30       , "F30" );
  map (Qt::Key_F31       , "F31" );
  map (Qt::Key_F32       , "F32" );
  map (Qt::Key_F33       , "F33" );
  map (Qt::Key_F34       , "F34" );
  map (Qt::Key_F35       , "F35" );
  map (Qt::Key_Insert    , "insert" );
  map (Qt::Key_Delete    , "delete" );
  map (Qt::Key_Home      , "home" );
  map (Qt::Key_End       , "end" );
  map (Qt::Key_PageUp    , "pageup" );
  map (Qt::Key_PageDown  , "pagedown" );
  map (Qt::Key_ScrollLock, "scrolllock" );
  map (Qt::Key_Pause     , "pause" );
  map (Qt::Key_SysReq    , "sysreq" );
  map (Qt::Key_Stop      , "stop" );
  map (Qt::Key_Menu      , "menu" );
  map (Qt::Key_Print     , "print" );
  map (Qt::Key_Select    , "select" );
  map (Qt::Key_Execute   , "execute" );
  map (Qt::Key_Help      , "help" );

  deadmap (Qt::Key_Dead_Acute     , "acute");
  deadmap (Qt::Key_Dead_Grave     , "grave");
  deadmap (Qt::Key_Dead_Diaeresis , "umlaut");
  deadmap (Qt::Key_Dead_Circumflex, "hat");
  deadmap (Qt::Key_Dead_Tilde     , "tilde");

  // map (0x0003              , "K-enter");
  // map (Qt::Key_Begin       , "begin" );
  // map (Qt::Key_PrintScreen , "printscreen" );
  // map (Qt::Key_Break       , "break" );
  // map (Qt::Key_User        , "user" );
  // map (Qt::Key_System      , "system" );
  // map (Qt::Key_Reset       , "reset" );
  // map (Qt::Key_ClearLine   , "clear" );
  // map (Qt::Key_ClearDisplay, "cleardisplay" );
  // map (Qt::Key_InsertLine  , "insertline" );
  // map (Qt::Key_DeleteLine  , "deleteline" );
  // map (Qt::Key_InsertChar  , "insert" );
  // map (Qt::Key_DeleteChar  , "delete" );
  // map (Qt::Key_Prev        , "prev" );
  // map (Qt::Key_Next        , "next" );
  // map (Qt::Key_Undo        , "undo" );
  // map (Qt::Key_Redo        , "redo" );
  // map (Qt::Key_Find        , "find" );
  // map (Qt::Key_ModeSwitchFunctionKey, "modeswitch" );
}
#ifdef __MINGW32__
enum WindowsNativeModifiers {
    ShiftLeft            = 0x00000001,
    ControlLeft          = 0x00000002,
    AltLeft              = 0x00000004,
    MetaLeft             = 0x00000008,
    ShiftRight           = 0x00000010,
    ControlRight         = 0x00000020,
    AltRight             = 0x00000040,
    MetaRight            = 0x00000080,
    CapsLock             = 0x00000100,
    NumLock              = 0x00000200,
    ScrollLock           = 0x00000400,
    ExtendedKey          = 0x01000000,
};
#endif
static long int QTMWcounter = 0; // debugging hack

/*! Constructor.
 
  \param _parent The parent QWidget.
  \param _tmwid the TeXmacs widget who owns this object.
 */
QTMWidget::QTMWidget (QWidget* _parent, qt_widget _tmwid)
: QTMScrollView (_parent), tmwid (_tmwid), backingPixmap (1,1), imwidget (NULL),
  id (QTMWcounter++)
{
  setObjectName (to_qstring ("QTMWidget" * as_string (id)));// What is this for?
  setFocusPolicy (Qt::StrongFocus);
  setAttribute (Qt::WA_InputMethodEnabled);
  all_widgets.insert (this);

  backing_pos = origin ();
  surface()->setMouseTracking (true);
  
  if (DEBUG_QT)
    debug_qt << "Creating QTMWidget " << as_string (id) << " of widget "
             << (tm_widget() ? tm_widget()->type_as_string() : "NULL") << LF;
}

QTMWidget::~QTMWidget () {
  if (DEBUG_QT)
    debug_qt << "Destroying QTMWidget " << as_string (id) << " of widget "
             << (tm_widget() ? tm_widget()->type_as_string() : "NULL") << LF;
  
    // remove ourselves from the list of QWidgets to be repainted.
  all_widgets.remove (this);
}

qt_simple_widget_rep*
QTMWidget::tm_widget () const { 
  return concrete_simple_widget (tmwid); 
}

void 
QTMWidget::invalidate_rect (int x1, int y1, int x2, int y2) {
#ifdef Q_WS_MAC
  //HACK: for unknown reasons we need to enlarge the invalid rect to prevent
  //artifacts while moving the cursor (for example at the end of a formula like
  // $a+f$. These artifacts seems present only on 64 bit Macs. 
  rectangle r = rectangle (x1-10, y1-10, x2+10, y2+10);
#else
  rectangle r = rectangle (x1, y1, x2, y2);
#endif
  // cout << "invalidating " << r << LF;
  invalid_regions = invalid_regions | rectangles (r);
}

void 
QTMWidget::invalidate_all () {
  QSize sz = surface()->size();
 // QPoint pt = QAbstractScrollArea::viewport()->pos();
   //cout << "invalidate all " << LF;
  invalid_regions = rectangles();
  invalidate_rect (0, 0, sz.width(), sz.height());
}

bool
QTMWidget::is_invalid () {
  return !is_nil (invalid_regions);
}

basic_renderer_rep* 
QTMWidget::getRenderer() {
#ifdef USE_CAIRO
  cairo_renderer_rep *ren = the_cairo_renderer ();
  cairo_surface_t *surf;
#ifdef Q_WS_X11
  //const QX11Info & info = x11Info();//qt_x11Info (this);
  //    Display *dpy = x11Info().display();
  //backingPixmap = QPixmap (width(),height());
  //cout << backingPixmap.width() << LF;
  Display *dpy = QX11Info::display();
  Drawable drawable = backingPixmap.handle();
  Visual *visual = (Visual*)(backingPixmap.x11Info().visual());
  surf = tm_cairo_xlib_surface_create (dpy, drawable, visual, 
                            backingPixmap.width (), backingPixmap.height ());
#elif defined (Q_WS_MAC)
  surf = tm_cairo_quartz_surface_create_for_cg_context (
                    (CGContextRef)(this->macCGHandle()), width(), height());
#endif
  cairo_t *ct = tm_cairo_create (surf);
  ren->begin (ct);
  tm_cairo_surface_destroy (surf);
  tm_cairo_destroy (ct);
#else
  qt_renderer_rep * ren = the_qt_renderer();
  ren->begin (&backingPixmap);
#endif
  return ren;
}

/*
 This function is called by the qt_gui::update method to keep the backing
 store in sync and propagate the changes to the surface on screen.
 First we check that the backing store geometry is right and then we
 request to the texmacs canvas widget to repaint the regions which were
 marked invalid. Subsequently, for each succesfully repainted region, we
 propagate its contents from the backing store to the onscreen surface.
 If repaint has been interrupted we do not propagate the changes and proceed
 to mark the region invalid again.
*/
void
QTMWidget::repaint_invalid_regions () {

  QRegion qrgn; 
  // qrgn is to keep track of the area on the screen which needs to be updated 

  // update backing store origin wrt. TeXmacs document
  if (backing_pos != origin()) {

    int dx =  origin().x() - backing_pos.x();
    int dy =  origin().y() - backing_pos.y();
    backing_pos = origin();
    
    QPixmap newBackingPixmap (backingPixmap.size());
    QPainter p (&newBackingPixmap);
    //newBackingPixmap.fill (Qt::black);
    p.drawPixmap (-dx,-dy,backingPixmap);
    p.end();
    backingPixmap = newBackingPixmap;
    //cout << "SCROLL CONTENTS BY " << dx << " " << dy << LF;
    
    QSize sz = backingPixmap.size();
    
    rectangles invalid;
    while (!is_nil (invalid_regions)) {
      rectangle r = invalid_regions->item ;
      //      rectangle q = rectangle (r->x1+dx,r->y1-dy,r->x2+dx,r->y2-dy);
      rectangle q = rectangle (r->x1-dx,r->y1-dy,r->x2-dx,r->y2-dy);
      invalid = rectangles (q, invalid);
      //cout << r << " ---> " << q << LF;
      invalid_regions = invalid_regions->next;
    }
    invalid_regions= invalid & rectangles (rectangle (0,0,
                                                    sz.width(),sz.height()));
    
    if (dy<0) 
      invalidate_rect (0,0,sz.width(),min (sz.height(),-dy));
    else if (dy>0)
      invalidate_rect (0,max (0,sz.height()-dy),sz.width(),sz.height());
    
    if (dx<0) 
      invalidate_rect (0,0,min (-dx,sz.width()),sz.height());
    else if (dx>0)
      invalidate_rect (max (0,sz.width()-dx),0,sz.width(),sz.height());
    
    // we call update now to allow repainting of invalid regions
    // this cannot be done directly since interpose_handler needs
    // to be run at least once in some situations
    // (for example when scrolling is initiated by TeXmacs itself)
    //the_gui->update();
    //  QAbstractScrollArea::viewport()->scroll (-dx,-dy);
   // QAbstractScrollArea::viewport()->update();
    qrgn += QRect (QPoint (0,0),sz);
  }
  
    //cout << "   repaint QPixmap of size " << backingPixmap.width() << " x " 
    // << backingPixmap.height() << LF;
  // update backing store size
  {
    QSize _oldSize = backingPixmap.size();
    QSize _newSize = surface()->size();
    
      //cout << "      surface size of " << _newSize.width() << " x " 
      // << _newSize.height() << LF;
    
    
    if (_newSize != _oldSize) {
      // cout << "RESIZING BITMAP"<< LF;
      QPixmap newBackingPixmap (_newSize);
      QPainter p (&newBackingPixmap);
      p.drawPixmap (0,0,backingPixmap);
      //p.fillRect (0, 0, _newSize.width(), _newSize.height(), Qt::red);
      if (_newSize.width() >= _oldSize.width()) {
        invalidate_rect (_oldSize.width(), 0, _newSize.width(), _newSize.height());
        p.fillRect (QRect (_oldSize.width(), 0, _newSize.width()-_oldSize.width(), _newSize.height()), Qt::gray);
      }
      if (_newSize.height() >= _oldSize.height()) {
        invalidate_rect (0,_oldSize.height(), _newSize.width(), _newSize.height());
        p.fillRect (QRect (0,_oldSize.height(), _newSize.width(), _newSize.height()-_oldSize.height()), Qt::gray);
      }
      p.end();
      backingPixmap = newBackingPixmap;
    }
  }
  
  // repaint invalid rectangles
  {
    rectangles new_regions;
    if (!is_nil (invalid_regions)) {
      rectangle lub= least_upper_bound (invalid_regions);
      if (area (lub) < 1.2 * area (invalid_regions))
        invalid_regions= rectangles (lub);
      
      basic_renderer_rep* ren = getRenderer();
      
      SI ox = -backing_pos.x()*PIXEL;  // Warning: this is NOT from_qpoint()
      SI oy = backing_pos.y()*PIXEL;
      
      rectangles rects = invalid_regions;
      invalid_regions = rectangles();

      while (!is_nil (rects)) {
        rectangle r = copy (rects->item);
        rectangle r0 = rects->item;
        QRect qr = QRect (r0->x1, r0->y1, r0->x2 - r0->x1, r0->y2 - r0->y1);
        //cout << "repainting " << r0 << "\n";
        ren->set_origin (ox, oy); 
        ren->encode (r->x1, r->y1);
        ren->encode (r->x2, r->y2);
        ren->set_clipping (r->x1, r->y2, r->x2, r->y1);
        tm_widget()->handle_repaint (ren, r->x1, r->y2, r->x2, r->y1);
        if (gui_interrupted ()) {
          //cout << "interrupted repainting of  " << r0 << "\n";
          //ren->set_pencil (green);
          //ren->line (r->x1, r->y1, r->x2, r->y2);
          //ren->line (r->x1, r->y2, r->x2, r->y1);
          invalidate_rect (r0->x1, r0->y1, r0->x2, r0->y2);
        } 
        qrgn += qr;
        rects = rects->next;
      }
      
      ren->end();
    } // !is_nil (invalid_regions)
  }

  // propagate immediately the changes to the screen
  surface()->repaint (qrgn);
}

void 
QTMWidget::scrollContentsBy (int dx, int dy) {
  QTMScrollView::scrollContentsBy (dx,dy);

  the_gui->force_update();
  // we force an update of the internal state to be in sync with the moving
  // scrollbars
}

void 
QTMWidget::resizeEvent (QResizeEvent* event) {
  (void) event;
  // Is this ok?
  //coord2 s = from_qsize (event->size());
  //the_gui -> process_resize (tm_widget(), s.x1, s.x2);

  // the_gui->force_update();

  //FIXME: I would like to have a force_update here but this causes a failed
  //assertion in TeXmacs since the at the boot not every internal structure is
  //initialized at this point. It seems not too difficult to fix but I
  //postpone this to discuss with Joris. 
  //
  //Not having a force_update results in some lack of sync of the surface
  //while the user is actively resizing with the mouse.
}

void
QTMWidget::resizeEventBis (QResizeEvent *event) {
  coord2 s = from_qsize (event->size());
  the_gui -> process_resize (tm_widget(), s.x1, s.x2);
}

/*!
 In the current implementation repainting takes place during the call to
 the widget's repaint_invalid_regions() method in the_gui::update. All
 we have to do is to take the backing store and put it on screen according
 to the QRegion marked invalid. 
 CHECK: Maybe just putting onscreen all the region bounding rectangles might 
 be less expensive.
*/
void
QTMWidget::paintEvent (QPaintEvent* event) {
  QPainter p (surface());
  QVector<QRect> rects = event->region().rects();
  for (int i = 0; i < rects.count(); ++i) {
    QRect qr = rects.at (i);
    p.drawPixmap (qr, backingPixmap, qr);
  }
}
void
QTMWidget::keyPressEvent (QKeyEvent* event) {
  if (is_nil (tmwid)) return;
  static bool fInit = false;
  if (!fInit) {
    if (DEBUG_QT && DEBUG_KEYBOARD) debug_qt << "Initializing keymap\n";
    initkeymap();
    fInit= true;
  }

  if (DEBUG_QT && DEBUG_KEYBOARD) debug_qt << "keypressed\n";
  {
    int key = event->key();
    Qt::KeyboardModifiers mods = event->modifiers();

    if (DEBUG_QT && DEBUG_KEYBOARD) {
      debug_qt << "key  : " << key << LF;
      debug_qt << "text : " << event->text().toLatin1().data() << LF;
      debug_qt << "count: " << event->text().count() << LF;
#ifdef __MINGW32__
      debug_qt << "nativeScanCode: " << event->nativeScanCode() << LF; 
      debug_qt << "nativeVirtualKey: " << event->nativeVirtualKey() << LF;
      debug_qt << "nativeModifiers: " << event->nativeModifiers() << LF;
#endif
      if (mods & Qt::ShiftModifier) debug_qt << "shift\n";
      if (mods & Qt::MetaModifier) debug_qt << "meta\n";
      if (mods & Qt::ControlModifier) debug_qt << "control\n";
      if (mods & Qt::KeypadModifier) debug_qt << "keypad\n";
      if (mods & Qt::AltModifier) debug_qt << "alt\n";
    }

    string r;
#ifdef __MINGW32__ 
/* "Qt::Key_AltGr On Windows, when the KeyDown event for this key is sent,
* the Ctrl+Alt modifiers are also set." (excerpt from Qt doc)
* However the AltGr key is used to obtain many symbols 
* which should not be regarded as C-A- shortcuts.
* (e.g. \ or @ on a French keyboard) 
* 
* Hence, when "native modifiers" are (ControlLeft | AltRight) 
* we clear Qt's Ctrl+Alt modifiers
*/
    if ((event->nativeModifiers() & (ControlLeft | AltRight)) == (ControlLeft | AltRight)){
		if (DEBUG_QT && DEBUG_KEYBOARD) debug_qt << "assuming it's an AltGr key code"<<LF;
		mods &= ~Qt::AltModifier;
		mods &= ~Qt::ControlModifier;
	}
#endif
    if (qtkeymap->contains (key)) {
      r = qtkeymap[key];
    }
    else if (qtdeadmap->contains (key)) {
      mods &=~ Qt::ShiftModifier;
      r = qtdeadmap[key];
    }
    else {
        // We need to use text(): Alt-{5,6,7,8,9} are []|{} under MacOS, etc.
      QString nss = event->text();
      unsigned short unic= nss.data()[0].unicode();
      if (unic < 32 && key < 128 && key > 0) {
        if (((char) key) >= 'A' && ((char) key) <= 'Z') {
          if ((mods & Qt::ShiftModifier) == 0)
            key= (int) (key + ((int) 'a') - ((int) 'A'));
        }
        mods &=~ Qt::ShiftModifier;
        r= string ((char) key);
      } else {
        switch (unic) {
          case 96:   r= "`"; 
            // unicode to cork conversion not appropriate for this case...
#ifdef Q_WS_MAC
            // CHECKME: are these two MAC exceptions really needed?
            if (mods & Qt::AltModifier) r= "grave";
#endif
            break;
          case 168:  r= "umlaut"; break;
          case 180:  r= "acute"; break;
            // the following combining characters should be caught by qtdeadmap
          case 0x300: r= "grave"; break;
          case 0x301: r= "acute"; break;
          case 0x302: r= "hat"; break;
          case 0x308: r= "umlaut"; break;
          case 0x33e: r= "tilde"; break;
          default:
            QByteArray buf= nss.toUtf8();
            string rr (buf.constData(), buf.count());
            string tstr= utf8_to_cork (rr);
            // HACK! The encodings defined in langs/encoding and which
            // utf8_to_cork uses (via the converters loaded in
            // converter_rep::load()), enclose the texmacs symbols in "< >", 
            // but this format is not used for keypresses, so we must remove
            // them.
            int len= N (tstr);
            if (len >= 1 && tstr[0] == '<' && tstr[1] != '#' && tstr[len-1] == '>')
              r= tstr (1, len-1);
            else
              r= tstr;
            if (r == "less") r= "<";
            else if (r == "gtr")r= ">";
        }
#ifdef Q_WS_MAC
          // Alt produces many symbols in Mac keyboards: []|{} etc.
        mods &= ~Qt::AltModifier; //unset Alt
#endif
        mods &= ~Qt::ShiftModifier;
      }
    }
    if (r == "") return;
    if (mods & Qt::ShiftModifier) r= "S-" * r;
    if (mods & Qt::AltModifier) r= "A-" * r;
    //if (mods & Qt::KeypadModifier) r= "K-" * r;
#ifdef Q_WS_MAC
    if (mods & Qt::MetaModifier) r= "C-" * r;        // The "Control" key
    if (mods & Qt::ControlModifier) r= "M-" * r;  // The "Command" key
#else
    if (mods & Qt::ControlModifier) r= "C-" * r;
    if (mods & Qt::MetaModifier) r= "M-" * r;     // The "Windows" key
#endif

    if (DEBUG_QT && DEBUG_KEYBOARD) debug_qt << "key press: " << r << LF;
    the_gui->process_keypress (tm_widget(), r, texmacs_time());
  }
}

static unsigned int
mouse_state (QMouseEvent* event, bool flag) {
  unsigned int i= 0;
  Qt::MouseButtons bstate= event->buttons ();
  Qt::MouseButton  tstate= event->button ();
  Qt::KeyboardModifiers kstate= event->modifiers ();
  if (flag) bstate= bstate | tstate;
  if ((bstate & Qt::LeftButton     ) != 0) i += 1;
  if ((bstate & Qt::MidButton      ) != 0) i += 2;
  if ((bstate & Qt::RightButton    ) != 0) i += 4;
  if ((bstate & Qt::XButton1       ) != 0) i += 8;
  if ((bstate & Qt::XButton2       ) != 0) i += 16;
#ifdef Q_WS_MAC
    // We emulate right and middle clicks with ctrl and option, but we pass the
    // modifiers anyway: old code continues to work and new one can use them.
  if ((kstate & Qt::MetaModifier   ) != 0) i = 1024+4; // control key
  if ((kstate & Qt::AltModifier    ) != 0) i = 2048+2; // option key
  if ((kstate & Qt::ShiftModifier  ) != 0) i += 256;
  if ((kstate & Qt::ControlModifier) != 0) i += 4096;   // cmd key
#else
  if ((kstate & Qt::ShiftModifier  ) != 0) i += 256;
  if ((kstate & Qt::ControlModifier) != 0) i += 1024;
  if ((kstate & Qt::AltModifier    ) != 0) i += 2048;
  if ((kstate & Qt::MetaModifier   ) != 0) i += 4096;
#endif
  return i;
}

static string
mouse_decode (unsigned int mstate) {
  if (mstate & 2) return "middle";
  else if (mstate & 4) return "right";
    // we check for left clicks after the others for macos (see ifdef in mouse_state)
  else if (mstate & 1) return "left";
  else if (mstate & 8) return "up";
  else if (mstate & 16) return "down";
  return "unknown";
}

#if 0 // NOT USED
static void setRoundedMask (QWidget *widget)
{
  QPixmap pixmap (widget->size());
  QPainter painter (&pixmap);
  painter.fillRect (pixmap.rect(), Qt::white);
  painter.setBrush (Qt::black);
#if (QT_VERSION >= 0x040400)
  painter.drawRoundedRect (pixmap.rect(),8,8, Qt::AbsoluteSize);
#else
  painter.drawRect (pixmap.rect());
#endif
  widget->setMask (pixmap.createMaskFromColor (Qt::white));
}
#endif


#if 0 
// OLD INPUT METHOD PREVIEW
void
QTMWidget::inputMethodEvent (QInputMethodEvent* event) {
  if (! imwidget) {   
    imwidget = new QLabel (this);
    imwidget->setWindowFlags (Qt::Tool | Qt::FramelessWindowHint);
  //  imwidget->setAttribute (Qt::WA_TranslucentBackground);
//    imwidget->setAutoFillBackground (false);
       imwidget->setAutoFillBackground (true);
    imwidget->setWindowOpacity (0.5);
    imwidget->setFocusPolicy (Qt::NoFocus);
    QPalette pal = imwidget->palette();
//    pal.setColor (QPalette::Window, QColor (0,0,255,80));
    pal.setColor (QPalette::Window, QColor (0,0,255,255));
    pal.setColor (QPalette::WindowText, Qt::white);
    imwidget->setPalette (pal);
    QFont f = imwidget->font();
    f.setPointSize (qt_zoom (30));
    imwidget->setFont (f);
    imwidget->setMargin (5);
  }

  QString const & preedit_string = event->preeditString();
  QString const & commit_string = event->commitString();

  if (preedit_string.isEmpty()) {
    imwidget->hide();
  } else {
    if (DEBUG_QT)
      debug_qt << "IM preediting :" << preedit_string.toUtf8().data() << LF;
    imwidget->setText (preedit_string);
    imwidget->adjustSize();
    QSize sz = size();
    QRect g = imwidget->geometry();
    QPoint c = mapToGlobal (cursor_pos);
    c += QPoint (5,5);
    // g.moveCenter (QPoint (sz.width()/2,sz.height()/2));
    g.moveTopLeft (c);
    if (DEBUG_QT)
      debug_qt << "IM hotspot: " << cursor_pos.x() << "," << cursor_pos.y() << LF;
    imwidget->setGeometry (g);
    // setRoundedMask (imwidget);
    imwidget->show();
#ifdef QT_MAC_USE_COCOA
    // HACK: we unexplicably loose the focus even when showing the small window,
    // so we need to restore it manually.....
    // The following fixes the problem (but I do not really understand why it 
    // happens)
    // Maybe this is a Qt/Cocoa bug.
    this->window()->activateWindow();
#endif    
  }
  
  if (!commit_string.isEmpty()) {
    if (DEBUG_QT)
      debug_qt << "IM committing :" << commit_string.toUtf8().data() << LF;

    int key = 0;
#if 1
    for (int i = 0; i < commit_string.size(); ++i) {
      QKeyEvent ev (QEvent::KeyPress, key, Qt::NoModifier, commit_string[i]);
      keyPressEvent (&ev);
    }
#else
    QKeyEvent ev (QEvent::KeyPress, key, Qt::NoModifier, commit_string);
    keyPressEvent (&ev);
#endif
  }
  
  event->accept();

}  

QVariant 
QTMWidget::inputMethodQuery (Qt::InputMethodQuery query) const {
  switch (query) {
    case Qt::ImMicroFocus :
      return QVariant (QRect (cursor_pos + QPoint (10,10),QSize (20,40)));
    default:
      return QVariant();
  }
}

#else

// NEW INPUT METHOD PREVIEW
void
QTMWidget::inputMethodEvent (QInputMethodEvent* event) {
  
  QString const & preedit_string = event->preeditString();
  QString const & commit_string = event->commitString();
  
  if (!commit_string.isEmpty()) {
    if (DEBUG_QT)
      debug_qt << "IM committing :" << commit_string.toUtf8().data() << LF;
    
    int key = 0;
#if 1
    for (int i = 0; i < commit_string.size(); ++i) {
      QKeyEvent ev (QEvent::KeyPress, key, Qt::NoModifier, commit_string[i]);
      keyPressEvent (&ev);
    }
#else
    QKeyEvent ev (QEvent::KeyPress, key, Qt::NoModifier, commit_string);
    keyPressEvent (&ev);
#endif
  }
  
  if (DEBUG_QT)
    debug_qt << "IM preediting :" << preedit_string.toUtf8().data() << LF;
  
  string r = "pre-edit:";
  if (!preedit_string.isEmpty())
  {
    
    // find cursor position in the preedit string
    QList<QInputMethodEvent::Attribute>  const & attrs = event->attributes();
    //    int pos = preedit_string.count();
    int pos = 0;
    bool visible_cur = false;
    for (int i=0; i< attrs.count(); i++) 
      if (attrs[i].type == QInputMethodEvent::Cursor) {
        pos = attrs[i].start;
        visible_cur = (attrs[i].length != 0);
      }
    
    // find selection in the preedit string
    int sel_start = 0;
    int sel_length = 0;
    if (pos <  preedit_string.count()) {
      for (int i=0; i< attrs.count(); i++) 
        if ((attrs[i].type == QInputMethodEvent::TextFormat) &&
            (attrs[i].start <= pos) &&
            (pos < attrs[i].start + attrs[i].length)) {
          sel_start = attrs[i].start;
          sel_length =  attrs[i].length;
          if (!visible_cur) pos += attrs[i].length;
        }
    } else {
      sel_start = pos;
      sel_length = 0;
    }
    (void) sel_start; (void) sel_length;
    
    r = r * as_string (pos) * ":" * from_qstring (preedit_string);
  }
  if (!is_nil (tmwid))
    the_gui->process_keypress (tm_widget(), r, texmacs_time());
  event->accept();
}  

QVariant 
QTMWidget::inputMethodQuery (Qt::InputMethodQuery query) const {
  switch (query) {
    case Qt::ImMicroFocus :
      return QVariant (QRect (cursor_pos, QSize (5,5)));
    default:
      return QWidget::inputMethodQuery (query);
  }
}

#endif // input method variants

void
QTMWidget::mousePressEvent (QMouseEvent* event) {
  if (is_nil (tmwid)) return;
  QPoint point = event->pos() + origin();
  scale (point);
  unsigned int mstate= mouse_state (event, false);
  string s= "press-" * mouse_decode (mstate);
  the_gui -> process_mouse (tm_widget(), s, point.x (), point.y (), 
                            mstate, texmacs_time ());
  event->accept();
}

void
QTMWidget::mouseReleaseEvent (QMouseEvent* event) {
  if (is_nil (tmwid)) return;
  QPoint point = event->pos() + origin();
  scale (point);
  unsigned int mstate = mouse_state (event, true);
  string s = "release-" * mouse_decode (mstate);
  the_gui->process_mouse (tm_widget(), s, point.x(), point.y(),
                            mstate, texmacs_time());
  event->accept();
}

void
QTMWidget::mouseMoveEvent (QMouseEvent* event) {
  if (is_nil (tmwid)) return;
  QPoint point = event->pos() + origin();
  scale (point);
  unsigned int mstate = mouse_state (event, false);
  string s = "move";
  the_gui->process_mouse (tm_widget(), s, point.x(), point.y(),
                          mstate, texmacs_time ());
  event->accept();
}


bool
QTMWidget::event (QEvent* event) {
    // Catch Keypresses to avoid default handling of (Shift+)Tab keys
  if (event->type() == QEvent::KeyPress) {
    QKeyEvent *ke = static_cast<QKeyEvent*> (event);
    keyPressEvent (ke);
    return true;
  } 
  /* NOTE: we catch ShortcutOverride in order to disable the QKeySequences we
   assigned to QActions while building menus, etc. In doing this, we keep the
   shortcut text in the menus while relaying all keypresses through the editor*/
  if (event->type() == QEvent::ShortcutOverride) {
    event->accept();
    return true;
  }
  return QTMScrollView::event (event);
}

void
QTMWidget::focusInEvent (QFocusEvent * event) {
  if (!is_nil (tmwid)) {
    if (DEBUG_QT) debug_qt << "FOCUSIN: " << tm_widget()->type_as_string() << LF;
    the_gui->process_keyboard_focus (tm_widget(), true, texmacs_time());
  }
  QTMScrollView::focusInEvent (event);
}

void
QTMWidget::focusOutEvent (QFocusEvent * event) {
  if (!is_nil (tmwid)) {
    if (DEBUG_QT) debug_qt << "FOCUSOUT: " << tm_widget()->type_as_string() << LF;
    the_gui -> process_keyboard_focus (tm_widget(), false, texmacs_time());
  }
  QTMScrollView::focusOutEvent (event);
}

QSize
QTMWidget::sizeHint () const {
  SI w = 0, h = 0;
  if (!is_nil (tmwid)) tm_widget()->handle_get_size_hint (w, h);
  return to_qsize (w, h);
}
