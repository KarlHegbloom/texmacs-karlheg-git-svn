<TeXmacs|1.0.1.10>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|L'interface utilisateur graphique>

  <section|Introduction>

  <subsection|Main architecture>

  The graphical toolkit used by Edit has two main components: an abstract
  window interface, which is very similar to X Window, and the actual
  toolkit. At this moment an abstract window interface for X Window has been
  implemented, but others might be added in the feature.

  The abstract window interface consists of two main classes:
  <verbatim|display> and <verbatim|window>. The <verbatim|display> class is
  responsible for

  <\itemize>
    <item>The connection with the (X-)server.

    <item>Managing server resources such as colors and fonts.

    <item>Inter-window communication (i.e. selections and so).

    <item>Redirection of input/output (pointer and keyboard grabs).
  </itemize>

  The <verbatim|window> class is responsible for

  <\itemize>
    <item>The part of the layout of a window, which is negotiated with the
    window manager.

    <item>Providing a basic set of postscript-compatible graphical routines.

    <item>Implementation of some clipping and region translation routines.

    <item>Delegation of events to the widget associated to the window.
  </itemize>

  In particular, the <verbatim|window>-class inherits from the
  <verbatim|ps_device>-class, which is responsible for providing the basic
  set of postscript-compatible graphical routines. Hence, user applications
  will draw their graphics on a <verbatim|ps_device>, since this will both
  allow them to visualize them in a window or on a printer.

  The graphical toolkit built on top of the abstract window interface is
  widget oriented. A large number of widget classes are implemented, which
  all inherit from the abstract <verbatim|widget> class. Widgets may have a
  finite number of children and they are responsible for

  <\itemize>
    <item>Their, and their children, sizes and positioning in the window.

    <item>Reaction on events, which are either redraw requests, keyboard or
    pointer events or other miscellaneous events.

    <item>Other functionalities, which depend on the particular widget class.
  </itemize>

  Later a special widget-style manager will be implemented, which will be
  able to present widgets according to different ``styles''.

  <subsubsection|A simple example>

  In order to create a window ``Test'' with the text ``Hello world'' in it,
  one first opens the display, then create the widget with the text and
  finally the window, with the widget attached to it\ 

  <\verbatim>
    \ \ \ \ display dis= open_display ();<format|next line> \ \ \ widget
    \ wid= text_widget ("Hello world");<format|next line> \ \ \ window \ win=
    plain_window (wid, dis, "Test");
  </verbatim>

  Technically speaking, the window creation amounts to several actions:

  <\itemize>
    <item>The widget is questioned for an appropriate size for it (actually,
    only minimal, default and maximal size hints are computed).

    <item>The window is created.

    <item>The widget is ``attached'' to the window.

    <item>The subwidgets are ``positioned'' at their appropriate places and
    they are given appropriate sizes within their parents.
  </itemize>

  The next step is to make the window visible by\ 

  <\verbatim>
    \ \ \ \ win-\<gtr\>map ();
  </verbatim>

  At this points repaint request events are generated, which are handled when
  starting the event loop by\ 

  <\verbatim>
    \ \ \ \ dis-\<gtr\>event_loop ();
  </verbatim>

  Eventually, the window <verbatim|win> will be destroyed using\ 

  <\verbatim>
    \ \ \ \ delete win;
  </verbatim>

  At this point, control is handled back by the event loop and we close and
  destroy the display by\ 

  <\verbatim>
    \ \ \ \ close_display (dis)<format|next line> \ \ \ delete dis;
  </verbatim>

  The user only has to bother about destroying window and displays; widgets
  are automatically destroyed if they are no longer being referenced to.

  <subsection|Widgets and event processing>

  From the implementation point of view, widgets are pointers to instances of
  the abstract widget representation class <verbatim|widget_rep>. Moreover,
  the widget class supports reference counting. The <verbatim|widget_rep>
  class contains information about the window it is attached to and its
  location within this window, its size, the position of the origin of its
  local coordinates (<verbatim|north_west>, <verbatim|north>, etc.) and its
  children. The <verbatim|widget_rep> class also provides a virtual event
  handler <verbatim|widget_rep::handle>. This handler returns true if the
  event could be handled.

  The implemented widget representation classes are organized in a hierarchy,
  which contains both concrete and abstract classes. The abstract classes
  reimplement the virtual event handler <verbatim|widget_rep::handle> in such
  a way that if the event is recognized, then the event is dispatched to a
  more particular virtual event handler, possibly after some processing.

  For example, instances of the <verbatim|basic_widget_rep> class can handle
  the most common events, such as keyboard, mouse, repaint events and so on.
  If a key is pressed, then the virtual function
  <verbatim|basic_widget_rep::handle_keypress> is called with an argument of
  type <verbatim|keypress_event>. The default implementation of this virtual
  function does nothing.

  The user can also create his own events, which he can pass to any widget.
  For instance in order to invalidate a region for redrawing, one creates an
  invalidate event using <verbatim|emit_invalidate> and sends it to the
  appropriate widget using the operator <verbatim|\<less\>\<less\>>. Notice
  that the user is responsible for sending events to widgets which can handle
  them. Bad matches are only discovered at run time, in which case an error
  is generated by <verbatim|\<less\>\<less\>>.

  <subsubsection|A simple example>

  Suppose that we want to make a widget, which tracks keyboard events and
  which displays them. Such a widget must have a construction routine and
  keyboard and repaint handlers:\ 

  <\verbatim>
    \ \ \ \ class track_widget_rep: basic_widget_rep {<format|next line>
    \ \ \ \ \ string last_key;<format|next line> \ \ \ \ \ track_widget_rep
    ();<format|next line> \ \ \ \ \ void handle_keypress (keypress_event
    ev);<format|next line> \ \ \ \ \ void handle_repaint (repaint_event
    ev);<format|next line> \ \ \ };
  </verbatim>

  The constructor is taken to be empty and places the origin at the center of
  the widget\ 

  <\verbatim>
    \ \ \ \ track_widget_rep::track_widget_rep (): basic_widget (center) {}
  </verbatim>

  In particular <verbatim|last_key> is initialized by the empty string. We
  also define the function\ 

  <\verbatim>
    \ \ \ \ void<format|next line> \ \ \ track_widget () {<format|next line>
    \ \ \ \ \ return new track_widget_rep ();<format|next line> \ \ \ }
  </verbatim>

  in order to create an instance of a <verbatim|track_widget>.

  The event handler for keyboard events should just reset the string
  <verbatim|last_key> and invalidate the entire widget region.\ 

  <\verbatim>
    \ \ \ \ void<format|next line> \ \ \ track_widget_rep::handle_keypress
    (keypress_event ev) {<format|next line> \ \ \ \ \ last_key=
    ev-\<gtr\>key;<format|next line> \ \ \ \ \ this \<less\>\<less\>
    emit_invalidate_all ();<format|next line> \ \ \ }
  </verbatim>

  The event handler for repainting first determines the string to be
  repainted as a function of <verbatim|last_key>, computes its extents and
  repaints it at the center.\ 

  <\verbatim>
    \ \ \ \ void<format|next line> \ \ \ track_widget_rep::handle_repaint
    (repaint_event ev) {<format|next line> \ \ \ \ \ string s= (last_key ==
    ""? "No key pressed": "Pressed " * last_key);<format|next
    line><format|next line> \ \ \ \ \ SI x1, y1, x2, y2;<format|next line>
    \ \ \ \ \ win-\<gtr\>get_extents (s, x1, y1, x2, y2); // CHECK
    THIS<format|next line><format|next line> \ \ \ \ \ win-\<gtr\>set_color
    (black);<format|next line> \ \ \ \ \ win-\<gtr\>fill (ev-\<gtr\>x1,
    ev-\<gtr\>y1, ev-\<gtr\>x2, ev-\<gtr\>y2);<format|next line>
    \ \ \ \ \ win-\<gtr\>set_color (white);<format|next line>
    \ \ \ \ \ win-\<gtr\>draw_string (s, -(x1+x2)\<gtr\>\<gtr\>1,
    -(y1+y2)\<gtr\>\<gtr\>1);<format|next line> \ \ \ }
  </verbatim>

  \;

  <section|The abstract window interface>

  <subsection|Displays>

  <section|Widget principles>

  <subsection|The widget class>

  Widgets are pointers to instances of the abstract widget representation
  class <verbatim|widget_rep>. Widgets support reference counting, so that a
  widget is automatically destroyed if it is not used any more (except in the
  case of circular referencing; see below). As a general rule, the user does
  not have to worry about the creation and destruction of widgets.

  <subsubsection|The widget representation class>

  The definition of the <verbatim|widget_rep> class goes as follows:\ 

  <\verbatim>
    \ \ \ \ struct widget_rep: rep_struct {<format|next line>
    \ \ \ \ \ window \ \ \ \ \ \ \ win; \ \ \ \ \ \ \ \ \ \ // underlying
    window<format|next line> \ \ \ \ \ SI \ \ \ \ \ \ \ \ \ \ \ ox, oy;
    \ \ \ \ \ \ \ // origin of widget in window<format|next line>
    \ \ \ \ \ SI \ \ \ \ \ \ \ \ \ \ \ w, h; \ \ \ \ \ \ \ \ \ // width and
    height of widget<format|next line> \ \ \ \ \ gravity \ \ \ \ \ \ grav;
    \ \ \ \ \ \ \ \ \ // position of the origin in the widget<format|next
    line> \ \ \ \ \ array\<less\>widget\<gtr\> a; \ \ \ \ \ \ \ \ \ \ \ \ //
    children of widget<format|next line> \ \ \ \ \ array\<less\>string\<gtr\>
    name; \ \ \ \ \ \ \ \ \ // names for the children<format|next
    line><format|next line> \ \ \ \ \ widget_rep (array\<less\>widget\<gtr\>
    a, array\<less\>string\<gtr\> name, gravity grav);<format|next line>
    \ \ \ \ \ virtual ~widget_rep ();<format|next line><format|next line>
    \ \ \ \ \ virtual operator tree () = 0;<format|next line>
    \ \ \ \ \ virtual bool handle (event ev) = 0;<format|next
    line><format|next line> \ \ \ \ \ SI \ \ \ \ \ \ x1 (); SI y1 (); //
    lower left window coordinates of widget<format|next line> \ \ \ \ \ SI
    \ \ \ \ \ \ x2 (); SI y2 (); // upper right window coordinates of
    widget<format|next line> \ \ \ \ \ bool \ \ \ \ attached (); \ \ \ \ //
    tests whether (win != NULL)<format|next line> \ \ \ \ \ volatile void
    fatal_error (string mess, string rout="", string fname="");<format|next
    line> \ \ \ <format|next line> \ \ \ \ \ friend class widget;<format|next
    line> \ \ \ };
  </verbatim>

  \;

  The <verbatim|win> field specifies the window to which the widget is
  attached (<verbatim|win=NULL>, by default). The origin <verbatim|(ox,oy)>
  of the widget is specified with respect to the windows origin. Next come
  the width <verbatim|w> and the height <verbatim|h> of the widget. The
  gravity <verbatim|grav> determines where the origin of the widget is
  located (<verbatim|north_west>, <verbatim|north>, etc.). The array
  <verbatim|a> specifies the children of the widget. The array
  <verbatim|name> gives names to the children of the widget. This is useful
  for addressing children by comprehensible names; the names are also useful
  for designing menu widgets.

  The virtual type casting operator for trees is used for debugging purposes;
  mainly in order to print widgets. The virtual member function
  <verbatim|handle> processes an event which is send to the widget and
  returns <verbatim|TRUE> if the event could be handled and <verbatim|FALSE>
  if not.

  <subsubsection|The widget class>

  The definition of the <verbatim|widget> class goes as follows:\ 

  <\verbatim>
    \ \ \ \ struct widget {<format|next line> \ \ \ #import null_indirect_h
    (widget, widget_rep)<format|next line> \ \ \ \ \ inline widget
    (widget_rep* rep2): rep (rep2) {<format|next line> \ \ \ \ \ \ \ if
    (rep!=NULL) rep-\<gtr\>ref_count++; }<format|next line> \ \ \ \ \ inline
    widget operator [] (int i) { return rep-\<gtr\>a[i]; }<format|next line>
    \ \ \ \ \ \ \ \ \ \ \ \ widget operator [] (string s);<format|next line>
    \ \ \ \ \ inline operator tree () { return (tree) (*rep); }<format|next
    line> \ \ \ \ \ inline bool operator == (widget w) { return rep == w.rep;
    }<format|next line> \ \ \ \ \ inline bool operator != (widget w) { return
    rep != w.rep; }<format|next line> \ \ \ };
  </verbatim>

  \;

  Widgets may be constructed in two ways. First, we may construct a symbolic
  ``nil'' widget by <verbatim|widget ()>. The function <verbatim|bool nil
  (widget)> is provided in order to test whether a widget is ``nil''.
  Secondly, we may construct a widget from a pointer of type
  <verbatim|widget_rep*>.

  The reference counting mechanism ensures widgets to be destroyed when they
  are no longer pointed to. An important exception is when two widgets point
  one to each other, which fools the reference counter (for instance a
  scrollbar and the widget which is scrolled need to point one to each
  other). In order to deal with such ``circular dependencies'', one works
  directly with <verbatim|widget_rep*> pointers if one does not want to the
  pointer to be taken into account in the reference counter.

  Child widgets can again be accessed to in two ways. First, we have the
  direct way, using its index in the array <verbatim|a>. Secondly, we can
  access to a child via its name. Actually, when using this method, a
  <verbatim|get_widget> event is generated. In the basic widget class, the
  default action for this event is to search in the name array for the child.
  However, the user may override this default action and provide another
  child searching method.

  <subsection|The event class>

  Events are pointers to instances of the abstract <verbatim|event_rep>
  class, which supports reference counting. Actually, concrete event
  representation classes just contain some information. Hence, events
  actually provide a safe and generic way to store and communicate
  information.

  <subsubsection|The event representation class>

  The definition of the <verbatim|event_rep> structure is as follows:\ 

  <\verbatim>
    \ \ \ \ struct event_rep: public rep_struct {<format|next line>
    \ \ \ \ \ int \ \ \ type; \ // the event type<format|next line>
    \ \ \ \ \ inline event_rep (int type2): rep_struct (0), type (type2)
    {}<format|next line> \ \ \ \ \ inline virtual ~event_rep ()
    {}<format|next line> \ \ \ \ \ virtual operator tree () = 0; \ \ // for
    displaying events (debugging)<format|next line> \ \ \ };
  </verbatim>

  \;

  The <verbatim|type> field gives the type of the event. A complete list of
  the event types if given in the file\ 

  <\verbatim>
    \ \ \ \ Window/Event/event_codes.hpp
  </verbatim>

  For each project which uses new event types, an analogue file should be
  made and the numbers of the event types should all be different.
  Unfortunately, there is no safe way in order to let this job be done by the
  compiler.

  <subsubsection|The event class>

  The <verbatim|event> structure is defined by\ 

  <\verbatim>
    \ \ \ \ struct event {<format|next line> \ \ \ #import indirect_h (event,
    event_rep) \ \ \ \ \ \ \ \ \ \ \ <format|next line> \ \ \ \ \ inline
    event (event_rep* rep2): rep (rep2) {<format|next line> \ \ \ \ \ \ \ if
    (rep!=NULL) rep-\<gtr\>ref_count++; }<format|next line> \ \ \ \ \ inline
    operator tree () { return (tree) (*rep); }<format|next line>
    \ \ \ };<format|next line> \ \ \ #import indirect_cc (event, event_rep)
  </verbatim>

  \;

  <subsubsection|Concrete event classes>

  Concrete event classes again come into two parts: the class itself and its
  representation class. For instance, the representation class for
  <verbatim|get_widget> events is defined by\ 

  <\verbatim>
    \ \ \ \ struct get_widget_event_rep: public event_rep {<format|next line>
    \ \ \ \ \ string which; widget& w;<format|next line>
    \ \ \ \ \ get_widget_event_rep (string which, widget& w);<format|next
    line> \ \ \ \ \ operator tree ();<format|next line> \ \ \ };
  </verbatim>

  The corresponding <verbatim|get_widget_event> class is defined by\ 

  <\verbatim>
    \ \ \ \ #import event (get_widget_event, get_widget_event_rep)
  </verbatim>

  \;

  The module <verbatim|event> with two parameters is defined by\ 

  <\verbatim>
    \ \ \ \ #module event (T, R)<format|next line> \ \ \ struct T
    {<format|next line> \ \ \ \ \ R* rep;<format|next line> \ \ \ \ \ T (T&
    ev);<format|next line> \ \ \ \ \ ~T ();<format|next line> \ \ \ \ \ T
    (event& ev);<format|next line> \ \ \ \ \ operator event ();<format|next
    line> \ \ \ \ \ R* operator -\<gtr\> ();<format|next line> \ \ \ \ \ T&
    operator = (T ev);<format|next line> \ \ \ };<format|next line>
    \ \ \ #endmodule // event (T, R)
  </verbatim>

  The important thing to notice is that we have converters from and to the
  generic <verbatim|event> class. Moreover, the generic <verbatim|event>
  class and the specific <verbatim|get_widget_event> class are compatible
  from the reference counting point of view.

  The implementation of the <verbatim|get_widget_event_rep> class is as
  follows:\ 

  <\verbatim>
    \ \ \ \ get_widget_event_rep::get_widget_event_rep (string ww, widget&
    w2):<format|next line> \ \ \ \ \ event_rep (GET_WIDGET_EVENT), which
    (ww), w (w2) {}<format|next line> \ \ \ get_widget_event_rep::operator
    tree () {<format|next line> \ \ \ \ \ return tree ("get_widget_event",
    which); }<format|next line> \ \ \ #import code_event (get_widget_event,
    get_widget_event_rep)
  </verbatim>

  The actual events are created by\ 

  <\verbatim>
    \ \ \ \ event get_widget (string which, widget& w) {<format|next line>
    \ \ \ \ \ return new get_widget_event_rep (which, w); }
  </verbatim>

  \;

  <subsubsection|Event handlers>

  Implementations of the generic event handler <verbatim|bool
  widget_rep::handle(event)> usually do the following

  <\itemize>
    <item>Determine the event type.

    <item>Perform some action, depending on the event type and the widget.

    <item>Dispatch the event to a concrete or abstract specific event handler
    or to the generic event handler of some other widget representation
    class.
  </itemize>

  For instance, the event handler for composite widgets is as follows:\ 

  <\verbatim>
    \ \ \ \ bool<format|next line> \ \ \ composite_widget_rep::handle (event
    ev) {<format|next line> \ \ \ \ \ switch (ev-\<gtr\>type) {<format|next
    line> \ \ \ \ \ case CLEAN_EVENT:<format|next line>
    \ \ \ \ \ \ \ handle_clean (ev);<format|next line> \ \ \ \ \ \ \ return
    TRUE;<format|next line> \ \ \ \ \ case INSERT_EVENT:<format|next line>
    \ \ \ \ \ \ \ handle_insert (ev);<format|next line> \ \ \ \ \ \ \ return
    TRUE;<format|next line> \ \ \ \ \ case REMOVE_EVENT:<format|next line>
    \ \ \ \ \ \ \ handle_remove (ev);<format|next line> \ \ \ \ \ \ \ return
    TRUE;<format|next line> \ \ \ \ \ }<format|next line> \ \ \ \ \ return
    basic_widget_rep::handle (ev);<format|next line> \ \ \ }
  </verbatim>

  The member function <verbatim|handle_insert> is implemented as follows:\ 

  <\verbatim>
    \ \ \ \ void<format|next line> \ \ \ composite_widget_rep::handle_insert
    (insert_event ev) {<format|next line> \ \ \ \ \ a \<less\>\<less\>
    ev-\<gtr\>w;<format|next line> \ \ \ \ \ name \<less\>\<less\>
    ev-\<gtr\>s;<format|next line> \ \ \ }
  </verbatim>

  In particular, we can retrieve the fields <verbatim|w> and <verbatim|s>
  from <verbatim|insert_event_rep> from the <verbatim|insert_event> in the
  member function.

  <subsubsection|Adding your own event classes>

  Summarizing, in order to add your own new event classes, you have to take
  care of the following steps:

  <\itemize>
    <item>Add a new event type to some <verbatim|event_codes> file.

    <item>Declare and implement the event type and its representation type.

    <item>Declare and implement the event creation functions.

    <item>Reimplement the generic event handler <verbatim|bool
    widget_rep::handle(event)> in the abstract or concrete widget
    representation class, where you want to use your new event class.
  </itemize>

  <subsection|The main event loop>

  The main event loop does the following

  <\itemize>
    <item>As long as the application did not destroy all its windows, wait
    for a new event to occur.

    <item>If an event occurs, handle all events on the queue, by creating the
    appropriate events and sending them to the appropriate widgets (job of
    the window interface implementation).

    <item>If there are no events left, send an <verbatim|inquire_event> to
    each window. This is useful for complex applications, where regions of
    the window may be invalidated during this phase. Indeed, the event
    handling phase may consist of many complex actions, so that the regions
    to invalidate may be determined easier <with|font shape|italic|a
    posteriori>.

    <item>Requests are emitted in order to repaint the regions which have
    been invalidated during the event processing stages.
  </itemize>

  <subsection|Coordinates>

  <subsubsection|Coordinates, pixels and rounding>

  All coordinates and sizes are represented by instances of type
  <verbatim|SI>, which is nothing but another name for <verbatim|int>. The
  <verbatim|SI> constant <verbatim|PIXEL>, which is a power of two
  <verbatim|1 \<less\>\<less\> PIXEL_SHIFT> denotes the size of a pixel on
  the screen. Since <verbatim|PIXEL\<gtr\>1>, coordinates and sizes are not
  necessarily integer multiples of the pixel size. However, the coordinates
  of the origin and the size of a widget should always be such multiples.

  In order to achieve this, some rounding functions are provided. The
  function <verbatim|round (SI&)> rounds the argument to an integer multiple
  of <verbatim|PIXEL>. Furthermore, the <verbatim|window> member functions\ 

  <\verbatim>
    \ \ \ \ void inner_round (SI& x1, SI& y1, SI& x2, SI& y2);<format|next
    line> \ \ \ void outer_round (SI& x1, SI& y1, SI& x2, SI& y2);
  </verbatim>

  transform a rectangle into a new one with integer multiple of
  <verbatim|PIXEL> coordinates, which is enclosed resp. encloses the original
  rectangle.

  <subsubsection|Local and global coordinates>

  Each widget has an origin <verbatim|(ox,oy)> with respect to the window to
  which it has been attached. This is the origin of the ``local
  coordinates''. The origin of the ``global coordinates'' is the origin of
  the window. The location of the local origin in the widget is determined by
  the widget's gravity, which is either one of <verbatim|north_west>,
  <verbatim|north>, <verbatim|north_east>, <verbatim|west>,
  <verbatim|center>, <verbatim|east>, <verbatim|south_west>, <verbatim|south>
  or <verbatim|south_east>.

  As a general rule, events are transmitted in global coordinates.
  Nevertheless, in widgets which are derived from the abstract basic widget
  class, by default, all computations are done with respect to local
  coordinates. This is due to two reasons

  <\itemize>
    <item>When an event has to be processed, the abstract widget event
    handler translates global into local coordinates and calls the
    appropriate virtual event handler using local coordinates.

    <item>When an event has to be emitted, the abstract widget provides event
    construction routines w.r.t. local coordinates, which override the global
    event construction routines w.r.t global coordinates.
  </itemize>

  <subsubsection|Screen coordinates>

  For some very particular purposes, such as popping up windows, one has to
  perform computations with respect to the screen coordinates. Given a point
  <verbatim|(x,y)> in the coordinates of some window <verbatim|win>, the
  screen coordinates of <verbatim|(x,y)> are obtained by adding the windows
  origin, which is obtained by calling <verbatim|win-\<gtr\>get_position
  (ox,oy)>.

  <subsection|Attaching and positioning widgets>

  <subsubsection|Attaching widgets>

  When a widget is created, the <verbatim|win> field of its representation is
  set to <verbatim|NULL>, since it is not yet attached to a window. In order
  to attach a widget <verbatim|w> to a window <verbatim|win>, one emits an
  <verbatim|attach_window_event>:\ 

  <\verbatim>
    \ \ \ \ w \<less\>\<less\> emit_attach_window (win);
  </verbatim>

  Notice that taking <verbatim|win==NULL> results in detaching the widget.
  Notice also that a widget may be attached to at most one window: attempts
  to reattach a widget, which is already attached, to another window, result
  in a fatal error.

  Some events can be handled by widgets which are not yet attached to a
  window, such as:

  <\itemize>
    <item>``get size'' events, which determine the default, minimal and
    maximal size of a widget. Such an event is generated before the creation
    of the window to which the widget will be attached in order to determine
    the size of the window.

    <item>``attach window'' events, in order to attach (or detach) a window.

    <item>Events for setting (and getting) attributes: after the creation of
    a widget some attributes of the widget may be given some default value.
    In order to change them, one might wish to set them to other values
    before attaching the widget to a window.

    <item>Events for modifying the composite structure of a widget: these
    events are used for instance in order to construct menus.
  </itemize>

  For some of these events, such as attribute changes, it may be necessary to
  emit invalidate events in case when the widget had been attached to some
  window. In order to test this one uses the member function\ 

  <\verbatim>
    \ \ \ \ bool widget_rep::attached ();
  </verbatim>

  \;

  <subsubsection|Positioning widgets>

  When an appropriate size <verbatim|(w,h)> has been determined for a widget
  (using ``get size'' events) and when a widget has been attached to some
  window, the widget is positioned in the main window. By default, all
  children are recursively positioned at the top left of the window at sizes
  <verbatim|(w,h)>. But for complex widgets with children, a specific
  positioning routine usually has to be implemented.

  Such a routine involves positioning of the children within the parent. This
  is done by emitting position events to the children. For instance,\ 

  <\verbatim>
    \ \ \ \ a[i] \<less\>\<less\> emit_position (x[i], y[i], w[i], h[i],
    center);
  </verbatim>

  positions the <verbatim|i>-th child, such that the origin of
  <verbatim|a[i]> is at position <verbatim|(x[i], y[i])> w.r.t. the local
  coordinates of <verbatim|this> and such that the origin is situated in the
  center of <verbatim|a[i]>. The width and height of <verbatim|a[i]> are set
  to <verbatim|w[i]> resp. <verbatim|h[i]>.

  <subsubsection|Repositioning widgets>

  During execution, it may happen that a particular widget has changed, so
  that it obtains a different size and/or position. In this case, one emits
  an <verbatim|update_event> to the closest ancestor, whose position and size
  did not change.

  For instance, consider the case of a footer <verbatim|footer>, which
  consists of a left footer <verbatim|footer["left"]>, followed by some glue
  <verbatim|footer["middle"]> and a right footer <verbatim|footer["right"]>.
  When the left footer changes:\ 

  <\verbatim>
    \ \ \ \ footer \<less\>\<less\> set_widget ("left", text_widget ("new
    text"));
  </verbatim>

  the size of <verbatim|footer["left"]> changes, and the size and position of
  the glue should also be changed. Nevertheless, the size and position of
  <verbatim|footer> remain unaltered, whence we update <verbatim|footer>:\ 

  <\verbatim>
    \ \ \ \ footer \<less\>\<less\> emit_update ();
  </verbatim>

  \;

  Updating an attached widget results in three actions to take place:

  <\itemize>
    <item>The widget is reattached to its own window. Indeed, some children
    of the widget might need be attached.

    <item>The widget is repositioned at its current position and size. Again
    this will actually affect the children.

    <item>The widget is invalidated, so that it will be repainted.
  </itemize>

  <subsection|The keyboard>

  <subsubsection|Keyboard focus>

  Each window <verbatim|win> on the screen determines a main widget
  <verbatim|win-\<gtr\>w> which is attached to it and a descendant
  <verbatim|win-\<gtr\>kbd_focus> of this widget, which handles the keyboard
  input directed to the window. This latter widget
  <verbatim|win-\<gtr\>kbd_focus>, which is set to <verbatim|win-\<gtr\>w> by
  default, is said to have keyboard focus, if the window <verbatim|win> has
  keyboard focus (i.e. if all keyboard events are sent to this window).
  Consequently, the widget which has keyboard focus receives all keyboard
  events.

  When the keyboard focus of a window <verbatim|win> changes, a
  <verbatim|keyboard_focus_event> is sent to <verbatim|win-\<gtr\>kbd_focus>.
  The field <verbatim|ev-\<gtr\>flag> of this event <verbatim|ev> is
  <verbatim|TRUE> if the window got the focus, and <verbatim|FALSE> if the
  window lost focus.

  The keyboard focus widget <verbatim|win-\<gtr\>kbd_focus> associated to a
  window can be changed by calling the <verbatim|window_rep> member function\ 

  <\verbatim>
    \ \ \ \ void window_rep::set_keyboard_focus (widget);
  </verbatim>

  Setting the input focus to another widget than <verbatim|win-\<gtr\>w> is
  useful, for instance, if a particular text input field of some form needs
  keyboard focus after a mouse click on it.

  <subsubsection|Keyboard events>

  When a widget has the keyboard focus, and a key is pressed, it receives a
  <verbatim|keypress_event>. The <verbatim|keypress_event_rep> class contains
  a field <verbatim|key>, which contains a comprehensible string
  corresponding to the key which was pressed.

  More precisely, <verbatim|key> is either a one character string, or a
  symbolic name like <verbatim|"\<less\>return\<gtr\>">,
  <verbatim|"\<less\>right\<gtr\>">, <verbatim|"\<less\>del\<gtr\>">, etc. or
  a composed name like <verbatim|"\<less\>shift-F1\<gtr\>">,
  <verbatim|"\<less\>ctrl-esc\<gtr\>"> or <verbatim|"\<less\>meta-x\<gtr\>">.
  The complete list of keys is as follows:\ 

  <\verbatim>
    \ \ \ \ "\<less\>F1\<gtr\>", "\<less\>F2\<gtr\>", "\<less\>F3\<gtr\>",
    "\<less\>F4\<gtr\>", "\<less\>F5\<gtr\>",
    "\<less\>F6\<gtr\>",<format|next line> \ \ \ "\<less\>F7\<gtr\>",
    "\<less\>F8\<gtr\>", "\<less\>F9\<gtr\>", "\<less\>F10\<gtr\>",
    "\<less\>F11\<gtr\>", "\<less\>F12\<gtr\>",<format|next line>
    \ \ \ "\<less\>esc\<gtr\>", "\<less\>tab\<gtr\>", "\<less\>less\<gtr\>",
    "\<less\>gtr\<gtr\>", "\<less\>del\<gtr\>",
    "\<less\>return\<gtr\>",<format|next line> \ \ \ "\<less\>ins\<gtr\>",
    "\<less\>home\<gtr\>", "\<less\>end\<gtr\>", "\<less\>page-down\<gtr\>",
    "\<less\>page-up\<gtr\>",<format|next line> \ \ \ "\<less\>left\<gtr\>",
    "\<less\>up\<gtr\>", "\<less\>down\<gtr\>", "\<less\>right\<gtr\>"
  </verbatim>

  The keys <verbatim|"\<less\>less\<gtr\>"> and
  <verbatim|"\<less\>gtr\<gtr\>"> correspond resp. to <verbatim|"\<less\>">
  and <verbatim|"\<gtr\>">. The allowed modifiers are <verbatim|"shift">,
  <verbatim|"ctrl"> and <verbatim|"meta"> or combinations of these.

  <subsection|The mouse>

  <subsubsection|Mouse events>

  A mouse event <verbatim|ev> occurs on a button change or a mouse movement.
  The <verbatim|ev-\<gtr\>type> field contains the type of the event and
  <verbatim|ev-\<gtr\>x> and <verbatim|ev-\<gtr\>y> the corresponding
  coordinates of the mouse. Finally, the states of the mouse buttons can be
  questioned using the routine <verbatim|ev-\<gtr\>pressed (string)>.

  The possible values of <verbatim|ev-\<gtr\>type> on button change events
  are the following:\ 

  <\verbatim>
    \ \ \ \ "press-left", "press-middle", "press-right",<format|next line>
    \ \ \ "release-left", "release-middle", "release-right"
  </verbatim>

  The possible values for mouse movement events are\ 

  <\verbatim>
    \ \ \ \ "move", "enter", "leave"
  </verbatim>

  The <verbatim|"enter"> and <verbatim|"leave"> events occur when the mouse
  enters resp. leaves the widget. Finally, the states of the left, middle and
  right mouse buttons can respectively be obtained using the calls\ 

  <\verbatim>
    \ \ \ \ ev-\<gtr\>pressed ("left")<format|next line>
    \ \ \ ev-\<gtr\>pressed ("middle")<format|next line>
    \ \ \ ev-\<gtr\>pressed ("right")<format|next line>\ 
  </verbatim>

  <subsubsection|Grabbing the mouse>

  For some applications such as popup menus or scrollbars, it is useful to
  direct all mouse events to a particular widget <verbatim|w>. This is done
  by grabbing the mouse by emitting the event\ 

  <\verbatim>
    \ \ \ \ w \<less\>\<less\> emit_grab_mouse (TRUE)
  </verbatim>

  After such a grab, all mouse events are directed to <verbatim|w>, even
  those events which occurred before the grab (contrary to X Window). The
  mouse grab is released by\ 

  <\verbatim>
    \ \ \ \ w \<less\>\<less\> emit_grab_mouse (FALSE)
  </verbatim>

  \;

  Actually, the display keeps track of a list of widgets for which a mouse
  grab occurred: if the mouse is grabbed by widgets <verbatim|w1> next
  <verbatim|w2>, and again ungrabbed by <verbatim|w2>, then all mouse events
  are again sent to <verbatim|w1>. This feature is useful for successive
  grabs by recursive popup menus.

  When a widget <verbatim|w1> grabs the mouse, and a previous mouse grab on a
  widget <verbatim|w2> is still active, then a <verbatim|"leave"> event is
  sent to <verbatim|w2> and an <verbatim|"enter"> event to <verbatim|w1>.
  Similarly, if <verbatim|w1> releases the grab, then a <verbatim|"leave">
  event is sent to <verbatim|w1> and an <verbatim|"enter"> event to
  <verbatim|w2>.

  <subsection|The screen>

  Each window keeps track of a list of rectangles to be repainted (moreover,
  redundant rectangles are eliminated automatically and adjacent rectangles
  are transformed in larger rectangles). During the repaint stage in the
  event loop, the widget is requested to repaint these rectangles.

  <subsubsection|Repainting rectangles>

  The repaint handler takes on input a <verbatim|repaint_event ev>, which
  determines the rectangle to be repainted. Moreover,
  <verbatim|repaint_event_rep> contains a boolean field <verbatim|stop>,
  which can be set in order to indicate that the repaint process was stopped
  somewhere in the middle.

  Indeed, for widgets which take a long time to be repainted, it may be
  useful to abort repainting if a key is pressed. The arrival of an event
  which aborts repainting can be checked directly on the postscript device
  <verbatim|dev>:\ 

  <\verbatim>
    \ \ \ \ if (dev-\<gtr\>check_event (EVENT_STATUS)) { ... } // CHECK THIS
  </verbatim>

  In the case of a window, such an event may signify that a key has been
  pressed; in the case of a printer, it might suggest the printer being
  turned off.

  If the application decides to abort repainting, it sets
  <verbatim|ev-\<gtr\>stop> to <verbatim|TRUE>. The rectangle which was being
  repainted is put back on the invalid rectangles list in the event loop; it
  will be processed again during the next pass through the repaint phase.

  <subsubsection|Invalidation of rectangles>

  When window is mapped on the screen or when a region is exposed, the window
  interface automatically invalidates the corresponding rectangle. The user
  may also invalidate a rectangle by using either one of the routines\ 

  <\verbatim>
    \ \ \ \ event emit_invalidate_all ();<format|next line> \ \ \ event
    emit_invalidate (SI x1, SI y1, SI x2, SI y2);
  </verbatim>

  The first routine creates an event to invalidate the entire widget area;
  the other routine invalidates a specified region.

  <subsection|The toolkit>

  <subsubsection|Other standard widget classes>

  Many widgets from the toolkit are derived from some other standard abstract
  widget classes, which can handle some other special events.

  <subsubsection|Composite widgets>

  These widgets allow to add or remove children to or from a widget. This
  makes them particularly useful for menu widgets. They respond to
  <verbatim|clean>, <verbatim|insert> and <verbatim|remove> events.

  <subsubsection|Attribute widgets>

  These widgets allow to set window attributes of some common types such as
  integers, strings, commands, points, etc. They can be used for instance to
  retrieve an input string or in order to set the scroll position in a canvas
  widget.

  <subsubsection|Glue widgets>

  Glue widgets are created by\ 

  <\verbatim>
    \ \ \ \ widget glue_widget (bool hext=TRUE, bool vext=TRUE, SI w=0, SI
    h=0);
  </verbatim>

  The first two arguments determine whether the widget is extensible
  horizontally resp. vertically. The last two elements determine the default
  and minimal size of the widget.

  <subsubsection|Text widgets>

  Text widgets are created using\ 

  <\verbatim>
    \ \ \ \ widget text_widget (string s);
  </verbatim>

  They just display the text <verbatim|s>.

  <subsubsection|Buttons>

  Two types of buttons have been implemented. First, command buttons are
  created using\ 

  <\verbatim>
    \ \ \ \ widget command_button (string s, command cmd);
  </verbatim>

  They display the text <verbatim|s> and execute the command <verbatim|cmd>
  when pressed. Secondly, we implemented popup buttons, which popup some
  window when pressed. Popup buttons are created by one of\ 

  <\verbatim>
    \ \ \ \ widget pulldown_button (string s, widget m);<format|next line>
    \ \ \ widget pullright_button (string s, widget m);
  </verbatim>

  depending on where the popup window should popup. The main widget attached
  to the popup window should be created using\ 

  <\verbatim>
    \ \ \ \ widget popup_widget (widget w, gravity quit);
  </verbatim>

  The <verbatim|quit> argument specifies that the popup window should
  disappear as soon as the pointer leaves the widget in the <verbatim|quit>
  direction.

  <subsubsection|Menus>

  Horizontal and vertical menus are created using\ 

  <\verbatim>
    \ \ \ \ widget horizontal_menu ();<format|next line> \ \ \ widget
    vertical_menu ();
  </verbatim>

  By default, they are empty. Subsequently, they can be modified as composite
  widgets.

  <subsubsection|Canvas widgets>

  Canvas widgets are created using\ 

  <\verbatim>
    \ \ \ \ widget canvas_widget (widget w);
  </verbatim>

  Canvas widget consist of a portion of the widget <verbatim|w> and
  scrollbars, which enable to scroll <verbatim|w>. The events\ 

  <\verbatim>
    \ \ \ \ event set_scrollable (widget w);<format|next line> \ \ \ event
    set_extents \ \ \ (SI ew, SI ey);<format|next line> \ \ \ event
    set_scroll_pos (SI x, SI y);<format|next line> \ \ \ event get_extents
    \ \ \ (SI& ew, SI& eh);<format|next line> \ \ \ event get_visible
    \ \ \ (SI& x1, SI& y1, SI& x2, SI& y2);
  </verbatim>

  enable to change <verbatim|w>, to set the extents of <verbatim|w>, to set
  the scroll position, to get the extents of <verbatim|w> and to get the
  rectangle of <verbatim|w>, which is currently visible.

  <subsubsection|Input widgets>

  Input widgets enable to type a string and to retrieve it when finished.
  They are created using\ 

  <\verbatim>
    \ \ \ \ widget input_text_widget (command call_back);
  </verbatim>

  Some initial text can be put in it using\ 

  <\verbatim>
    \ \ \ \ event set_input_string (string s);
  </verbatim>

  The command <verbatim|call_back> is executed when typing has been finished
  or aborted (by typing return, escape or ctrl-c). The typed string can then
  be retrieved using\ 

  <\verbatim>
    \ \ \ \ event get_input_string (string& s);
  </verbatim>

  Usually, the returned <verbatim|s> is a string enclosed between quotes. If
  typing was aborted, <verbatim|s> contains the string <verbatim|"cancel">.

  <apply|tmdoc-copyright|1998--2003|Joris van der Hoeven|Mich�le Garoche>

  <expand|tmdoc-license|Permission is granted to copy, distribute and/or
  modify this document under the terms of the GNU Free Documentation License,
  Version 1.1 or any later version published by the Free Software Foundation;
  with no Invariant Sections, with no Front-Cover Texts, and with no
  Back-Cover Texts. A copy of the license is included in the section entitled
  "GNU Free Documentation License".>
</body>

<\initial>
  <\collection>
    <associate|paragraph width|150mm>
    <associate|odd page margin|30mm>
    <associate|shrinking factor|4>
    <associate|page right margin|30mm>
    <associate|page top margin|30mm>
    <associate|reduction page right margin|25mm>
    <associate|page type|a4>
    <associate|reduction page bottom margin|15mm>
    <associate|even page margin|30mm>
    <associate|reduction page left margin|25mm>
    <associate|page bottom margin|30mm>
    <associate|reduction page top margin|15mm>
    <associate|language|french>
  </collection>
</initial>

<\references>
  <\collection>
    <associate|idx-1|<tuple|<uninit>|?>>
    <associate|idx-2|<tuple|<uninit>|?>>
    <associate|toc-40|<tuple|3.9.4|?>>
    <associate|toc-10|<tuple|3.1.1|?>>
    <associate|toc-20|<tuple|3.4.1|?>>
    <associate|toc-30|<tuple|3.7|?>>
    <associate|toc-31|<tuple|3.7.1|?>>
    <associate|toc-41|<tuple|3.9.5|?>>
    <associate|toc-21|<tuple|3.4.2|?>>
    <associate|toc-11|<tuple|3.1.2|?>>
    <associate|toc-42|<tuple|3.9.6|?>>
    <associate|toc-32|<tuple|3.7.2|?>>
    <associate|toc-12|<tuple|3.2|?>>
    <associate|toc-22|<tuple|3.4.3|?>>
    <associate|toc-43|<tuple|3.9.7|?>>
    <associate|toc-33|<tuple|3.8|?>>
    <associate|toc-13|<tuple|3.2.1|?>>
    <associate|toc-23|<tuple|3.5|?>>
    <associate|toc-44|<tuple|3.9.8|?>>
    <associate|toc-34|<tuple|3.8.1|?>>
    <associate|toc-14|<tuple|3.2.2|?>>
    <associate|toc-24|<tuple|3.5.1|?>>
    <associate|toc-45|<tuple|3.9.9|?>>
    <associate|toc-35|<tuple|3.8.2|?>>
    <associate|toc-15|<tuple|3.2.3|?>>
    <associate|toc-25|<tuple|3.5.2|?>>
    <associate|toc-46|<tuple|<uninit>|?>>
    <associate|toc-36|<tuple|3.9|?>>
    <associate|toc-16|<tuple|3.2.4|?>>
    <associate|toc-26|<tuple|3.5.3|?>>
    <associate|toc-17|<tuple|3.2.5|?>>
    <associate|toc-27|<tuple|3.6|?>>
    <associate|toc-37|<tuple|3.9.1|?>>
    <associate|toc-38|<tuple|3.9.2|?>>
    <associate|toc-18|<tuple|3.3|?>>
    <associate|toc-28|<tuple|3.6.1|?>>
    <associate|toc-39|<tuple|3.9.3|?>>
    <associate|toc-19|<tuple|3.4|?>>
    <associate|toc-29|<tuple|3.6.2|?>>
    <associate|toc-1|<tuple|1|?>>
    <associate|toc-2|<tuple|1.1|?>>
    <associate|toc-3|<tuple|1.1.1|?>>
    <associate|toc-4|<tuple|1.2|?>>
    <associate|toc-5|<tuple|1.2.1|?>>
    <associate|toc-6|<tuple|2|?>>
    <associate|toc-7|<tuple|2.1|?>>
    <associate|toc-8|<tuple|3|?>>
    <associate|toc-9|<tuple|3.1|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|toc>
      <vspace*|1fn><with|font series|<quote|bold>|math font
      series|<quote|bold>|1<space|2spc>Introduction><value|toc-dots><pageref|toc-1><vspace|0.5fn>

      1.1<space|2spc>Main architecture<value|toc-dots><pageref|toc-2>

      <with|left margin|<quote|1.5fn>|1.1.1<space|2spc>A simple
      example<value|toc-dots><pageref|toc-3>>

      1.2<space|2spc>Widgets and event processing<value|toc-dots><pageref|toc-4>

      <with|left margin|<quote|1.5fn>|1.2.1<space|2spc>A simple
      example<value|toc-dots><pageref|toc-5>>

      <vspace*|1fn><with|font series|<quote|bold>|math font
      series|<quote|bold>|2<space|2spc>The abstract window
      interface><value|toc-dots><pageref|toc-6><vspace|0.5fn>

      2.1<space|2spc>Displays<value|toc-dots><pageref|toc-7>

      <vspace*|1fn><with|font series|<quote|bold>|math font
      series|<quote|bold>|3<space|2spc>Widget
      principles><value|toc-dots><pageref|toc-8><vspace|0.5fn>

      3.1<space|2spc>The widget class<value|toc-dots><pageref|toc-9>

      <with|left margin|<quote|1.5fn>|3.1.1<space|2spc>The widget
      representation class<value|toc-dots><pageref|toc-10>>

      <with|left margin|<quote|1.5fn>|3.1.2<space|2spc>The widget
      class<value|toc-dots><pageref|toc-11>>

      3.2<space|2spc>The event class<value|toc-dots><pageref|toc-12>

      <with|left margin|<quote|1.5fn>|3.2.1<space|2spc>The event
      representation class<value|toc-dots><pageref|toc-13>>

      <with|left margin|<quote|1.5fn>|3.2.2<space|2spc>The event
      class<value|toc-dots><pageref|toc-14>>

      <with|left margin|<quote|1.5fn>|3.2.3<space|2spc>Concrete event
      classes<value|toc-dots><pageref|toc-15>>

      <with|left margin|<quote|1.5fn>|3.2.4<space|2spc>Event
      handlers<value|toc-dots><pageref|toc-16>>

      <with|left margin|<quote|1.5fn>|3.2.5<space|2spc>Adding your own event
      classes<value|toc-dots><pageref|toc-17>>

      3.3<space|2spc>The main event loop<value|toc-dots><pageref|toc-18>

      3.4<space|2spc>Coordinates<value|toc-dots><pageref|toc-19>

      <with|left margin|<quote|1.5fn>|3.4.1<space|2spc>Coordinates, pixels
      and rounding<value|toc-dots><pageref|toc-20>>

      <with|left margin|<quote|1.5fn>|3.4.2<space|2spc>Local and global
      coordinates<value|toc-dots><pageref|toc-21>>

      <with|left margin|<quote|1.5fn>|3.4.3<space|2spc>Screen
      coordinates<value|toc-dots><pageref|toc-22>>

      3.5<space|2spc>Attaching and positioning
      widgets<value|toc-dots><pageref|toc-23>

      <with|left margin|<quote|1.5fn>|3.5.1<space|2spc>Attaching
      widgets<value|toc-dots><pageref|toc-24>>

      <with|left margin|<quote|1.5fn>|3.5.2<space|2spc>Positioning
      widgets<value|toc-dots><pageref|toc-25>>

      <with|left margin|<quote|1.5fn>|3.5.3<space|2spc>Repositioning
      widgets<value|toc-dots><pageref|toc-26>>

      3.6<space|2spc>The keyboard<value|toc-dots><pageref|toc-27>

      <with|left margin|<quote|1.5fn>|3.6.1<space|2spc>Keyboard
      focus<value|toc-dots><pageref|toc-28>>

      <with|left margin|<quote|1.5fn>|3.6.2<space|2spc>Keyboard
      events<value|toc-dots><pageref|toc-29>>

      3.7<space|2spc>The mouse<value|toc-dots><pageref|toc-30>

      <with|left margin|<quote|1.5fn>|3.7.1<space|2spc>Mouse
      events<value|toc-dots><pageref|toc-31>>

      <with|left margin|<quote|1.5fn>|3.7.2<space|2spc>Grabbing the
      mouse<value|toc-dots><pageref|toc-32>>

      3.8<space|2spc>The screen<value|toc-dots><pageref|toc-33>

      <with|left margin|<quote|1.5fn>|3.8.1<space|2spc>Repainting
      rectangles<value|toc-dots><pageref|toc-34>>

      <with|left margin|<quote|1.5fn>|3.8.2<space|2spc>Invalidation of
      rectangles<value|toc-dots><pageref|toc-35>>

      3.9<space|2spc>The toolkit<value|toc-dots><pageref|toc-36>

      <with|left margin|<quote|1.5fn>|3.9.1<space|2spc>Other standard widget
      classes<value|toc-dots><pageref|toc-37>>

      <with|left margin|<quote|1.5fn>|3.9.2<space|2spc>Composite
      widgets<value|toc-dots><pageref|toc-38>>

      <with|left margin|<quote|1.5fn>|3.9.3<space|2spc>Attribute
      widgets<value|toc-dots><pageref|toc-39>>

      <with|left margin|<quote|1.5fn>|3.9.4<space|2spc>Glue
      widgets<value|toc-dots><pageref|toc-40>>

      <with|left margin|<quote|1.5fn>|3.9.5<space|2spc>Text
      widgets<value|toc-dots><pageref|toc-41>>

      <with|left margin|<quote|1.5fn>|3.9.6<space|2spc>Buttons<value|toc-dots><pageref|toc-42>>

      <with|left margin|<quote|1.5fn>|3.9.7<space|2spc>Menus<value|toc-dots><pageref|toc-43>>

      <with|left margin|<quote|1.5fn>|3.9.8<space|2spc>Canvas
      widgets<value|toc-dots><pageref|toc-44>>

      <with|left margin|<quote|1.5fn>|3.9.9<space|2spc>Input
      widgets<value|toc-dots><pageref|toc-45>>
    </associate>
  </collection>
</auxiliary>
