
/******************************************************************************
* MODULE     : gui.hpp
* DESCRIPTION: Abstract interface to various GUIs.
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This module contains all system-wide routines of GUIs.
* An abstract widget interface can be found in widget.hpp / message.hpp and
* an abstract interface for drawing primitives in renderer.hpp.
* The Widkit plug-in provides a default implementation for a widget library.
* When using Widkit, you should provide a simple window implementation,
* as specified in window.hpp.
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#ifndef GUI_H
#define GUI_H
#include "tree.hpp"
#include "bitmap_font.hpp"
#include "timer.hpp"

struct font;
class widget;
typedef int color;

/******************************************************************************
* Main routines
******************************************************************************/

void gui_open (int argc= 0, char** argv= NULL);
  // start the gui
void gui_interpose (void (*) (void));
  // specify an interpose routine for the main loop
void gui_start_loop ();
  // start the main loop
void gui_close ();
  // cleanly close the gui
void gui_root_extents (SI& width, SI& height);
  // get the screen size
void gui_maximal_extents (SI& width, SI& height);
  // get the maximal size of a window (can be larger than the screen size)
void gui_refresh ();
  // update and redraw all windows (e.g. on change of output language)

/******************************************************************************
* Colors
******************************************************************************/

extern color black, white, red, green, blue;
extern color yellow, magenta, orange, brown, pink;
extern color light_grey, grey, dark_grey;

color  rgb_color (int r, int g, int b);
  // get a color by its RGB components ranging from 0 to 255 included
void   get_rgb_color (color col, int& r, int& g, int& b);
  // get the RGB components of a color
color  named_color (string s);
  // get a color by its name
string get_named_color (color c);
  // get a standard name for the color if it exists

/******************************************************************************
* Font support
******************************************************************************/

void set_default_font (string name);
  // set the name of the default font
font get_default_font (bool tt= false);
  // get the default font or monospaced font (if tt is true)
void load_system_font (string family, int size, int dpi,
		       font_metric& fnm, font_glyphs& fng);
  // load the metric and glyphs of a system font
  // you are not obliged to provide any system fonts

/******************************************************************************
* Clipboard support
******************************************************************************/

bool set_selection (string cb, tree t, string s);
  // Copy a selection 't' with string equivalent 's' to the clipboard 'cb'
  // Returns true on success
bool get_selection (string cb, tree& t, string& s);
  // Retrieve the selection 't' with string equivalent 's' from clipboard 'cb'
  // Returns true on success; sets t to (extern s) for external selections
void clear_selection (string cb);
  // Clear the selection on clipboard 'cb'

/******************************************************************************
* Miscellaneous
******************************************************************************/

#define INTERRUPT_EVENT   0
#define INTERRUPTED_EVENT 1
#define ANY_EVENT         2
#define DRAG_EVENT        3
#define MOTION_EVENT      4
#define MENU_EVENT        5

void beep ();
  // Issue a beep
bool check_event (int type);
  // Check whether an event of one of the above types has occurred;
  // we check for keyboard events while repainting windows
void image_gc (string name= "*");
  // Garbage collect images of a given name (may use wildcards)
  // This routine only needs to be implemented if you use your own image cache
void show_help_balloon (widget balloon, SI x, SI y);
  // Display a help balloon at position (x, y); the help balloon should
  // disappear as soon as the user presses a key or moves the mouse
void show_wait_indicator (widget base, string message, string argument);
  // Display a wait indicator with a message and an optional argument
  // The indicator might for instance be displayed at the center of
  // the base widget which triggered the lengthy operation;
  // the indicator should be removed if the message is empty

#endif // defined GUI_H
