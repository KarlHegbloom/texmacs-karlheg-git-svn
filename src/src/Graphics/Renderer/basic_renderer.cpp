
/******************************************************************************
* MODULE     : basic_renderer.cpp
* DESCRIPTION: common drawing interface class
* COPYRIGHT  : (C) 2008 Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#if (defined(QTTEXMACS) || defined(AQUATEXMACS))

#include "basic_renderer.hpp"
#include "analyze.hpp"
#include "gui.hpp" // for INTERRUPT_EVENT, INTERRUPTED_EVENT
#include "font.hpp" // for the definition of font
#include "rgb_colors.hpp"
#include "iterator.hpp"
#include <string.h>

/******************************************************************************
* structure for caching font pixmaps
******************************************************************************/

basic_character::operator tree () {
  tree t (TUPLE,  as_string (rep->c), rep->fng->res_name);
  t << as_string (rep->sf) << as_string (rep->fg) << as_string (rep->bg);
  return t;
}

bool
operator == (basic_character xc1, basic_character xc2) {
  return
    (xc1->c==xc2->c) && (xc1->fng.rep==xc2->fng.rep) &&
    (xc1->sf==xc2->sf) && (xc1->fg==xc2->fg) && (xc1->bg==xc2->bg);
}

bool
operator != (basic_character xc1, basic_character xc2) {
  return
    (xc1->c!=xc2->c) || (xc1->fng.rep!=xc2->fng.rep) ||
    (xc1->sf!=xc2->sf) || (xc1->fg!=xc2->fg) || (xc1->bg!=xc2->bg);
}

int
hash (basic_character xc) {
  return xc->c ^ ((intptr_t) xc->fng.rep) ^ xc->fg ^ xc->bg ^ xc->sf;
}

/******************************************************************************
* Set up colors
******************************************************************************/

bool reverse_colors= false;

#ifdef QTTEXMACS
#define LARGE_COLORMAP
#else
#define MEDIUM_COLORMAP
#endif

#ifndef LARGE_COLORMAP
#ifdef SMALL_COLORMAP
int CSCALES= 4;
int CFACTOR= 5;
int GREYS  = 16;
#else
int CSCALES= 8;
int CFACTOR= 9;
int GREYS  = 256;
#endif
int CTOTAL = (CFACTOR*CFACTOR*CFACTOR+GREYS+1);
#endif

#ifdef LARGE_COLORMAP

color
rgb_color (int r, int g, int b, int a) {
  return (a << 24) + (r << 16) + (g << 8) + b;
}

void
get_rgb_color (color col, int& r, int& g, int& b, int& a) {
  a= (col >> 24) & 255;
  r= (col >> 16) & 255;
  g= (col >> 8) & 255;
  b= col & 255;
}

#else

color
rgb_color (int r, int g, int b, int a) {
  if ((r==g) && (g==b))
    return (a << 24) + (r*GREYS+ 128)/255;
  else {
    r= (r*CSCALES+ 128)/255;
    g= (g*CSCALES+ 128)/255;
    b= (b*CSCALES+ 128)/255;
    return (a << 24) + r*CFACTOR*CFACTOR+ g*CFACTOR+ b+ GREYS+ 1;
  }
}

void
get_rgb_color (color col, int& r, int& g, int& b, int& a) {
  a= (col >> 24) & 255;
  col= col & 0xffffff;
  if (col <= ((color) GREYS)) {
    r= (col*255)/GREYS;
    g= (col*255)/GREYS;
    b= (col*255)/GREYS;
  }
  else {
    int rr, gg, bb;
    col-= (GREYS+1);
    bb  = col % CFACTOR;
    gg  = (col/CFACTOR) % CFACTOR;
    rr  = (col/(CFACTOR*CFACTOR)) % CFACTOR;
    r   = (rr*255)/CSCALES;
    g   = (gg*255)/CSCALES;
    b   = (bb*255)/CSCALES;
  }
}

#endif

color	black   = rgb_color (0, 0, 0);
color	white   = rgb_color (255, 255, 255);
color	red     = rgb_color (255, 0, 0);
color	blue    = rgb_color (0, 0, 255);
color	yellow  = rgb_color (255, 255, 0);
color	green   = rgb_color (0, 255, 0);
color	magenta = rgb_color (255, 0, 255);
color	orange  = rgb_color (255, 128, 0);
color	brown   = rgb_color (128, 32, 0);
color	pink    = rgb_color (255, 128, 128);

color	light_grey = rgb_color (208, 208, 208);
color	grey       = rgb_color (184, 184, 184);
color	dark_grey  = rgb_color (112, 112, 112);

static color
named_color_bis (string s) {
  if ((N(s) == 4) && (s[0]=='#')) {
    int r= 17 * from_hexadecimal (s (1, 2));
    int g= 17 * from_hexadecimal (s (2, 3));
    int b= 17 * from_hexadecimal (s (3, 4));
    return rgb_color (r, g, b);
  }
  if ((N(s) == 7) && (s[0]=='#')) {
    int r= from_hexadecimal (s (1, 3));
    int g= from_hexadecimal (s (3, 5));
    int b= from_hexadecimal (s (5, 7));
    return rgb_color (r, g, b);
  }
#ifdef REDUCED_COLORMAP
  unsigned int depth= 8;
#else
#ifdef MEDIUM_COLORMAP
  unsigned int depth= 16;
#else
  unsigned int depth= 24;
#endif
#endif
  int pastel= (depth>=16? 223: 191);
  
  if ((N(s) > 4) && (s (1,4) == "gray") && (is_numeric (s (5, N(s))))) {
    int level, i=5;
    if (read_int(s,i,level)) {
      level = (level*255) /100;
      return rgb_color (level, level, level);
    }
  }
	
  if (s == "black")          return black;
  if (s == "white")          return white;
  if (s == "grey")           return grey;
  if (s == "red")            return red;
  if (s == "blue")           return blue;
  if (s == "yellow")         return yellow;
  if (s == "green")          return green;
  if (s == "magenta")        return magenta;
  if (s == "cyan")           return rgb_color (0, 255, 255);
  if (s == "orange")         return orange;
  if (s == "brown")          return brown;
  if (s == "pink")           return pink;
  if (s == "broken white")   return rgb_color (255, 255, pastel);
  if (s == "light grey")     return light_grey;
  if (s == "darker grey")    return rgb_color (64, 64, 64);
  if (s == "dark grey")      return dark_grey;
  if (s == "dark red")       return rgb_color (128, 0, 0);
  if (s == "dark blue")      return rgb_color (0, 0, 128);
  if (s == "dark yellow")    return rgb_color (128, 128, 0);
  if (s == "dark green")     return rgb_color (0, 128, 0);
  if (s == "dark magenta")   return rgb_color (128, 0, 128);
  if (s == "dark cyan")      return rgb_color (0, 128, 128);
  if (s == "dark orange")    return rgb_color (128, 64, 0);
  if (s == "dark brown")     return rgb_color (64, 16, 0);
  if (s == "pastel grey")    return rgb_color (pastel, pastel, pastel);
  if (s == "pastel red")     return rgb_color (255, pastel, pastel);
  if (s == "pastel blue")    return rgb_color (pastel, pastel, 255);
  if (s == "pastel yellow")  return rgb_color (255, 255, pastel);
  if (s == "pastel green")   return rgb_color (pastel, 255, pastel);
  if (s == "pastel magenta") return rgb_color (255, pastel, 255);
  if (s == "pastel cyan")    return rgb_color (pastel, 255, 255);
  if (s == "pastel orange")  return rgb_color (255, pastel, 2*pastel-255);
  if (s == "pastel brown")   return rgb_color (pastel, 2*pastel-255, 2*pastel-255);
  return black;
}

color
named_color (string s, int a) {
  color c= named_color_bis (s);
  return (a << 24) + (c & 0xffffff);
}

string
get_named_color (color c) {
  int r, g, b, a;
  get_rgb_color (c, r, g, b, a);
  return "#" *
    as_hexadecimal (r, 2) *
    as_hexadecimal (g, 2) *
    as_hexadecimal (b, 2);
}

#define RGBCOLOR(r,g,b) rgb_color(r,g,b)

color
xpm_to_color (string s) {
  if (s == "none") return RGBCOLOR(100,100,100,0);
  if ((N(s) == 4) && (s[0]=='#')) {
    int r= 17 * from_hexadecimal (s (1, 2));
    int g= 17 * from_hexadecimal (s (2, 3));
    int b= 17 * from_hexadecimal (s (3, 4));
    return RGBCOLOR(r,g,b);
  }
  if ((N(s) == 7) && (s[0]=='#')) {
    int r= from_hexadecimal (s (1, 3));
    int g= from_hexadecimal (s (3, 5));
    int b= from_hexadecimal (s (5, 7));
    return RGBCOLOR(r,g,b);
  }
  if ((N(s) == 13) && (s[0]=='#')) {
    int r= from_hexadecimal (s (1, 5));
    int g= from_hexadecimal (s (5, 9));
    int b= from_hexadecimal (s (9, 13));
    return RGBCOLOR(r,g,b);
  }
  {
    c_string name (s);
    for(int i=0; i<RGBColorsSize; i++) {
      if (strcmp(name,RGBColors[i].name)==0) {
        return RGBCOLOR(RGBColors[i].r,RGBColors[i].g,RGBColors[i].b);
      }
    }
  }
  return RGBCOLOR (0, 0, 0);
}

#undef RGBCOLOR

/******************************************************************************
* Conversion between window and postscript coordinates
******************************************************************************/

void
basic_renderer_rep::get_extents (int& w2, int& h2) {
  w2 = w; h2 = h;
}

void basic_renderer_rep::begin (void* handle) { 
  (void) handle; 
}

void basic_renderer_rep::end () {  }

/******************************************************************************
* Drawing into drawables
******************************************************************************/

color
basic_renderer_rep::rgb (int r, int g, int b, int a) {
  return rgb_color (r, g, b, a);
}

void
basic_renderer_rep::get_rgb (color col, int& r, int& g, int& b, int& a) {
  get_rgb_color (col, r, g, b, a);
}

color
basic_renderer_rep::get_color () {
  return cur_fg;
}

#if 0
color
basic_renderer_rep::get_color (string s) {
  return named_color (s);
}
#endif

brush
basic_renderer_rep::get_brush () {
  return fg_brush;
}

brush
basic_renderer_rep::get_background () {
  return bg_brush;
}

void
basic_renderer_rep::set_color (color c) {
  cur_fg= c;
}

void
basic_renderer_rep::set_brush (brush b) {
  fg_brush= b;
  cur_fg= b->c;
}

void
basic_renderer_rep::set_background (brush b) {
  bg_brush= b;
  cur_bg= b->c;
}


void
basic_renderer_rep::set_clipping (SI x1, SI y1, SI x2, SI y2, bool restore) {
  (void) restore;
//  outer_round (x1, y1, x2, y2);
  renderer_rep::set_clipping (x1, y1, x2, y2);
}

/* shadowing and copying rectangular regions across devices defaults to nothing */

void
basic_renderer_rep::fetch (SI x1, SI y1, SI x2, SI y2,
			   renderer dev, SI x, SI y)
{
  (void) x1; (void) y1; (void) x2; (void) y2; (void) dev; (void) x; (void) y; 
  if (DEBUG_EVENTS)
    cout << "REN fetch (" << x1 << "," << x2 << "," << y1 << "," << y2
	 << ", dev ," << x << "," << y << ")\n";
}

void
basic_renderer_rep::new_shadow (renderer& dev) {
  dev = this; 
  if (DEBUG_EVENTS) cout << "REN new_shadow\n";
}

void
basic_renderer_rep::delete_shadow (renderer& dev) { dev= NULL; 
  if (DEBUG_EVENTS) cout << "REN delete_shadow\n";
}

void
basic_renderer_rep::get_shadow (renderer dev, SI x1, SI y1, SI x2, SI y2) {
  (void) x1; (void) y1; (void) x2; (void) y2; (void) dev; 
  if (DEBUG_EVENTS)
    cout << "REN get_shadow (" << x1 << "," << x2
	 << "," << y1 << "," << y2 << ", dev )\n";
}

void
basic_renderer_rep::put_shadow (renderer dev, SI x1, SI y1, SI x2, SI y2) {
  (void) x1; (void) y1; (void) x2; (void) y2; (void) dev; 
  if (DEBUG_EVENTS)
    cout << "REN put_shadow (dev, " << x1 << "," << x2
	 << "," << y1 << "," << y2 << ")\n";
}

void
basic_renderer_rep::apply_shadow (SI x1, SI y1, SI x2, SI y2) {
  (void) x1; (void) y1; (void) x2; (void) y2; 
  if (DEBUG_EVENTS)
    cout << "REN apply_shadow (" << x1 << "," << x2
	 << "," << y1 << "," << y2 << ")\n";
}

/******************************************************************************
* Image cache
******************************************************************************/

static time_t cache_image_last_gc = 0;
static int    cache_image_tot_size= 0;
static int    cache_image_max_size= 10000;
static hashmap<tree,cache_image_element> cache_image;

// to inform texmacs about image sizes we need to fill this structure
// see System/Files/image_files.cpp

extern hashmap<tree,string> ps_bbox; 

void
image_auto_gc () {
  time_t time= texmacs_time ();
  if (time-cache_image_last_gc <= 300000) return;
  cache_image_last_gc= time;
  if (DEBUG_AUTO)
    cout << "TeXmacs] Launching garbage collection for unused pictures\n";
  
  iterator<tree> it= iterate (cache_image);
  while (it->busy()) {
    tree lookup= it->next();
    cache_image_element ci = cache_image [lookup];
    time_t diff= time- ci->time;
    int fact= ci->nr;
    fact= fact * fact * fact;
    if ((ci->w * ci->h) < 400) fact= fact * 5;
    if ((ci->w * ci->h)  < 6400) fact= fact * 5;
    if (diff > 60000*fact) {
      cache_image->reset (lookup);
      ps_bbox->reset (lookup[0]);
    }
  }
}

void
image_gc (string name) {
  (void) name;
  cache_image_last_gc= texmacs_time ();
  iterator<tree> it= iterate (cache_image);
  while (it->busy()) {
    tree lookup= it->next();
    if (!is_ramdisc (as_url (lookup[0]))) {
      cache_image_element ci = cache_image [lookup];
      cache_image->reset (lookup);
      ps_bbox->reset (lookup[0]);
    }
  }
}

cache_image_element 
get_image_cache (tree lookup) {
  if (cache_image->contains (lookup)) return cache_image [lookup];
  return cache_image_element();
}

void
set_image_cache (tree lookup, cache_image_element ci)  {
  if (N(cache_image) == 0) cache_image_last_gc= texmacs_time ();

  cache_image (lookup)= ci;
  cache_image_tot_size += (ci->w)*(ci->h);
  if (cache_image_tot_size > cache_image_max_size) {
    image_auto_gc ();
    if (cache_image_tot_size > cache_image_max_size)
      cache_image_max_size= cache_image_tot_size << 1;
  }
}

bool
gui_interrupted (bool check) {
  return check_event (check? INTERRUPT_EVENT: INTERRUPTED_EVENT);
}

#endif
