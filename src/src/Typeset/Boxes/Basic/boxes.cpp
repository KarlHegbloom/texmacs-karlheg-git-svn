
/******************************************************************************
* MODULE     : boxes.cpp
* DESCRIPTION: Important routines for all boxes
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "boxes.hpp"
#include "formatter.hpp"
#include "Graphics/point.hpp"
#include "timer.hpp"
#include "PsDevice/printer.hpp"
#include "file.hpp"
#include "merge_sort.hpp"

/******************************************************************************
* Default settings for virtual routines
******************************************************************************/

int box_rep::subnr () { return 0; }
box box_rep::subbox (int i) { (void) i; return box (); }
box box::operator [] (path p) {
  if (nil (p)) return *this; else return rep->subbox(p->item)[p->next]; }
double box_rep::left_slope () { return 0.0; }
double box_rep::right_slope () { return 0.0; }
SI box_rep::left_correction () { return (SI) (-min (0, y1) * left_slope ()); }
SI box_rep::right_correction () { return (SI) (max (0, y2) * right_slope ()); }
SI box_rep::lsub_correction () { return 0; }
SI box_rep::lsup_correction () { return 0; }
SI box_rep::rsub_correction () { return 0; }
SI box_rep::rsup_correction () { return 0; }
SI box_rep::sub_lo_base (int level) { (void) level; return y1; }
SI box_rep::sub_hi_lim  (int level) { (void) level; return y1 + ((y2-y1)/3); }
SI box_rep::sup_lo_lim  (int level) { (void) level; return (y1 + y2) >> 1; }
SI box_rep::sup_lo_base (int level) { (void) level; return y2 - ((y2-y1)/3); }
SI box_rep::sup_hi_lim  (int level) { (void) level; return y2; }

/******************************************************************************
* Positioning routines
******************************************************************************/

bool
outside (SI x, SI delta, SI x1, SI x2) {
  return
    (x<x1) || ((x==x1) && (delta<0)) ||
    (x>x2) || ((x==x2) && (delta>=0));
}

SI
get_delta (SI x, SI x1, SI x2) {
  if (x1==x2) return 0;
  if (x==x1) return -1;
  if (x==x2) return 1;
  return 0;
}

SI
box_rep::distance (int i, SI x, SI y, SI delta) {
  box b= subbox (i);
  x -= sx(i);
  y -= sy(i);
  int dx, dy;
  if (x <=  b->x1) dx = b->x1- x- (delta<0? 1:0);
  else if (x >=  b->x2) dx = x- b->x2+ (delta<0? 0:1);
  else dx = 0;
  if (y <  b->y1) dy = b->y1- y;
  else if (y >= b->y2) dy = y- b->y2;
  else dy = 0;
  return dx+dy;
}

bool
box_rep::in_rectangle (SI X1, SI Y1, SI X2, SI Y2) {
  return x1>=X1 && y1>=Y1 && x2<=X2 && y2<=Y2;
}

bool
box_rep::contains_rectangle (SI X1, SI Y1, SI X2, SI Y2) {
  return x1<=X1 && y1<=Y1 && x2>=X2 && y2>=Y2;
}

/******************************************************************************
* Cursor routines
******************************************************************************/

path
box_rep::find_box_path (SI x, SI y, SI delta, bool force) {
  (void) y;
  (void) force;
  SI m= (x1+x2)>>1;
  return path (((x<m) || ((x==m) && (delta<0)))? 0: 1);
}

path
box_rep::find_lip () {
  return descend (ip, 0);
}

path
box_rep::find_rip () {
  return descend (ip, 1);
}

path
box_rep::find_left_box_path () {
  return path (0);
}

path
box_rep::find_right_box_path () {
  return path (1);
}

path
box_rep::find_box_path (path p, bool& found) {
  found= (!nil(p)) && is_accessible (ip);
  if (last_item (p) == 0) return path (0);
  else return path (1);
}

path
box_rep::find_tree_path (path bp) {
  if (bp == path (0)) return reverse (descend_decode (ip, 0));
  else return reverse (descend_decode (ip, 1));
}

cursor
box_rep::find_cursor (path bp) {
  bool flag= bp == path (0);
  double slope= flag? left_slope (): right_slope ();
  cursor cu (flag? x1: x2, 0);
  cu->y1= y1; cu->y2= y2;
  cu->slope= slope;
  return cu;
}

selection
box_rep::find_selection (path lbp, path rbp) {
  if (lbp == rbp)
    return selection (rectangles (),
		      find_tree_path (lbp), find_tree_path (rbp));
  else
    return selection (rectangle (x1, y1, x2, y2),
		      find_tree_path (path (0)), find_tree_path (path (1)));
}

path
box_rep::find_tree_path (SI x, SI y, SI delta) {
  path bp= find_box_path (x, y, delta, false);
  //cout << "Find " << x << ", " << y << "; " << delta;
  //cout << " -> " << bp << "\n";
  return find_tree_path (bp);
}

cursor
box_rep::find_check_cursor (path p) {
  bool found;
  path bp= find_box_path (p, found);
  cursor cu= find_cursor (bp);
  cu->valid= found;
  return cu;
}

selection
box_rep::find_check_selection (path lp, path rp) {
  bool lfound= false, rfound= false;
  path lbp= find_box_path (lp, lfound);
  path rbp= find_box_path (rp, rfound);
  selection sel= find_selection (lbp, rbp);
  sel->valid= lfound && rfound;
  return sel;
}

void
box_rep::relocate (path new_ip, bool force) {
  if (!force)
    if (nil (ip) || (ip->item >= 0) || (ip == new_ip)) return;
  ip= new_ip;
  int i, n= subnr ();
  for (i=0; i<n; i++) subbox (i)->relocate (ip, force);
}

box
box_rep::transform (frame fr) {
  (void) fr;
  return box ();
}

/******************************************************************************
* Modified cursor routines in presence of scrolled boxes
******************************************************************************/

path
find_innermost_scroll (box b, path p) {
  // Given a box b and a logical path p, this routine returns 
  // the longest box path sp such that b[sp] is a scroll node
  path bp;
  while (true) {
    bool found= false;
    bp= b->find_box_path (p, found);
    if (found) break;
    p= path_up (p);
    if (nil (p)) return path ();
  }
  bp= path_up (bp);
  path cp, sp;
  while (!nil (bp)) {
    if (b->get_type () == SCROLL_BOX) sp= reverse (cp);
    b = b[bp->item];
    cp= path (bp->item, cp);
    bp= bp->next;
  }
  if (nil (sp)) return sp;
  else return sp * 0;
}

path
find_scrolled_box_path (box b, path sp, SI x, SI y, SI delta) {
  if (nil (sp)) return b->find_box_path (x, y, delta, false);
  else {
    int m= sp->item;
    SI xx= x - b->sx (m), yy= y - b->sy (m);
    SI dd= delta + get_delta (xx, b[m]->x1, b[m]->x2);
    return path (m, find_scrolled_box_path (b[m], sp->next, xx, yy, dd));
  }
}

path
find_scrolled_tree_path (box b, path sp, SI x, SI y, SI delta) {
  path bp= find_scrolled_box_path (b, sp, x, y, delta);
  //cout << "Find " << x << ", " << y << "; " << delta;
  //cout << " -> " << bp << "\n";
  return b->find_tree_path (bp);
}

void
find_canvas_info (box b, path sp, SI& x, SI& y, SI& sx, SI& sy,
		  rectangle& outer, rectangle& inner)
{
  if (nil (sp)) {
    x= y= sx= sy= 0;
    outer= inner= rectangle (0, 0, 0, 0);
  }
  else if (atom (sp)) {
    x    = 0;
    y    = 0;
    sx   = b->sx (0);
    sy   = b->sy (0);
    outer= rectangle (b->x1, b->y1, b->x2, b->y2);
    inner= rectangle (b[0]->x1, b[0]->y1, b[0]->x2, b[0]->y2);
  }
  else {
    find_canvas_info (b[sp->item], sp->next, x, y, sx, sy, outer, inner);
    x += b->sx (sp->item);
    y += b->sy (sp->item);
  }
}

/******************************************************************************
* For graphical boxes
******************************************************************************/

frame
box_rep::get_frame () {
  return frame ();
}

grid
box_rep::get_grid () {
  return grid ();
}

void
box_rep::get_limits (point& lim1, point& lim2) {
  lim1= point (); lim2= point ();
}

frame
box_rep::find_frame (path bp, bool last) {
  SI    x= 0;
  SI    y= 0;
  box   b= this;
  frame f= get_frame ();
  while (!nil (bp)) {
    x += b->sx (bp->item);
    y += b->sy (bp->item);
    b  = b->subbox (bp->item);
    bp = bp->next;
    frame g= b->get_frame ();
    if (!nil (g))
      if (last)
	f= g;
      else
	f= scaling (1.0, point (x, y)) * g;
  }
  return f;
}

grid
box_rep::find_grid (path bp) {
  box   b= this;
  grid g= get_grid ();
  while (!nil (bp)) {
    b  = b->subbox (bp->item);
    bp = bp->next;
    grid g2= b->get_grid ();
    if (!nil (g2)) g= g2;
  }
  return g;
}

void
box_rep::find_limits (path bp, point& lim1, point& lim2) {
  box b= this;
  get_limits (lim1, lim2);
  while (!nil (bp)) {
    point slim1, slim2;
    b  = b->subbox (bp->item);
    bp = bp->next;
    b->get_limits (slim1, slim2);
    if (slim1 != point ()) {
      lim1= slim1;
      lim2= slim2;
    }
  }
}

SI
box_rep::graphical_distance (SI x, SI y) {
  SI dx, dy;
  if (x <=  x1) dx= x1 - x;
  else if (x >=  x2) dx= x - x2;
  else dx= 0;
  if (y <  y1) dy= y1 - y;
  else if (y >= y2) dy= y - y2;
  else dy= 0;
  return (SI) norm (point (dx, dy));
}

gr_selections
box_rep::graphical_select (SI x, SI y, SI dist) {
  gr_selections res;
  if (graphical_distance (x, y) <= dist) {
    gr_selection gs;
    gs->dist= graphical_distance (x, y);
    gs->cp << find_tree_path (x, y, dist);
    // FIXME: check whether this is correct: I do not remember whether
    // find_tree_path returns an absolute or a relative path
    gs->c= curve ();
    res << gs;
  }
  return res;
}

gr_selections
box_rep::graphical_select (SI x1, SI y1, SI x2, SI y2) {
  gr_selections res;
  if (in_rectangle (x1, y1, x2, y2)) {
    gr_selection gs;
    gs->dist= graphical_distance (x1, y1);
    SI dist= (SI)norm (point (x2-x1, y2-y1));
    gs->cp << find_tree_path (x1, y1, dist);
    // FIXME: as above, check whether this is correct or not
    gs->pts= array<point> (0);
    gs->c= curve ();
    res << gs;
  }
  return res;
}

/******************************************************************************
* Getting information from boxes
******************************************************************************/

int
box_rep::get_type () {
  return STD_BOX;
}

tree
box_rep::get_info (tree in) {
  (void) in;
  return "";
}

int
box_rep::get_leaf_left_pos () {
  cerr << "\nTeXmacs] the box is " << box (this) << "\n";
  fatal_error ("this box is not textual", "get_leaf_left_pos");
  return 0; // Because of bug in certain versions of g++
}

int
box_rep::get_leaf_right_pos () {
  cerr << "\nTeXmacs] the box is " << box (this) << "\n";
  fatal_error ("this box is not textual", "get_leaf_right_pos");
  return 0; // Because of bug in certain versions of g++
}

string
box_rep::get_leaf_string () {
  cerr << "\nTeXmacs] the box is " << box (this) << "\n";
  fatal_error ("this box is not textual", "get_leaf_string");
  return ""; // Because of bug in certain versions of g++
}

font
box_rep::get_leaf_font () {
  cerr << "\nTeXmacs] the box is " << box (this) << "\n";
  fatal_error ("this box is not textual", "get_leaf_font");
  return font (); // Because of bug in certain versions of g++
}

color
box_rep::get_leaf_color () {
  cerr << "\nTeXmacs] the box is " << box (this) << "\n";
  fatal_error ("this box is not textual", "get_leaf_color");
  return 0; // Because of bug in certain versions of g++
}

language
box_rep::get_leaf_language () {
  cerr << "\nTeXmacs] the box is " << box (this) << "\n";
  fatal_error ("this box is not textual", "get_leaf_language");
  return language (); // Because of bug in certain versions of g++
}

tree
box_rep::get_leaf_tree () {
  cerr << "\nTeXmacs] the box is " << box (this) << "\n";
  fatal_error ("no tree attached to this box", "get_leaf_tree");
  return ""; // Because of bug in certain versions of g++
}

lazy
box_rep::get_leaf_lazy () {
  cerr << "\nTeXmacs] the box is " << box (this) << "\n";
  fatal_error ("no lazy attached to this box", "get_leaf_lazy");
  return lazy (); // Because of bug in certain versions of g++
}

SI
box_rep::get_leaf_offset (string search) {
  (void) search;
  return w();
}

/******************************************************************************
* Redrawing boxes
******************************************************************************/

int nr_painted= 0;

void
clear_pattern_rectangles (ps_device dev, rectangles l) {
  while (!nil (l)) {
    rectangle r (l->item);
    dev->clear_pattern (r->x1- dev->ox, r->y1- dev->oy,
			r->x2- dev->ox, r->y2- dev->oy);
    l= l->next;
  }
}

int
box_rep::reindex (int i, int item, int n) {
  if (item<0) item=0;
  if (item>n) item=n;
  if (i==0) return item;
  if ((i <= (item<<1)) && (i <= ((n-item)<<1))) {
    int d=(i+1)>>1;
    if (((i+1)&1)==0) return item-d;
    else return item+d;
  }
  if (i > (item<<1)) return i;
  return n-i;
}

void
box_rep::redraw (ps_device dev, path p, rectangles& l) {
  if (((nr_painted&15) == 15) && dev->interrupted (true)) return;
  dev->move_origin (x0, y0);
  SI delta= dev->pixel; // adjust visibility to compensate truncation
  if (dev->is_visible (x3- delta, y3- delta, x4+ delta, y4+ delta)) {
    rectangles ll;
    l= rectangles();
    pre_display (dev);

    int i, item=-1, n=subnr (), i1= n, i2= -1;
    if (!nil(p)) i1= i2= item= p->item;
    for (i=0; i<n; i++) {
      int k= reindex (i, item, n-1);
      if (nil(p)) subbox (k)->redraw (dev, path (), ll);
      else if (i!=0) {
	if (k > item) subbox(k)->redraw (dev, path (0), ll);
	else subbox(k)->redraw (dev, path (subbox(k)->subnr()-1), ll);
      }
      else subbox(k)->redraw (dev, p->next, ll);
      if (!nil(ll)) {
	i1= min (i1, k);
	i2= max (i2, k);
	l = ll * l;
	ll= rectangles ();
      }
    }

    if (((nr_painted&15) == 15) && dev->interrupted ()) {
      l= translate (l, -dev->ox, -dev->oy);
      clear_incomplete (l, dev->pixel, item, i1, i2);
      l= translate (l, dev->ox, dev->oy);
    }
    else {
      l= rectangle (x3+ dev->ox, y3+ dev->oy, x4+ dev->ox, y4+ dev->oy);
      display (dev);
      if (nr_painted < 15) dev->apply_shadow (x1, y1, x2, y2);
      nr_painted++;
    }

    post_display (dev);
  }
  dev->move_origin (-x0, -y0);
}

void
box_rep::redraw (ps_device dev, path p, rectangles& l, SI x, SI y) {
  dev->move_origin (x, y);
  redraw (dev, p, l);
  dev->move_origin (-x, -y);
}

void
box_rep::clear_incomplete (rectangles& rs, SI pixel, int i, int i1, int i2) {
  (void) rs; (void) pixel; (void) i; (void) i1; (void) i2;
}

void
box_rep::pre_display (ps_device &dev) {
  (void) dev;
}

void
box_rep::post_display (ps_device &dev) {
  (void) dev;
}

/******************************************************************************
* The cursor class
******************************************************************************/

cursor::cursor (SI x, SI y, SI delta, SI y1, SI y2, double slope, bool valid):
  rep (new cursor_rep)
{
  rep->ox= x ; rep->oy= y ; rep->delta= delta;
  rep->y1= y1; rep->y2= y2; rep->slope= slope;
  rep->valid= valid;
}

cursor
copy (cursor cu) {
  return cursor (cu->ox, cu->oy, cu->delta, cu->y1, cu->y2,
		 cu->slope, cu->valid);
}

bool
operator == (cursor cu1, cursor cu2) {
  return
    (cu1->ox == cu2->ox) && (cu1->oy == cu2->oy) &&
    // (cu1->delta == cu2->delta) &&
    (cu1->y1 == cu2->y1) && (cu1->y2 == cu2->y2) &&
    (cu1->slope == cu2->slope);
}

bool
operator != (cursor cu1, cursor cu2) {
  return ! (cu1 == cu2);
}

ostream&
operator << (ostream& out, cursor cu) {
  out << "cursor (" << (cu->ox>>8) << ", " << (cu->oy>>8) << ": "
      << cu->delta << ": "
      << (cu->y1>>8) << ", " << (cu->y2>>8) << ": "
      << cu->slope << ")";
  return out;
}

/******************************************************************************
* Selections
******************************************************************************/

selection::selection (rectangles rs, path start, path end, bool valid):
  rep (new selection_rep)
{
  rep->rs   = rs;
  rep->start= start;
  rep->end  = end;
  rep->valid= valid;
}

bool
operator == (selection sel1, selection sel2) {
  return
    (sel1->start == sel2->start) &&
    (sel1->end == sel2->end);
}

bool
operator != (selection sel1, selection sel2) {
  return !(sel1 == sel2);
}

ostream&
operator << (ostream& out, selection sel) {
  return out << "selection (" << sel->start << ", " << sel->end << ")";
}

/******************************************************************************
* Graphical selections
******************************************************************************/

gr_selection::gr_selection (array<path> cp, SI dist):
  rep (new gr_selection_rep)
{
  rep->cp  = cp;
  rep->dist= dist;
}

ostream&
operator << (ostream& out, gr_selection sel) {
  return out << "gr_selection (" << sel->dist << ", " << sel->cp << ")";
}

struct less_eq_gr_selection {
  static inline bool leq (gr_selection& a, gr_selection& b) {
    return a->dist <= b->dist; }
};

tree as_tree (gr_selections sels) {
  merge_sort_leq <gr_selection, less_eq_gr_selection> (sels);
  int i, n= N(sels);
  array<array<path> > res (n);
  for (i=0; i<n; i++)
    res[i]= sels[i]->cp;
  return (tree) res;
}

/******************************************************************************
* Animations
******************************************************************************/

int
box_rep::anim_length () {
  int i, n= subnr (), len=0;
  for (i=0; i<n; i++) {
    int slen= subbox (i)->anim_length ();
    if (slen == -1) return -1;
    if (slen > len) len= slen;
  }
  return len;
}

bool
box_rep::anim_started () {
  int i, n= subnr ();
  for (i=0; i<n; i++)
    if (!subbox (i)->anim_started ()) return false;
  return true;
}

bool
box_rep::anim_finished () {
  int i, n= subnr ();
  for (i=0; i<n; i++)
    if (!subbox (i)->anim_finished ()) return false;
  return true;
}

void
box_rep::anim_start_at (time_t at) {
  int i, n= subnr ();
  for (i=0; i<n; i++)
    subbox (i)->anim_start_at (at);
}

void
box_rep::anim_finish_now () {
  int i, n= subnr ();
  for (i=0; i<n; i++)
    subbox (i)->anim_finish_now ();
}

time_t
box_rep::anim_next_update () {
  fatal_error ("Invalid situation", "box_rep::anim_next_update");
  return texmacs_time ();
}

void
box_rep::anim_check_invalid (bool& flag, time_t& at, rectangles& rs) {
  time_t now= texmacs_time ();
  time_t finish_at= anim_next_update ();
  if (finish_at - now < 0) finish_at= now;
  if (flag && at - now < 0) at= now;
  if (!flag || finish_at - (at - 3) < 0) {
    flag= true;
    at  = finish_at;
    rs  = rectangle (x1, y1, x2, y2);
  }
  else if (finish_at - (at + 3) <= 0) {
    rs << rectangle (x1, y1, x2, y2);
    if (finish_at - at < 0)
      at= finish_at;
  }
}

void
box_rep::anim_get_invalid (bool& flag, time_t& at, rectangles& rs) {
  int i, n= subnr ();
  for (i=0; i<n; i++) {
    bool   flag2= false;
    time_t at2= at;
    rectangles rs2;
    subbox (i)->anim_get_invalid (flag2, at2, rs2);
    if (flag2) {
      rs2= translate (rs2, sx (i), sy (i));
      if (at2 - (at-3) < 0) rs= rs2;
      else rs << rs2;
      flag= true;
      if (at2 - at < 0) at= at2;
    }
  }
}

/******************************************************************************
* Miscellaneous routines
******************************************************************************/

tree
box_rep::action (tree t, SI x, SI y, SI delta) {
  (void) x; (void) y; (void) delta; (void) t;
  return "";
}

void
box_rep::loci (SI x, SI y, SI delta, list<string>& ids, rectangles& rs) {
  (void) x; (void) y; (void) delta;  
  ids= list<string> ();
  rs = rectangles ();
}

void
box_rep::position_at (SI x, SI y, rectangles& change_log) {
  int i, n= subnr ();
  x += x0; y += y0;
  for (i=0; i<n; i++) subbox (i)->position_at (x, y, change_log);
}

void
box_rep::collect_page_numbers (hashmap<string,tree>& h, tree page) {
  (void) h; (void) page;
}

path
box_rep::find_tag (string name) {
  (void) name;
  return path ();
}

bool box::operator == (box b2) { return rep==b2.rep; }
bool box::operator != (box b2) { return rep!=b2.rep; }

box::operator tree () { return tree (*rep); }
ostream& operator << (ostream& out, box b) { return out << ((tree) b); }

path
descend_decode (path ip, int side) {
  if (nil (ip)) return descend (ip, side);
  else switch (ip->item) {
  case DECORATION       : return ip->next;
  case DECORATION_LEFT  : return descend (ip->next, 0);
  case DECORATION_MIDDLE: return descend (ip->next, side);
  case DECORATION_RIGHT : return descend (ip->next, 1);
  default               : return descend (ip, side);
  }
}

tree
attach_dip (tree ref, path dip) {
  path old_ip= obtain_ip (ref);
  if (old_ip != path (DETACHED)) return ref;
  if (is_atomic (ref)) {
    tree r (ref->label);
    r->obs= list_observer (ip_observer (dip), r->obs);
    return r;
  }
  else {
    int i, n= N(ref);
    tree r (ref, n);
    for (i=0; i<n; i++)
      r[i]= attach_dip (ref[i], descend (dip, i));
    r->obs= list_observer (ip_observer (dip), r->obs);
    return r;
  }
}

/******************************************************************************
* Convert to postscript
******************************************************************************/

void
make_eps (url name, display dis, box b, int dpi) {
  double inch= ((double) dpi * PIXEL);
  double cm  = inch / 2.54;
  SI w= b->x4 - b->x3;
  SI h= b->y4 - b->y3;
  b->x0= -b->x3;
  b->y0= -b->y4;
  ps_device dev= printer (dis, name, dpi, 1, "user", false, w/cm, h/cm);
  dev->set_color (dis->black);
  dev->set_background (dis->white);
  rectangles rs;
  b->redraw (dev, path (0), rs);
  delete dev;
}
