
/******************************************************************************
* MODULE     : line_breaker.cpp
* DESCRIPTION: Line breaking facility for paragraphs
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "Boxes/construct.hpp"
#include "Format/line_item.hpp"

/******************************************************************************
* Information about the best line breaks
******************************************************************************/

struct lb_info_rep: concrete_struct {
  path prev;
  int  pen;
  int  pen_spc;

  lb_info_rep (): prev (), pen (HYPH_INVALID), pen_spc(1000000000) {}
};

struct lb_info {
  CONCRETE(lb_info);
  lb_info () { rep= new lb_info_rep (); }
  operator tree () {
    return tuple ((tree) rep->prev,
		  as_string (rep->pen),
		  as_string (rep->pen_spc)); }
};
CONCRETE_CODE(lb_info);

ostream&
operator << (ostream& out, lb_info hi) {
  return out << "[ " << hi->prev << ", "
	     << hi->pen << ", " << hi->pen_spc << " ]";
}

// FIXME: explicit instanciation for broken g++
#include "hashmap.hpp"
template lb_info& hashmap_rep<list<int>, lb_info>::bracket_rw(list<int>);

/******************************************************************************
* The line_breaker class
******************************************************************************/

struct line_breaker_rep {
  array<line_item> a;
  int start;
  int end;
  SI  line_width;
  SI  first_spc;
  SI  last_spc;
  int pass;
  hashmap<path,lb_info> best;

  line_breaker_rep (array<line_item> a, int start, int end,
		    SI line_width, SI first_spc, SI last_spc);

  void empty_line_fix (line_item& first, path& pos, int& cur_nr);
  path next_ragged_break (path pos);
  array<path> compute_ragged_breaks ();

  void test_better (path new_pos, path old_pos, int penalty, int pen_spc);
  bool propose_break (path new_pos, path old_pos, int penalty, space spc);
  void break_string (line_item item, path pos, int i, space spc);
  void process (path pos);
  void get_breaks (array<path>& ap, path p);
  array<path> compute_breaks ();
};

line_breaker_rep::line_breaker_rep (
  array<line_item> a2, int start2, int end2,
  SI line_width2, SI first_spc2, SI last_spc2):
    a (a2), start (start2), end (end2),
    line_width (line_width2), first_spc (first_spc2), last_spc (last_spc2),
    best (lb_info ()) {}

/******************************************************************************
* Some subroutines
******************************************************************************/

static int
get_position (font fn, string s, SI x) {
  int prev_i, prev_x=0, i=0, n=N(s);
  STACK_NEW_ARRAY (xpos, SI, n+1);
  fn->get_xpositions (s, xpos);
  while (i<n) {
    prev_i= i;
    if (s[i]=='<') {
      while ((i<n) && (s[i]!='>')) i++;
      if (i<n) i++;
    }
    else i++;
    int m= (prev_x + xpos[i]) >> 1;
    if (x<m) {
      STACK_DELETE_ARRAY (xpos);
      return prev_i;
    }
    prev_x= xpos[i];
  }
  STACK_DELETE_ARRAY (xpos);
  return i;
}

inline int square (int i) { return i*i; }

/*static*/ void
hyphenate (line_item item, int pos, line_item& item1, line_item& item2) {
  path ip= item->b->ip;
  int  x1= is_accessible (ip)? item->b->get_leaf_left_pos (): 0;
  int  x2= is_accessible (ip)? x1+ pos+ 1: 0;

  box b= item->b;
  string s  = b->get_leaf_string ();
  font   fn = b->get_leaf_font ();
  color  col= b->get_leaf_color ();

  string s1, s2;
  array<int> hp= item->lan->get_hyphens (s);
  item->lan->hyphenate (s, pos, s1, s2);
  item1= line_item (STRING_ITEM,
		    shorter_box (ip, text_box (ip, x1, s1, fn, col), pos+ 1),
		    hp[pos], item->lan);
  item2= line_item (STRING_ITEM, text_box (ip, x2, s2, fn, col),
		    item->penalty, item->lan);
  item2->spc= item->spc;
  // cout << s << " ---> " << s1 << " " << s2 << "\n";
}

/******************************************************************************
* Naive line breaking for ragged paragraph types
******************************************************************************/

void
line_breaker_rep::empty_line_fix (line_item& first, path& pos, int& cur_nr) {
  // Fix for avoiding lines with only empty boxes
  int i;
  SI tot_spc= 0;
  for (i= pos->item; i<end && i<=cur_nr; i++) {
    line_item cur_item = (i==pos->item? first: a[i]);
    tot_spc += cur_item->b->w() /*+ cur_item->spc->def*/;
    if (tot_spc != 0 && i < cur_nr) { i= cur_nr; break; }
  }
  cur_nr= i;
  while (cur_nr<end) {
    line_item cur_item = (cur_nr==pos->item? first: a[cur_nr]);
    if (cur_item->b->w() /*+ cur_item->spc->def*/ != 0) break;
    cur_nr++;
  }
}

path
line_breaker_rep::next_ragged_break (path pos) {
  int       cur_nr  = pos->item;
  line_item cur_item= a[cur_nr];
  SI        cur_spc;

  if (pos == path (start)) cur_spc= first_spc+ cur_item->b->w();
  else {
    path p= pos;
    while (!atom (p)) {
      line_item item1, item2;
      p= p->next;
      hyphenate (cur_item, p->item, item1, item2);
      cur_item= item2;
    }
    cur_spc= cur_item->b->w();
  }
  
  line_item first= cur_item;
  while (true) {
    cur_spc += cur_item->spc->def;
    if ((++cur_nr)==end) break;
    cur_item = a[cur_nr];
    cur_spc += cur_item->b->w();
    if (cur_spc > line_width) {
      // cout << "Overfull " << cur_spc << ", " << line_width << "\n";
      break;
    }
  }

  while (true) {
    if (cur_nr<end) {
      cur_spc -= cur_item->b->w();
      if ((cur_spc <= line_width) &&
	  (cur_item->type==STRING_ITEM)) {
	string s= cur_item->b->get_leaf_string ();
	if (N(s)>4) {
	  array<int> hp= cur_item->lan->get_hyphens (s);
	  int i= get_position (cur_item->b->get_leaf_font (),
			       s, line_width- cur_spc);
	  for (i= min (i+1, N(hp)-1); i>=0; i--)
	    if (hp[i] < HYPH_INVALID) {
	      line_item item1, item2;
	      hyphenate (cur_item, i, item1, item2);
	      if (cur_spc+item1->b->w() <= line_width)
		return (cur_nr>pos->item)?
		  path (cur_nr, i): pos * i;
	    }
	}
      }
    }
    if ((--cur_nr)<pos->item) {
      do cur_nr++;
      while ((cur_nr<end) && (a[cur_nr]->penalty >= HYPH_INVALID));
      if (cur_nr<end) cur_nr++;
      //empty_line_fix (first, pos, cur_nr);
      return path (cur_nr);
    }
    cur_item = (cur_nr==pos->item? first: a[cur_nr]);
    cur_spc -= cur_item->spc->def;
    if ((cur_spc <= line_width) &&
	((cur_item->penalty < HYPH_INVALID) || (cur_nr==end-1))) {
      cur_nr++;
      //empty_line_fix (first, pos, cur_nr);
      return path (cur_nr);
    }
  }
}

/******************************************************************************
* Computation of the ragged line breaks
******************************************************************************/

array<path>
line_breaker_rep::compute_ragged_breaks () {
  array<path> ap;
  path p (start);
  while (p != path (end)) {
    ap << p;
    p= next_ragged_break (p);
  }
  ap << p;
  return ap;
}

/******************************************************************************
* Test whether we found a better break
******************************************************************************/

void
line_breaker_rep::test_better (path new_pos, path old_pos,
			       int pen, int pen_spc)
{
  // cout << "Test " << new_pos << ", " << old_pos << ", "
  //      << pen << ", " << pen_spc << "\n";
  if (!best->contains (new_pos)) best (new_pos)= lb_info ();
  lb_info cur= best [new_pos];
  if ((pen < cur->pen) ||
      ((pen == cur->pen) && (pen_spc < cur->pen_spc))) {
    cur->prev   = old_pos;
    cur->pen    = pen;
    cur->pen_spc= min (pen_spc, 1000000000);
  }
}

bool
line_breaker_rep::propose_break (path new_pos, path old_pos,
				 int pen, space spc)
{
  lb_info cur= best[old_pos];

  if ((spc->min <= line_width) &&
      ((spc->max >= line_width) || (new_pos->item==end))) {
    SI d= max (line_width- spc->def, spc->def- line_width);
    if (new_pos->item==end) d=0;
    test_better (new_pos, old_pos, min (HYPH_INVALID, cur->pen+ pen),
		 cur->pen_spc+ (cur->pen==HYPH_INVALID? 0: square (d/PIXEL)));
  }
  
  if (pass==2) {
    if (spc->max < line_width)
      test_better (new_pos, old_pos, HYPH_INVALID,
		   (cur->pen==HYPH_INVALID? cur->pen_spc: 0)+
		   square ((line_width- spc->max)/PIXEL)+
		   (new_pos->item==old_pos->item?
		    square (line_width/PIXEL): 0));
    if (spc->min > line_width)
      test_better (new_pos, old_pos, HYPH_INVALID,
		   (cur->pen==HYPH_INVALID? cur->pen_spc: 0)+
		   square ((spc->min- line_width)/PIXEL)+
		   square (4*line_width/PIXEL));
  }
  
  return spc->min > line_width;
}

/******************************************************************************
* Fill up a line starting from pos
******************************************************************************/

void
line_breaker_rep::break_string (line_item item, path pos, int i, space spc) {
  int j;
  string item_s= item->b->get_leaf_string ();
  array<int> hp= item->lan->get_hyphens (item_s);

  if ((item->b->w() > line_width) || (!atom (pos))) {
    j= get_position (item->b->get_leaf_font (), item_s, line_width- spc->def);
    for (j= min (j+2, N(hp)-1); j>=0; j--)
      if (hp[j] < HYPH_INVALID) {
	line_item item1, item2;
	hyphenate (item, j, item1, item2);
	path next= (i==pos->item)? pos * j: path (i, j);
	space spc_hyph= spc+ space (item1->b->w());
	if (spc_hyph->min <= line_width) {
	  propose_break (next, pos, hp[j], spc_hyph->min);
	  break;
	}
      }
  }
  else {
    for (j=0; j<N(hp); j++)
      if (hp[j] < HYPH_INVALID) {
	line_item item1, item2;
	hyphenate (item, j, item1, item2);
	path next= (i==pos->item)? pos * j: path (i, j);
	space spc_hyph= spc+ space (item1->b->w());
	(void) propose_break (next, pos, hp[j], spc_hyph);
      }
  }
}

void
line_breaker_rep::process (path pos) {
  int i;
  space spc;
  line_item first;

  first= a[pos->item];
  if (pos == path (start)) spc= space (first_spc+ first->b->w());
  else {
    path p= pos;
    while (!atom (p)) {
      line_item item1, item2;
      p= p->next;
      hyphenate (first, p->item, item1, item2);
      first= item2;
    }
    spc= space (first->b->w());
  }

  if ((pass>1) || (best[pos]->pen < HYPH_INVALID)) {
    // cout << "Process " << pos << ": " << first << "\n";
    for (i=pos->item; i<end; i++) {
      line_item item= a[i];
      if (i == pos->item) item= first;
      else spc= spc+ a[i-1]->spc+ space (item->b->w());
      if ((spc->max > line_width) &&
	  (item->type == STRING_ITEM) &&
	  (N(item->b->get_leaf_string ())>4))
	break_string (item, pos, i, spc+ space (-item->b->w()));
      if (item->penalty < HYPH_INVALID)
	if (propose_break (path (i+1), pos, item->penalty, spc))
	  break;
      if ((item->type == CONTROL_ITEM) &&
	  (item->t == LINE_BREAK) &&
	  (spc->min < line_width))
	if (propose_break (path (i+1), pos, 0, space (line_width)))
	  break;
    }
    if (i==end) {
      line_width -= last_spc;
      propose_break (path (i), pos, 0, spc);
      line_width += last_spc;
    }
  }

  if (first->type == STRING_ITEM) {
    string first_s= first->b->get_leaf_string ();
    int n= N(first_s);
    if (n>4)
      for (i=0; i<n-1; i++)
	if (best-> contains (pos * i))
	  process (pos * i);
  }
}

/******************************************************************************
* Hyphenate an array of line_items
******************************************************************************/

void
line_breaker_rep::get_breaks (array<path>& ap, path p) {
  if (nil (p)) return;
  lb_info cur= best[p];
  get_breaks (ap, cur->prev);
  ap << p;
}

array<path>
line_breaker_rep::compute_breaks () {
  int i;
  test_better (path (start), path (), 0, 0);

  pass= 1;
  for (i=start; i<end; i++)
    process (path (i));

  pass= 2;
  if (best [path (end)]->pen == HYPH_INVALID)
    for (i=start; i<end; i++)
      process (path (i));
  
  array<path> ap (0);
  get_breaks (ap, path (end));
  return ap;
}

/******************************************************************************
* The exported line breaking routine
*******************************************************************************
* Given an array of line_items on input,
* we compute an array of best line breaks.
* These breaks are represented as paths:
* the first element indicates a line_item.
* The other elements correspond to successive hyphenations of
* the line_item, if it was a STRING_ITEM.
******************************************************************************/

array<path>
line_breaks (array<line_item> a, int start, int end,
	     SI line_width, SI first_spc, SI last_spc, bool ragged)
{
  int tol= 5;         // extra tolerance of 5tmpt avoid rounding errors when
  line_width += tol;  // the widths of the boxes sum up to precisely 1par
  line_breaker_rep* H=
    new line_breaker_rep (a, start, end, line_width, first_spc, last_spc);
  array<path> ap= ragged? H->compute_ragged_breaks (): H->compute_breaks ();
  delete H;
  return ap;
}
