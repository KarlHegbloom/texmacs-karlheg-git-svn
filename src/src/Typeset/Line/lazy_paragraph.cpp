
/******************************************************************************
* MODULE     : lazy_paragraph.cpp
* DESCRIPTION: Last pass for typesetting paragraphs;
*              hyphenation and creation of page items
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "Line/lazy_paragraph.hpp"
#include "Line/lazy_vstream.hpp"
#include "Format/format.hpp"
#include "Line/lazy_vstream.hpp"
#include "Boxes/construct.hpp"

array<line_item> typeset_concat (edit_env env, tree t, path ip);
void hyphenate (line_item item, int pos, line_item& item1, line_item& item2);
array<path>
line_breaks (array<line_item> a, int start, int end,
	     SI line_width, SI first_spc, SI last_spc, bool ragged);

/******************************************************************************
* Constructor
******************************************************************************/

lazy_paragraph_rep::lazy_paragraph_rep (edit_env env2, path ip):
  lazy_rep (LAZY_PARAGRAPH, ip),
  env (env2), style (""), sss (new stacker_rep)
{
  sss->ip= ip; // is this necessary?
  style (PAR_FIRST)   = env->read (PAR_FIRST);
  style (PAR_NO_FIRST)= env->read (PAR_NO_FIRST);
  // env->assign (PAR_NO_FIRST, "false");
  env->monitored_write_update (PAR_NO_FIRST, "false");

  SI d1, d2, d3, d4, d5, d6, d7;
  env->get_page_pars (width, d1, d2, d3, d4, d5, d6, d7);

  mode      = as_string (env->read (PAR_MODE));
  hyphen    = as_string (env->read (PAR_HYPHEN));
  left      = env->get_length (PAR_LEFT);
  right     = env->get_length (PAR_RIGHT);
  bot       = 0;
  top       = env->fn->yx;
  sep       = env->get_length (PAR_SEP);
  height    = env->decode_length (string ("1fn"))+ sep;
  hor_sep   = env->get_length (PAR_HOR_SEP);
  tab_sep   = hor_sep;
  line_sep  = env->get_space (PAR_LINE_SEP);
  par_sep   = env->get_space (PAR_PAR_SEP);
  nr_cols   = env->get_int (PAR_COLUMNS);

  tree dec  = env->read (ATOM_DECORATIONS);
  if (N(dec) > 0) decs << tuple ("0", dec);
}

lazy_paragraph_rep::~lazy_paragraph_rep () {
  delete sss;
}

lazy_paragraph_rep::operator tree () {
  return "Paragraph";
}

/******************************************************************************
* Filling lines of paragraphs
******************************************************************************/

void
lazy_paragraph_rep::line_print (line_item item) {
  // cout << "Printing: " << item << "\n";
  if (item->type == CONTROL_ITEM) {
    if (is_func (item->t, HTAB))
      tabs << tab (N(items), item->t);
    else if (is_func (item->t, VSPACE_BEFORE) ||
	is_func (item->t, VSPACE_AFTER))
    {
      SI vmin, vdef, vmax;
      if (N(item->t)==1) {
	vmin= env->decode_length (as_string (item->t[0]) * "-");
	vdef= env->decode_length (item->t[0]);
	vmax= env->decode_length (as_string (item->t[0]) * "+");
      }
      else {
	vmin= env->decode_length (item->t[0]);
	vdef= env->decode_length (item->t[1]);
	vmax= env->decode_length (item->t[2]);
      }
      if (is_func (item->t, VSPACE_BEFORE))
	sss->vspace_before (space (vmin, vdef, vmax));
      else sss->vspace_after (space (vmin, vdef, vmax));
    }
    else if (L(item->t) == DECORATE_ATOMS)
      decs << tuple (as_string (N(items)), item->t);
    else if (item->t == NO_PAGE_BREAK_BEFORE)
      sss->no_page_break_before ();
    else if (item->t == NO_PAGE_BREAK_AFTER)
      sss->no_page_break_after ();
    else if (is_tuple (item->t, "env_page") ||
	     (item->t == PAGE_BREAK) ||
	     (item->t == NEW_PAGE) ||
	     (item->t == NEW_DOUBLE_PAGE))
      sss->print (item->t, nr_cols);
    else if (item->t == PAGE_BREAK_BEFORE)
      sss->print (PAGE_BREAK, nr_cols, true);
    else if (item->t == NEW_PAGE_BEFORE)
      sss->print (NEW_PAGE, nr_cols, true);
    else if (item->t == NEW_DOUBLE_PAGE_BEFORE)
      sss->print (NEW_DOUBLE_PAGE, nr_cols, true);
  }
  else if (item->type == FLOAT_ITEM) {
    fl << item->b->get_leaf_lazy ();
    // REPLACE item by item without lazy attachment !
  }

  if (N(spcs)>0) cur_w = cur_w + spcs[N(spcs)-1];
  items << item->b;
  spcs  << item->spc;
  item->b->x0= cur_w->def;
  item->b->y0= 0;
  cur_w =  cur_w + space (item->b->x2);
}

void
lazy_paragraph_rep::line_print (line_item item, path left, path right) {
  if (nil (left) && nil (right)) line_print (item);
  else {
    line_item item1, item2;
    hyphenate (item, nil (left)? right->item: left->item, item1, item2);
    line_print (nil (left) ? item1: item2,
		nil (left) ? left : left->next,
		nil (right)? right: right->next);
  }
}

void
lazy_paragraph_rep::line_print (path start, path end) {
  if (start->item == end->item)
    line_print (a[start->item], start->next, end->next);
  else {
    int i;
    line_print (a[start->item], start->next, path ());
    for (i=start->item+1; i<end->item; i++)
      line_print (a[i]);
    if (!atom (end))
      line_print (a[end->item], path (), end->next);
  }
}

/******************************************************************************
* Typesetting a line
******************************************************************************/

void
lazy_paragraph_rep::make_unit (string mode, SI the_width, bool break_flag) {
  int i;

  // format tabs
  //cout << "      " << N(tabs) << "] " << (cur_w->def/PIXEL)
  //     << " < " << (the_width/PIXEL) << "? (" << break_flag << ")\n";
  if (break_flag && (N(tabs)>0) && (cur_w->def<the_width)) {
    double tot_weight= 0.0;
    int pos_first= -1, pos_last=-1;
    int num_hflush= 0;
    for (i=0; i<N(tabs); i++) {
      tab& tab_i= tabs[i];
      tot_weight += tab_i->weight;
      if (tab_i->kind == tab_first && pos_first < 0) {
	num_hflush++; pos_first= i; }
      else if (tab_i->kind == tab_last) {
	if (pos_last < 0) num_hflush++;
	pos_last= i; 
      }
      else if (tab_i->kind == tab_all && tab_i->weight == 0.0)
	num_hflush++;
    }
    for (i=cur_start; i<N(items)-1; i++) items_sp << spcs[i]->def;
    for (i=0; i<N(tabs); i++) {
      double part;
      if (tot_weight==0.0) {
	if (i==pos_first || i==pos_last || tabs[i]->kind==tab_all)
	  part= 1.0 / num_hflush;
	else part= 0.0;
      }
      else part= tabs[i]->weight / tot_weight;
      items_sp[tabs[i]->pos] += (SI) (part * (the_width- cur_w->def));
    }
    return;
  }

  // stretching case
  if (mode == "justify") {
    if ((cur_w->def < the_width) &&
	(cur_w->max > cur_w->def) &&
	(!break_flag)) {
      double f=
	((double) (the_width - cur_w->def)) /
	((double) (cur_w->max - cur_w->def));
      for (i=cur_start; i<N(items)-1; i++)
	items_sp <<
	  (spcs[i]->def+ ((SI) (f*((double) spcs[i]->max- spcs[i]->def))));
      return;
    }
  }
  
  // shrinking case
  if ((cur_w->def > the_width) &&
      (cur_w->def > cur_w->min)) {
    double f=
      ((double) (cur_w->def - the_width)) /
      ((double) (cur_w->def - cur_w->min));
    if (f>1.0) f=1.0;
    for (i=cur_start; i<N(items)-1; i++)
      items_sp <<
	(spcs[i]->def- ((SI) (f*((double) spcs[i]->def- spcs[i]->min))));
    return;
  }

  if (mode == "center")
    items_sp[cur_start] += (the_width- cur_w->def) >> 1;
  if (mode == "right")
    items_sp[cur_start] += the_width- cur_w->def;
  for (i=cur_start; i<N(items)-1; i++)
    items_sp << spcs[i]->def;
}

/******************************************************************************
* Handling decorations
******************************************************************************/

void
lazy_paragraph_rep::handle_decoration (
  int& i, int& j, SI& xoff, box& b, SI& b_sp)
{
  string xoff_str= as_string (xoff) * "unit";
  array<box> new_items;
  array<SI>  new_items_sp;
  tree t= decs[j][1]; j++;
  handle_decorations (i, j, xoff, new_items, new_items_sp);
  b_sp= new_items_sp [0]; new_items_sp[0]= 0;
  b   = concat_box (ip, new_items, new_items_sp);

  int k, n=N(t);
  tree e (DECORATED_BOX);
  for (k=n-1; k>=0; k--)
    if (is_func (t[k], MACRO, 2))
      e= tree (EXPAND, t[k], e);
  if (e != tree (DECORATED_BOX)) {
    // cout << "Typesetting " << e << LF;
    env->decorated_boxes << b;
    tree old_xoff= env->local_begin (XOFF_DECORATIONS, xoff_str);
    box bb= typeset_as_concat (env, e, decorate_middle (ip));
    env->local_end (XOFF_DECORATIONS, old_xoff);
    env->decorated_boxes->resize (N (env->decorated_boxes) - 1);
    b= bb;
  }
}

void
lazy_paragraph_rep::handle_decorations (
  int& i, int& j, SI& xoff, array<box>& new_items, array<SI>& new_items_sp)
{
  while (i < N(items)) {
    // cout << "Handling " << items[i] << LF;
    if ((j < N (decs)) && (as_int (decs[j][0]) == i)) {
      tree t= decs[j][1];
      if (t == tree (DECORATE_ATOMS)) {
	xoff += items_sp[i] + items [i]->x2;
	new_items    << items [i];
	new_items_sp << items_sp [i];
	i++; j++;
	return;
      }
      else {
	box b;
	SI  b_sp;
	// cout << "Handling decoration " << t << LF << INDENT;
	handle_decoration (i, j, xoff, b, b_sp);
	// cout << UNINDENT << "Handled " << t << LF;
	new_items    << b;
	new_items_sp << b_sp;
      }
    }
    else {
      xoff += items_sp[i] + items [i]->x2;
      new_items    << items [i];
      new_items_sp << items_sp [i];
      i++;
    }
  }
}

void
lazy_paragraph_rep::handle_decorations () {
  // cout << "Handling decorations: " << decs << LF << INDENT;
  array<box> new_items;
  array<SI>  new_items_sp;
  int i=0, j=0;
  SI  xoff= 0;
  handle_decorations (i, j, xoff, new_items, new_items_sp);
  items   = new_items;
  items_sp= new_items_sp;
  // cout << UNINDENT << "Handled decorations " << decs << LF;

  array<tree> new_decs;
  for (i=0; i<N(decs); i++) {
    tree t= decs [i][1];
    if (t == tree (DECORATE_ATOMS))
      new_decs->resize (max (0, N(new_decs)-1));
    else new_decs << tuple ("0", t);
  }
  decs= new_decs;
  // cout << "Decorations on exit: " << decs << LF << HRULE;
}

/******************************************************************************
* Making lines
******************************************************************************/

void
lazy_paragraph_rep::line_start () {
  items   = array<box> ();
  items_sp= array<SI> ();
  spcs    = array<space> ();
  fl      = array<lazy> ();

  cur_r    = 0;
  cur_start= 0;
}

void
lazy_paragraph_rep::line_unit (path start, path end, bool break_flag,
			       string mode, SI the_left, SI the_right)
{
  tabs = array<tab> ();
  cur_w= space (0);
  int n= N(items_sp);
  SI  m= max (the_left- cur_r, 0);
  items_sp << m;

  SI the_width= the_right- the_left;
  line_print (start, end);
  make_unit (mode, the_width, break_flag);

  int i;
  cur_r= the_left+ items_sp[n]- m;
  for (i= cur_start; i<N(items); i++)
    cur_r += items[i]->w()+ (i<N(items)-1? items_sp[i+1]: 0);
  cur_start= N(items);
}

void
lazy_paragraph_rep::line_end (space spc, int penalty) {
  if (N(items) == 0) return;
  if (N(decs) != 0) handle_decorations ();
  // cout << items << ", " << spc << ", " << penalty << LF;
  box b= phrase_box (sss->ip, items, items_sp);
  sss->print (b, fl, nr_cols);
  sss->print (spc);
  sss->penalty (penalty);
  sss->flush ();
}

void
lazy_paragraph_rep::line_units (
  int start, int end,
  bool is_start, bool is_end, string mode, string hyphen,
  SI the_left, SI the_right, SI the_first, SI the_last)
{
  if (start == end) return;
  // cout << "  Line units " << start << ", " << end << "\n";
  // cout << "    is_end   : " << is_end << "\n";
  // cout << "    mode     : " << mode << "\n";
  // cout << "    hyphen   : " << hyphen << "\n";
  // cout << "    the_left : " << (the_left/PIXEL) << "\n";
  // cout << "    the_right: " << (the_right/PIXEL) << "\n";

  int i;
  bool ragged= (hyphen == "normal");
  array<path> hyphs= line_breaks (a, start, end, the_right-the_left,
				  the_first, the_last, ragged);
  for (i=0; i<N(hyphs)-1; i++) {
    if (i>0) line_start ();
    line_unit (hyphs[i], hyphs[i+1], i==N(hyphs)-2, mode,
	       the_left+ (is_start&&(i==0)? the_first: 0),
	       the_right- (is_end&&(i==N(hyphs)-2)? the_last: 0));
    if (i<N(hyphs)-2) line_end (line_sep, 1);
  }
  // cout << "    Done!\n";
}

/******************************************************************************
* Typesetting a paragraph
******************************************************************************/

void
lazy_paragraph_rep::format_paragraph_unit (int the_start, int the_end) {
  // cout << "Paragraph unit " << the_start << ", " << the_end << "\n";
  int i, start= the_start, end= the_start;
  for (i=the_start; i<=the_end; i++)
    if ((i==the_end) ||
	((a[i]->type == CONTROL_ITEM) &&
	 (a[i]->t == NEXT_LINE)))
    {
      start= end;
      end  = i;
      line_start ();
      line_units (start, end, start==the_start, end==the_end,
		  mode, hyphen,
		  left, width, first, 0);
      if (end<the_end) line_end (line_sep, 1);
      else return;
    }
  // cout << "Unit done\n";
}

void
lazy_paragraph_rep::format_paragraph () {
  width -= right;

  int start= 0, i, j;
  // cout << "Typeset " << a << "\n";
  for (i=0; i<=N(a); i++) {
    // determine the next unit
    if (i<N(a)) {
      if (a[i]->type != CONTROL_ITEM) continue;
      if (a[i]->t == NEW_LINE);
      else continue;
    }

    // determine the style parameters
    bool no_first= (style [PAR_NO_FIRST] == "true");
    style (PAR_NO_FIRST)= "false";
    if (no_first) style (PAR_FIRST)= "0cm";
    for (j=start; j<i; j++)
      if (a[j]->type == CONTROL_ITEM)
	if (is_tuple (a[j]->t, "env_par"))
	  style (a[j]->t[1]->label)= a[j]->t[2];
    no_first= (style [PAR_NO_FIRST] == "true");
    if (no_first) env->monitored_write_update (PAR_NO_FIRST, "true");
    if (mode == "center") first= 0;
    else first= env->decode_length (style [PAR_FIRST]);
    sss->set_env_vars (height, sep, hor_sep, bot, top);

    // typeset paragraph unit
    format_paragraph_unit (start, i);
    line_end (line_sep + par_sep, 0);
    sss->new_paragraph ();

    start= i;
  }
  // cout << "Paragraph done\n";

  /*
  array<path> ps;
  cout << "pass 3: " << a << "\n";
  ps= array<path> (N(a));
  for (i=0; i<N(a); i++) ps[i]= a[i]->p;
  cout << "paths : " << ps << "\n";
  */
}

/******************************************************************************
* User interface
******************************************************************************/

static array<line_item>
convert (edit_env env, array<box> bs, path ip) {
  array<line_item> a;
  int i, n=N(bs);
  for (i=0; i<n; i++) {
    if (i==0) {
      box b= empty_box (decorate (ip), 0, 0, 0, env->fn->yx);
      tree ct= tuple ("env_par", PAR_FIRST, "0cm");
      a << line_item (CONTROL_ITEM, b, 0, ct);
    }
    a << line_item (STD_ITEM, bs[i], 0);
    if (i<(n-1)) {
      box b= empty_box (decorate (ip), 0, 0, 0, env->fn->yx);
      a << line_item (CONTROL_ITEM, b, 0, NEXT_LINE);
    }
  }
  return a;
}

array<line_item>
typeset_concat_or_table (edit_env env, tree t, path ip) {
  if (is_func (t, TABLE)) {
    array<box> bs= typeset_as_var_table (env, t, ip);
    return convert (env, bs, ip);
  }
  else return typeset_concat (env, t, ip);
}

array<page_item>
typeset_stack (edit_env env, tree t, path ip,
	       array<line_item> a, array<line_item> b, stack_border& sb)
{
  // cout << "Typeset stack " << t << "\n";
  lazy_paragraph par (env, ip);
  par->a= a;
  par->a << typeset_concat_or_table (env, t, ip);
  par->a << b;
  par->format_paragraph ();
  sb= par->sss->sb;
  return par->sss->l;
}

lazy
make_lazy_paragraph (edit_env env, tree t, path ip) {
  lazy_paragraph par (env, ip);
  par->a= typeset_concat (env, t, ip);
  return par;
}

lazy
make_lazy_paragraph (edit_env env, array<box> bs, path ip) {
  lazy_paragraph par (env, ip);
  par->a= convert (env, bs, ip);
  return par;
}

array<line_item>
join (array<line_item> a, array<line_item> b) {
  int i, m= N(a), n= N(b);
  array<line_item> c (m+n);
  for (i=0; i<m; i++) c[i  ]= a[i];
  for (i=0; i<n; i++) c[i+m]= b[i];
  return c;
}

format
lazy_paragraph_rep::query (lazy_type request, format fm) {
  if ((request == LAZY_BOX) && (fm->type == QUERY_VSTREAM_WIDTH)) {
    array<line_item> li= a;
    query_vstream_width qvw= (query_vstream_width) fm;
    if (N (qvw->before) != 0) li= join (qvw->before, li);
    if (N (qvw->after ) != 0) li= join (li, qvw->after);

    SI w= 0;
    int i, n= N(li);
    for (i=0; i<n-1; i++)
      w += li[i]->spc->def + li[i]->b->x2;
    w += li[i]->b->x2;
    w= max (w, 1);  // width of a paragraph must be strictly positive for
                    // correct positioning inside tables
    return make_format_width (w);
  }
  return lazy_rep::query (request, fm);
}

lazy
lazy_paragraph_rep::produce (lazy_type request, format fm) {
  if (request == type) return this;
  if (request == LAZY_VSTREAM) {
    if (fm->type == FORMAT_VSTREAM) {
      format_vstream fs= (format_vstream) fm;
      width= fs->width;
      if (N (fs->before) != 0) a= join (fs->before, a);
      if (N (fs->after ) != 0) a= join (a, fs->after );
    }
    format_paragraph ();
    return lazy_vstream (ip, "", sss->l, sss->sb);
  }
  return lazy_rep::produce (request, fm);
}
