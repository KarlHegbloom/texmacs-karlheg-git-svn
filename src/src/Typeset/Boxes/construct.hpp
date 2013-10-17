
/******************************************************************************
* MODULE     : construct.hpp
* DESCRIPTION: the exported box construction routines
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef CONSTRUCT_H
#define CONSTRUCT_H
#include "boxes.hpp"
#include "array.hpp"
#include "font.hpp"
#include "command.hpp"

class frame;

/******************************************************************************
* Construction routines for boxes
******************************************************************************/

box empty_box (path ip, int x1=0, int y1=0, int x2=0, int y2=0);
box marker_box (path ip, int x1, int y1, int x2, int y2, box ref);
box test_box (path ip);
box line_box (path ip, SI x1, SI y1, SI x2, SI y2, pencil pen);
box arc_box (path ip, SI x1, SI y1, SI x2, SI y2, int a1, int a2, pencil p);
box polygon_box (path ip, array<SI> x, array<SI> y, brush fill);
box polygon_box (path ip, array<SI> x, array<SI> y, brush fill, pencil pen);
box image_box (path ip, url u, SI w, SI h, int alpha, int px);

box text_box (path ip, int pos, string s, font fn, pencil pen);
box delimiter_box (path ip, string s, font fn, pencil pen, SI y1, SI y2);
box big_operator_box (path ip, string s, font fn, pencil pen, int n);
box wide_box (path ip, string s, font fn, pencil pen, SI width);
box bracket_box (path ip, int br_type, pencil pen, SI y1, SI y2);
box wide_hat_box (path ip, SI x1, SI x2, pencil pen);
box wide_tilda_box (path ip, SI x1, SI x2, pencil pen);
box wide_bar_box (path ip, SI x1, SI x2, pencil pen);
box wide_vect_box (path ip, SI x1, SI x2, pencil pen);
box wide_check_box (path ip, SI x1, SI x2, pencil pen);
box wide_breve_box (path ip, SI x1, SI x2, pencil pen);
box wide_invbreve_box (path ip, SI x1, SI x2, pencil pen);
box wide_squbr_box (path ip, SI x1, SI x2, pencil pen);
box wide_sqobr_box (path ip, SI x1, SI x2, pencil pen);
box control_box (path ip, tree t, font fn);
box control_box (path ip, box b, font fn);
box control_box (path ip, lazy lz, font fn);

box concat_box (path ip, array<box> bs, array<SI> spc, bool indent= false);
box phrase_box (path ip, array<box> bs, array<SI> spc);
box stack_box (path ip, array<box> bs, array<SI> spc);
box composite_box (path ip, array<box> bs, bool bfl= true);
box composite_box (path ip, array<box> bs, array<SI> x, array<SI> y,
		   bool bfl= true);
box table_box (path ip, array<box> bs, array<SI> x, array<SI> y,
               array<string> halign, int cols);
box superpose_box (path ip, array<box> bs, bool bfl= true);
box scatter_box (path ip, array<box> bs, array<SI> x, array<SI> y);
box cell_box (path ip, box b, SI x0, SI y0, SI x1, SI y1, SI x2, SI y2,
	      SI bl, SI br, SI bb, SI bt, brush fg, brush bg);
box remember_box (path ip, box b);
box highlight_box (path ip, box b, box xb, tree shape, SI w, SI xpad, SI ypad,
		   brush bg, brush xc, brush sunny, brush shadow);

box frac_box (path ip, box b1, box b2, font fn, font sfn, pencil pen);
box sqrt_box (path ip, box b1, box b2, box sqrtb, font fn, pencil pen);
box neg_box (path ip, box b, font fn, pencil pen);
box tree_box (path ip, array<box> bs, font fn, pencil pen);
box wide_box (path ip, box ref, string s, font fn, pencil p, bool wf, bool af);
box repeat_box (path ip, box ref, box repeat, SI xoff=0);
box limit_box (path ip, box ref, box lo, box hi, font fn, bool glued);
box script_box (path ip, box b1, box b2, font fn);
box left_script_box (path ip, box ref, box b1, box b2, font fn, int level);
box right_script_box (path ip, box ref, box b1, box b2, font fn, int level);
box side_box (path ip, box ref, box l1, box l2, box r1, box r2, font f, int l);
box specific_box (path ip, box b, bool printer_flag, font fn);
box flag_box (path ip, box b, SI h, pencil dark, brush light);
box info_box (path ip, SI h, pencil dark, brush light);
box scrollbar_box (path ip, box b, bool vertical, SI span, tree t);

box symbol_box (path ip, box b, int n);
box shorter_box (path ip, box b, int n);
box frozen_box (path ip, box b);
box move_box (path ip, box b, SI x, SI y, bool chf= false, bool bigf= false);
box shift_box (path ip, box b, SI x, SI y, bool chf= false, bool bigf= false);
box resize_box (path ip, box b, SI x1, SI y1, SI x2, SI y2,
		bool chf= false, bool adjust= false);
box vresize_box (path ip, box b, SI y1, SI y2);
box transformed_box (path ip, box b, frame f);
box effect_box (path ip, array<box> bs, tree eff);
box clip_box (path ip, box b, SI x1, SI y1, SI x2, SI y2);
box clip_box (path ip, box b, SI x1, SI y1, SI x2, SI y2,
	      tree xt, tree yt, SI scx, SI scy);
box vcorrect_box (path ip, box b, SI top_cor, SI bot_cor);
box page_box (path ip, tree page, SI w, SI h,
	      array<box> bs  , array<SI> bs_x  , array<SI> bs_y,
	      array<box> decs, array<SI> decs_x, array<SI> decs_y);
box action_box (path ip, box b, tree filter, command cmd, bool child_flag);
box action_box (path ip, box b, tree f, command c, bool ch, path vip);
box locus_box (path ip, box b, list<string> ids, SI pixel);
box locus_box (path ip, box b, list<string> ids, SI pixel, string ref, string anchor);
box macro_box (path ip, box b, font big_fn= font ());
box tag_box (path ip, box b, tree keys);
box note_box (path ip, box b, box note, SI nx, SI ny);

box anim_compose_box (path ip, array<box> b);
box anim_repeat_box (path ip, box b);
box anim_constant_box (path ip, box b, int l);
box anim_translate_box (path ip, box b, int len, SI sx, SI sy, SI ex, SI ey);
box anim_progressive_box (path ip, box b, int len, rectangle r1, rectangle r2);
box sound_box (path ip, url u, SI h);
box video_box (path ip, url u, SI w, SI h, int a, int msecs, bool rep, int px);

#endif // defined CONSTRUCT_H
