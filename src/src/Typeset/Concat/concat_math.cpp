
/******************************************************************************
* MODULE     : concat_math.cpp
* DESCRIPTION: Typesetting mathematical markup
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "concater.hpp"
#include "analyze.hpp"

/******************************************************************************
* Typesetting special mathematical symbols
******************************************************************************/

void
concater_rep::typeset_large (tree t, path ip, int tp, int otp, string prefix) {
  if ((N(t) == 1) && is_atomic (t[0])) {
    string s= prefix * t[0]->label * ">";
    box b= text_box (ip, 0, s, env->fn, env->pen);
    print (tp, otp, b);
    // temporarary: use parameters from group-open class in std-math.syx
    // bug: allow hyphenation after ) and before *
  }
  else if ((N(t) == 2) && is_atomic (t[0]) && is_int (t[1])) {
    string s= prefix * t[0]->label * "-" * t[1]->label * ">";
    box b= text_box (ip, 0, s, env->fn, env->pen);
    SI dy= env->fn->yfrac - ((b->y1 + b->y2) >> 1);
    box mvb= move_box (ip, b, 0, dy, false, true);
    print (STD_ITEM, otp, macro_box (ip, mvb, env->fn));
  }
  else if ((N(t) >= 2) && is_atomic (t[0])) {
    SI y1, y2;
    if (N(t) == 2) {
      SI l= env->as_length (t[1]) >> 1;
      y1= env->fn->yfrac - l;
      y2= env->fn->yfrac + l;
    }
    else {
      y1= env->as_length (t[1]);
      y2= env->as_length (t[2]);
    }
    string s= prefix * t[0]->label * ">";
    box b= delimiter_box (ip, s, env->fn, env->pen, y1, y2);
    print (STD_ITEM, otp, b);
  }
  else typeset_error (t, ip);
}

static void
get_big_flags (string l, bool& int_flag, bool& it_flag, bool& lim_flag) {
  int n= N(l);
  if (n < 3) return;
  if (l[n-3] == 'l' && l[n-2] == 'i' && l[n-1] == 'm') {
    l= l (0, n-3);
    n -= 3;
    if (l[n-3] != 'i' || l[n-2] != 'n' || l[n-1] != 't') return;
    int_flag= true;
    it_flag = l[0] != 'u' || l[1] != 'p';
    lim_flag= true;
  }
  else {
    if (l[n-3] != 'i' || l[n-2] != 'n' || l[n-1] != 't') return;
    int_flag= true;
    it_flag = l[0] != 'u' || l[1] != 'p';
    lim_flag= false;
  }
}

void
concater_rep::typeset_bigop (tree t, path ip) {
  if ((N(t) == 1) && is_atomic (t[0])) {
    space spc= env->fn->spc;
    string l= t[0]->label;
    string s= "<big-" * l * ">";
    bool flag= (!env->math_condensed) && (l != ".");
    bool stix= starts (env->fn->res_name, "stix-");
    box b;
    if (stix) {
      font mfn= rubber_font (env->fn);
      b= big_operator_box (ip, s, mfn, env->pen,
                           env->display_style? 2: 1);
    }
    else if (env->display_style && env->fn->type == FONT_TYPE_UNICODE) {
      font mfn= rubber_font (env->fn);
      b= big_operator_box (ip, s, mfn, env->pen, 1);
    }
    else b= big_operator_box (ip, s, env->fn, env->pen,
                              env->display_style? 2: 1);
    print (STD_ITEM, OP_BIG, b);
    penalty_min (HYPH_PANIC);
    bool int_flag= false, it_flag= false, lim_flag= true;
    get_big_flags (l, int_flag, it_flag, lim_flag);
    if (lim_flag) with_limits (LIMITS_DISPLAY);
    if (flag) {
      if (int_flag && stix) print (env->display_style? (spc / 2): (spc / 4));
      else if (it_flag) print (env->display_style? 0: (spc / 4));
      else print (env->display_style? spc: (spc / 2));
    }
    // temporarary: use parameters from operator-big class in std-math.syx
  }
  else typeset_error (t, ip);
}

string
replace_primes (string s) {
  string r;
  int i, n= N(s);
  for (i=0; i<n; i++)
    if (s[i] == '\'') r << "<prime>";
    else if (s[i] == '`') r << "<backprime>";
    else r << s[i];
  return r;
}

void
concater_rep::typeset_lprime (tree t, path ip) {
  if ((N(t) == 1) && is_atomic (t[0])) {
    string s= t[0]->label;
    bool flag= (env->fn->type == FONT_TYPE_UNICODE);
    if (flag)
      for (int i=0; i<N(s); i++)
	flag= flag && (s[i] == '\'' || s[i] == '`');
    if (env->fn->type == FONT_TYPE_TEX)
      s= replace_primes (s);
    tree old_il;
    if (!flag) old_il= env->local_begin_script ();
    path sip= descend (ip, 0);
    box b1, b2;
    b2= typeset_as_concat (env, s /*t[0]*/, sip);
    b2= symbol_box (sip, b2, N(t[0]->label));
    b2= move_box (sip, b2,
		  flag? 0: env->as_length (string ("-0.05fn")),
		  flag? env->as_length ("-0.75ex"): 0);
    if (!flag) env->local_end_script (old_il);
    print (LSUP_ITEM, OP_SKIP, script_box (ip, b1, b2, env->fn));
    penalty_max (HYPH_INVALID);
  }
  else typeset_error (t, ip);
}

void
concater_rep::typeset_rprime (tree t, path ip) {
  if ((N(t) == 1) && is_atomic (t[0])) {
    string s= t[0]->label;
    bool flag= (env->fn->type == FONT_TYPE_UNICODE);
    if (flag)
      for (int i=0; i<N(s); i++)
	flag= flag && (s[i] == '\'' || s[i] == '`');
    if (env->fn->type == FONT_TYPE_TEX)
      s= replace_primes (s);
    tree old_il;
    if (!flag) old_il= env->local_begin_script ();
    path sip= descend (ip, 0);
    box b1, b2;
    b2= typeset_as_concat (env, s /*t[0]*/, sip);
    b2= symbol_box (sip, b2, N(t[0]->label));
    b2= move_box (sip, b2,
		  flag? 0: env->as_length (string ("0.05fn")),
		  flag? env->as_length ("-0.75ex"): 0);
    if (!flag) env->local_end_script (old_il);
    penalty_max (HYPH_INVALID);
    if (N(a)>0) a[N(a)-1]->limits= false;
    print (RSUP_ITEM, OP_SKIP, script_box (ip, b1, b2, env->fn));
  }
  else typeset_error (t, ip);
}

/******************************************************************************
* Typesetting scripts
******************************************************************************/

void
concater_rep::typeset_long_arrow (tree t, path ip) {
  if (N(t) != 2 && N(t) != 3) { typeset_error (t, ip); return; }
  tree old_ds= env->local_begin (MATH_DISPLAY, "false");
  tree old_mc= env->local_begin (MATH_CONDENSED, "true");
  tree old_il= env->local_begin_script ();
  box sup_b, sub_b;
  if (N(t) >= 2) {
    tree old_vp= env->local_begin (MATH_VPOS, "-1");
    sup_b= typeset_as_concat (env, t[1], descend (ip, 1));
    env->local_end (MATH_VPOS, old_vp);
  }
  if (N(t) >= 3) {
    tree old_vp= env->local_begin (MATH_VPOS, "1");
    sub_b= typeset_as_concat (env, t[2], descend (ip, 2));
    env->local_end (MATH_VPOS, old_vp);
  }
  env->local_end_script (old_il);
  env->local_end (MATH_CONDENSED, old_mc);
  env->local_end (MATH_DISPLAY, old_ds);

  string s= as_string (t[0]);
  SI w= sup_b->w();
  if (N(t) == 3) w= max (w, sub_b->w());
  w += env->fn->wquad;
  box arrow= wide_box (decorate (descend (ip, 0)), s, env->fn, env->pen, w);

  space spc= env->fn->spc;
  if (env->math_condensed) spc= space (spc->min>>3, spc->def>>3, spc->max>>2);
  else spc= space (spc->min>>1, spc->def>>1, spc->max);
  print (spc);
  print (limit_box (ip, arrow, sub_b, sup_b, env->fn, false));
  print (spc);
}

void
concater_rep::typeset_below (tree t, path ip) {
  if (N(t) != 2) { typeset_error (t, ip); return; }
  box b1= typeset_as_concat (env, t[0], descend (ip, 0));
  tree old_ds= env->local_begin (MATH_DISPLAY, "false");
  tree old_mc= env->local_begin (MATH_CONDENSED, "true");
  tree old_il= env->local_begin_script ();
  box b2= typeset_as_concat (env, t[1], descend (ip, 1));
  env->local_end_script (old_il);
  env->local_end (MATH_CONDENSED, old_mc);
  env->local_end (MATH_DISPLAY, old_ds);
  print (limit_box (ip, b1, b2, box (), env->fn, false));
}

void
concater_rep::typeset_above (tree t, path ip) {
  if (N(t) != 2) { typeset_error (t, ip); return; }
  box b1= typeset_as_concat (env, t[0], descend (ip, 0));
  tree old_ds= env->local_begin (MATH_DISPLAY, "false");
  tree old_mc= env->local_begin (MATH_CONDENSED, "true");
  tree old_il= env->local_begin_script ();
  box b2= typeset_as_concat (env, t[1], descend (ip, 1));
  env->local_end_script (old_il);
  env->local_end (MATH_CONDENSED, old_mc);
  env->local_end (MATH_DISPLAY, old_ds);
  print (limit_box (ip, b1, box (), b2, env->fn, false));
}

void
concater_rep::typeset_script (tree t, path ip, bool right) {
  if (N(t) != 1) { typeset_error (t, ip); return; }
  int type= RSUP_ITEM;
  box b1, b2;
  tree old_ds= env->local_begin (MATH_DISPLAY, "false");
  tree old_mc= env->local_begin (MATH_CONDENSED, "true");
  tree old_il= env->local_begin_script ();
  if (is_func (t, SUB (right))) {
    tree old_vp= env->local_begin (MATH_VPOS, "-1");
    b1= typeset_as_concat (env, t[0], descend (ip, 0));
    type= right? RSUB_ITEM: LSUB_ITEM;
    env->local_end (MATH_VPOS, old_vp);
  }
  if (is_func (t, SUP (right))) {
    tree old_vp= env->local_begin (MATH_VPOS, "1");
    b2= typeset_as_concat (env, t[0], descend (ip, 0));
    type= right? RSUP_ITEM: LSUP_ITEM;
    env->local_end (MATH_VPOS, old_vp);
  }
  env->local_end_script (old_il);
  env->local_end (MATH_CONDENSED, old_mc);
  env->local_end (MATH_DISPLAY, old_ds);
  if (right) penalty_max (HYPH_INVALID);
  a << line_item (type, OP_SKIP,
                  script_box (ip, b1, b2, env->fn), HYPH_INVALID);
  // do not use print, because of italic space
  if (!right) penalty_max (HYPH_INVALID);
}

/******************************************************************************
* Standard mathematical operations
******************************************************************************/

void
concater_rep::typeset_frac (tree t, path ip) {
  if (N(t) != 2) { typeset_error (t, ip); return; }
  bool disp= env->display_style;
  tree old;
  if (disp) old= env->local_begin (MATH_DISPLAY, "false");
  else old= env->local_begin_script ();
  tree old_vp= env->local_begin (MATH_VPOS, "1");
  box nom= typeset_as_concat (env, t[0], descend (ip, 0));
  env->local_end (MATH_VPOS, "-1");
  box den= typeset_as_concat (env, t[1], descend (ip, 1));
  env->local_end (MATH_VPOS, old_vp);
  font sfn= env->fn;
  if (disp) env->local_end (MATH_DISPLAY, old);
  else env->local_end_script (old);
  print (frac_box (ip, nom, den, env->fn, sfn, env->pen));
}

void
concater_rep::typeset_sqrt (tree t, path ip) {
  if (N(t) != 1 && N(t) != 2) { typeset_error (t, ip); return; }
  box b= typeset_as_concat (env, t[0], descend (ip, 0));
  box ind;
  if (N(t)==2) {
    bool disp= env->display_style;
    tree old;
    if (disp) old= env->local_begin (MATH_DISPLAY, "false");
    tree old_il= env->local_begin_script ();
    ind= typeset_as_concat (env, t[1], descend (ip, 1));
    env->local_end_script (old_il);
    if (disp) env->local_end (MATH_DISPLAY, old);
  }
  SI sep= env->fn->sep;
  box sqrtb= delimiter_box (decorate_left (ip), "<large-sqrt>",
                            env->fn, env->pen, b->y1, b->y2+ sep);
  print (sqrt_box (ip, b, ind, sqrtb, env->fn, env->pen));
}

void
concater_rep::typeset_wide (tree t, path ip, bool above) {
  if (N(t) != 2) { typeset_error (t, ip); return; }
  box b= typeset_as_concat (env, t[0], descend (ip, 0));
  string s= as_string (t[1]);
  if (s == "^") s= "<hat>";
  if (s == "~") s= "<tilde>";
  bool request_wide= is_func (t, VAR_WIDE);
  if (starts (s, "<wide-")) {
    s= "<" * s (6, N(s));
    request_wide= true;
  }
  if (ends (s, "brace>") || ends (s, "brace*>"))
    b= move_box (decorate_middle (descend (ip, 0)), b, 0, 0, true);
  print (wide_box (ip, b, s, env->fn, env->pen, request_wide, above));
  if ((s == "<underbrace>") || (s == "<overbrace>") ||
      (s == "<squnderbrace>") || (s == "<sqoverbrace>"))
    with_limits (LIMITS_ALWAYS);
}

void
concater_rep::typeset_neg (tree t, path ip) {
  if (N(t) != 1) { typeset_error (t, ip); return; }
  box b= typeset_as_concat (env, t[0], descend (ip, 0));
  print (neg_box (ip, b, env->fn, env->pen));
}

/******************************************************************************
* Other markup
******************************************************************************/

static string
bracket_color (int nl) {
  switch (nl % 3) {
  case 0 : return "#662266";
  case 1 : return "#226666";
  default: return "#663322";
  }
}

static tree
make_large (tree_label l, tree t) {
  if (!is_atomic (t)) return tree (l, ".");
  string s= t->label;
  if (N(s) <= 1) return tree (l, s);
  if (s[0] != '<' || s[N(s)-1] != '>' || s == "<nobracket>")
    return tree (l, ".");
  return tree (l, s (1, N(s)-1));
}

void
concater_rep::typeset_around (tree t, path ip, bool colored) {
  if (colored) {
    int nl= env->get_int (MATH_NESTING_LEVEL);
    tree old_col= env->local_begin (COLOR, bracket_color (nl));
    tree old_nl = env->local_begin (MATH_NESTING_LEVEL, as_string (nl+1));
    typeset_around (t, ip, false);
    env->local_end (MATH_NESTING_LEVEL, old_nl);
    env->local_end (COLOR, old_col);
  }
  else {
    marker (descend (ip, 0));
    switch (L(t)) {
    case AROUND:
      if (N(t) == 3) {
        box br1= typeset_as_concat (env, t[0], descend (ip, 0));
        print (STD_ITEM, OP_OPENING_BRACKET, br1);
        typeset (t[1], descend (ip, 1));
        box br2= typeset_as_concat (env, t[2], descend (ip, 2));
        print (STD_ITEM, OP_CLOSING_BRACKET, br2);
      }
      else typeset_error (t, ip);
      break;
    case VAR_AROUND:
      if (N(t) == 3) {
        font old_fn= env->fn;
        font new_fn= env->fn;
        if (starts (new_fn->res_name, "stix-"))
          new_fn= rubber_font (new_fn);
        env->fn= new_fn;
        typeset (make_large (LEFT, t[0]),
                 decorate_middle (descend (ip, 0)));
        env->fn= old_fn;
        typeset (t[1], descend (ip, 1));
        env->fn= new_fn;
        typeset (make_large (RIGHT, t[2]),
                 decorate_middle (descend (ip, 2)));
        env->fn= old_fn;
      }
      else typeset_error (t, ip);
      break;
    case BIG_AROUND:
      if (N(t) == 2) {
        typeset (make_large (BIG, t[0]),
                 decorate_middle (descend (ip, 0)));
        typeset (t[1], descend (ip, 1));
      }
      else typeset_error (t, ip);
      break;
    default:
      break;
    }
    marker (descend (ip, 1));
  }
}

void
concater_rep::typeset_tree (tree t, path ip) {
  if (N(t) == 0) { typeset_error (t, ip); return; }
  int i, n= N(t);
  array<box> bs(n);
  for (i=0; i<n; i++) bs[i]= typeset_as_concat (env, t[i], descend (ip, i));
  print (tree_box (ip, bs, env->fn, env->pen));
}

void
concater_rep::typeset_table (tree t, path ip) {
  box b= typeset_as_table (env, t, ip);
  print (b);
}
