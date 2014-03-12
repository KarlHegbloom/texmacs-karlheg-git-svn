
/******************************************************************************
* MODULE     : conservative_fromtex.cpp
* DESCRIPTION: Conservative conversion of LaTeX to TeXmacs
* COPYRIGHT  : (C) 2014 Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "Tex/convert_tex.hpp"
#include "analyze.hpp"
#include "hashset.hpp"
#include "scheme.hpp"
#include "base64.hpp"
#include "iterator.hpp"
#include "fast_search.hpp"
#include "merge_sort.hpp"

/******************************************************************************
* Getting the TeXmacs attachments
******************************************************************************/

static bool
get_texmacs_attachments (string s, string& mod, tree& src, string& mtar) {
  string bs= "\n%%%%%%%%%% Begin TeXmacs source\n";
  string es= "\n%%%%%%%%%% End TeXmacs source\n";
  int bpos= search_forwards (bs, 0, s);
  if (bpos < 0) return false;
  int epos= search_forwards (es, bpos + N(bs), s);
  if (epos < 0) return false;
  int i= epos + N(es), n= N(s);
  while (i<n && (s[i] == ' ' || s[i] == '\t' || s[i] == '\n')) i++;
  if (i<n) return false;
  string comm_enc_atts= s (bpos + N(bs), epos);
  string enc_atts= replace (comm_enc_atts, "\n %", "\n");
  string atts= decode_base64 (enc_atts);
  // TODO: check integrity checksum
  string sep= "\n% Separate attachments\n";
  int spos= search_forwards (sep, 0, atts);
  if (spos < 0) return false;
  mod = s (0, bpos);
  src= scheme_to_tree (atts (0, spos));
  mtar= atts (spos + N(sep), N(atts));
  return true;
}

/******************************************************************************
* Computing the correspondence
******************************************************************************/

string
latex_correspondence (string mtar, hashset<path> l, hashmap<path,path>& corr) {
  hashmap<int,array<path> > pcorr;
  string tar= latex_unmark (mtar, l, pcorr);
  hashmap<path,int> inv (-1);
  iterator<int> it= iterate (pcorr);
  while (it->busy ()) {
    int pos= it->next ();
    array<path> a= pcorr[pos];
    for (int i=0; i<N(a); i++) {
      path p= a[i];
      path cp= path_up (p) * (1 - last_item (p));
      inv (p)= pos;
      int cpos= inv [cp];
      if (cpos >= 0) {
        int b= min (pos, cpos);
        int e= max (pos, cpos);
        corr (path_up (p))= path (b, e);
      }
    }
  }
  return tar;
}

string
latex_correspondence (string mtar, hashmap<path,path>& corr) {
  hashset<path> l;
  return latex_correspondence (mtar, l, corr);
}

/******************************************************************************
* Construct invarianted LaTeX document
******************************************************************************/

int
latex_best_match (int b, int e, array<int> ms, string orig, string modif) {
  if (N(ms) == 1) return ms[0];
  int best= -1, best_len= 0;
  for (int i=0; i<N(ms); i++) {
    int b_orig = b;
    int e_orig = e;
    int b_modif= ms[i];
    int e_modif= b_modif + (e_orig - b_orig);
    while (b_orig > 0 && b_modif > 0 &&
           orig[b_orig-1] == modif[b_modif-1]) {
      b_orig--;
      b_modif--;
    }
    while (b_orig + 1 < N(orig) && e_modif + 1 < N (modif) &&
           orig[e_orig+1] == modif[e_modif+1]) {
      e_orig++;
      e_modif++;
    }
    if (e_orig - b_orig > best_len) {
      best= i;
      best_len= e_orig - b_orig;
    }
    else if (e_orig - b_orig == best_len)
      best= -1;
  }
  //if (best >= 0)
  //cout << HRULE << "Multiple matches: " << ms
  //<< " -> " << ms[best] << LF << HRULE;
  return best;
}

void
latex_invarianted_search (string_searcher finder, tree t, path p, string tar,
                          hashmap<path,path> corr,
                          array<bool>& done, hashmap<int,path>& subs) {
  if (corr->contains (p)) {
    path r= corr[p];
    int b= r->item, e= r->next->item;
    if (b >= 0 && e <= N(tar)) {
      string ss= tar (b, e);
      array<int> ps= finder->search_all (ss);
      //cout << "Search " << ss << " ~~~> " << ps << LF;
      if (N(ps) >= 1) {
        string mod= finder->get_string ();
        int pos= latex_best_match (b, e, ps, tar, mod);
        if (pos >= 0) {
          int pos= ps[0];
          bool ok= true;
          for (int i= pos; i<pos+N(ss); i++)
            ok= ok && !done[i];
          if (ok) {
            for (int i= pos; i<pos+N(ss); i++)
              done[i]= true;
            subs (pos)= path (N(ss), p);
          }
        }
      }
    }
  }
  if (is_compound (t)) {
    for (int i=0; i<N(t); i++)
      latex_invarianted_search (finder, t[i], p * i, tar, corr, done, subs);
  }
}

string
latex_invarianted_apply (string s, hashmap<int,path> subs) {
  string r;
  bool empty_flag= true;
  int i= 0, n= N(s);
  while (i<n) {
    if (subs->contains (i) && empty_flag) {
      path p= subs[i];
      int len= p->item;
      string id= encode_as_string (p->next);
      r << "{\\itm{" << id << "}}";
      i += len;
      empty_flag= (len != 0);
    }
    else {
      r << s[i++];
      empty_flag= true;
    }
  }
  return r;
}

string
latex_invarianted (string s, tree src, string tar, hashmap<path,path> corr) {
  array<bool> done (N(s));
  hashmap<int,path> subs;
  for (int i=0; i<N(s); i++) done[i]= false;
  string_searcher finder (s);
  latex_invarianted_search (finder, src, path (), tar, corr, done, subs);
  //cout << "subs= " << subs << "\n";
  string invs= latex_invarianted_apply (s, subs);
  return invs;
}

/******************************************************************************
* Replace invarianted identifiers by corresponding trees
******************************************************************************/

tree
latex_invarianted_replace (tree t, tree src) {
  if (is_atomic (t)) return t;
  else if (is_compound (t, "itm", 1)) {
    path p= decode_as_path (as_string (t[0]));
    if (has_subtree (src, p)) return subtree (src, p);
    else return t;
  }
  else {
    int i, n= N(t);
    tree r (t, n);
    for (i=0; i<n; i++)
      r[i]= latex_invarianted_replace (t[i], src);
    return r;
  }
}

/******************************************************************************
* Conserve style and preamble in case of unchanged preambles
******************************************************************************/

bool
latex_unchanged_style (string old, string mod) {
  int old_pos= search_forwards ("\\documentclass", old);
  int mod_pos= search_forwards ("\\documentclass", mod);
  if (old_pos < 0 && mod_pos < 0) return true;
  if (old_pos < 0 || mod_pos < 0) return false;
  skip_line (old, old_pos);
  skip_line (mod, mod_pos);
  if (old_pos != mod_pos) return false;
  return old (0, old_pos) == mod (0, mod_pos);
}

tree
texmacs_recover_style (tree doc, tree src) {
  tree s= extract (src, "style");
  doc= change_doc_attr (doc, "style", s);
  return doc;
}

bool
latex_unchanged_preamble (string old, string mod) {
  int old_pos= search_forwards ("\\begin{document}", old);
  int mod_pos= search_forwards ("\\begin{document}", mod);
  if (old_pos < 0 && mod_pos < 0) return true;
  if (old_pos != mod_pos) return false;
  return old (0, old_pos) == mod (0, mod_pos);
}

tree
texmacs_recover_preamble (tree doc, tree src) {
  tree s= extract (src, "style");
  tree i= extract (src, "initial");
  doc= change_doc_attr (doc, "style", s);
  doc= change_doc_attr (doc, "initial", i);
  tree old_body= extract (src, "body");
  tree new_body= extract (doc, "body");
  if (!is_document (old_body)) old_body= tree (DOCUMENT, old_body);
  if (!is_document (new_body)) new_body= tree (DOCUMENT, new_body);
  if (is_compound (old_body[0], "hide-preamble", 1)) {
    if (is_compound (new_body[0], "hide-preamble", 1))
      new_body[0][0]= old_body[0][0];
    else
      new_body= tree (DOCUMENT, old_body[0]) * new_body;
    doc= change_doc_attr (doc, "body", new_body);
  }
  return doc;
}

/******************************************************************************
* Conserve style and preamble as much as possible otherwise
******************************************************************************/

static tree
rename_arguments (tree val, tree old) {
  if (is_func (val, MACRO) && is_func (old, MACRO) && N(val) == N(old)) {
    val= copy (val);
    int a= N(old) - 1;
    for (int i=0; i<a; i++)
      if (is_atomic (val[i]) && is_atomic (old[i])) {
        val[a]= replace (val[a], tree (ARG, val[i]), tree (ARG, old[i]));
        val[i]= old[i];
      }
  }
  return val;
}

tree
texmacs_merge_preamble (tree newt, tree oldt) {
  // Merge document styles
  tree olds= copy (extract (oldt, "style"));
  tree news= extract (newt, "style");
  if (is_atomic (olds)) olds= tuple (olds);
  if (is_atomic (news)) news= tuple (news);
  if (N(olds) >= 1 && N(news) >= 1 && news != olds) {
    //cout << "Old style: " << olds << LF;
    //cout << "New style: " << news << LF;
    olds[0]= news[0];
    for (int i=1; i<N(news); i++) {
      int j;
      for (j=0; j<N(olds); j++)
        if (olds[j] == news[i]) break;
      if (j == N(olds)) olds << news[i];
    }
    //cout << "Merged style: " << olds << LF;
    newt= change_doc_attr (newt, "style", olds);
  }

  // Merge initial settings of environment variables
  hashmap<string,tree> oldi (UNINIT, extract (oldt, "initial"));
  hashmap<string,tree> newi (UNINIT, extract (newt, "initial"));
  oldi->join (newi);
  newt= change_doc_attr (newt, "initial", make_collection (oldi));

  // Merge preambles
  tree oldb= extract (oldt, "body");
  tree newb= extract (newt, "body");
  if (!is_document (oldb)) oldb= tree (DOCUMENT, oldb);
  if (!is_document (newb)) newb= tree (DOCUMENT, newb);
  if (is_compound (oldb[0], "hide-preamble", 1) &&
      is_compound (newb[0], "hide-preamble", 1) &&
      is_document (oldb[0][0]) &&
      is_document (newb[0][0])) {
    tree oldp= copy (oldb[0][0]);
    tree newp= newb[0][0];
    //cout << "Old preamble: " << oldp << LF;
    //cout << "New preamble: " << newp << LF;
    int j, n= N(oldp);
    for (int i=0; i<N(newp); i++) {
      for (j=0; j<n; j++)
        if (newp[i] == oldp[j]) break;
      if (j<n) continue;
      if (is_func (newp[i], ASSIGN, 2)) {
        for (j=0; j<n; j++)
          if (is_func (oldp[j], ASSIGN, 2) && newp[i][0] == oldp[i][0]) break;
        if (j<n) oldp[j][1]= rename_arguments (newp[j][1], oldp[j][1]);
        else oldp << newp[j];
      }
      else if (!is_atomic (newp[i]))
        cout << "Ignored preamble change: " << newp[i] << LF;
    }
    oldb[0][0]= oldp;
    //cout << "Merged preamble: " << oldp << LF;
    newt= change_doc_attr (newt, "body", oldb);
  }
  return newt;
}

/******************************************************************************
* Conserve titles when possible
******************************************************************************/

bool
latex_unchanged_metadata (string old, string mod) {
  int old_start= search_forwards ("\\title", old);
  int mod_start= search_forwards ("\\title", mod);
  if (old_start < 0 || mod_start < 0) return false;
  int old_pos= search_forwards ("\\maketitle", old);
  int mod_pos= search_forwards ("\\maketitle", mod);
  if (old_pos < 0 && mod_pos < 0) {
    old_pos= search_forwards ("\\end{frontmatter}", old);
    mod_pos= search_forwards ("\\end{frontmatter}", mod);
  }
  if (old_pos < 0 || mod_pos < 0) return false;
  return old (old_start, old_pos) == mod (mod_start, mod_pos);
}

static int
search_doc_data (tree doc) {
  if (!is_document (doc)) return -1;
  int pos= -1;
  for (int i=0; i<N(doc); i++)
    if (is_compound (doc[i], "doc-data")) {
      if (pos != -1) return -1;
      pos= i;
    }
  return pos;
}

tree
texmacs_recover_metadata (tree doc, tree src) {
  tree old_body= extract (src, "body");
  tree new_body= extract (doc, "body");
  int old_pos= search_doc_data (old_body);
  int new_pos= search_doc_data (new_body);
  if (old_pos < 0 || new_pos < 0) return doc;
  new_body[new_pos]= old_body[old_pos];
  //cout << "Recovered metadata " << old_body[old_pos] << LF;
  doc= change_doc_attr (doc, "body", new_body);
  return doc;
}

/******************************************************************************
* Conservative TeXmacs -> LaTeX conversion
******************************************************************************/

tree
conservative_latex_to_texmacs (string s, bool as_pic) {
  if (get_preference ("latex->texmacs:conservative", "off") != "on")
    return tracked_latex_to_texmacs (s, as_pic);
  string mod;
  tree src;
  string mtar;
  bool ok= get_texmacs_attachments (s, mod, src, mtar);
  tree srcb= extract (src, "body");
  if (!ok) return tracked_latex_to_texmacs (s, as_pic);
  hashmap<path,path> corr;
  string tar= latex_correspondence (mtar, corr);
  if (s == tar) return src;
  //cout << "corr= " << corr << LF;
  string imod= latex_invarianted (mod, srcb, tar, corr);
  //cout << "imod= " << imod << LF;
  tree iconv= tracked_latex_to_texmacs (imod, as_pic);
  //cout << "iconv= " << iconv << LF;
  tree ibody= extract (iconv, "body");
  tree body= latex_invarianted_replace (ibody, srcb);
  tree conv= change_doc_attr (iconv, "body", body);
  if (latex_unchanged_style (tar, s))
    conv= texmacs_recover_style (conv, src);
  if (latex_unchanged_preamble (tar, s))
    conv= texmacs_recover_preamble (conv, src);
  else
    conv= texmacs_merge_preamble (conv, src);
  if (latex_unchanged_metadata (tar, s))
    conv= texmacs_recover_metadata (conv, src);
  //cout << "conv= " << conv << LF;
  return conv;
}