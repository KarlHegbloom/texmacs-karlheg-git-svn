
/******************************************************************************
* MODULE     : metadata-acm.cpp
* DESCRIPTION: conversion of (ACM) TeX metadata into TeXmacs metadata
* COPYRIGHT  : (C) 2012 Joris van der Hoeven, Poulain François
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "Tex/convert_tex.hpp"
#include "metadata.hpp"

static bool
is_acm_titlenote (tree t) {
  return is_tuple (t, "\\titlenote", 1) || is_tuple (t, "\\thanks", 1);
}

static bool
is_line_break (tree t) {
  return is_tuple (t, "\\\\") || is_tuple (t, "\\\\*");
}

static void
get_acm_title_notes (tree t, array<tree> &r) {
  if (is_atomic (t)) return;
  if (is_acm_titlenote (t)) {
    r << tree (APPLY, "\\doc-note", t[1]);
    return;
  }
  if (is_tuple (t, "\\tmacmmisc", 1)) {
    r << tree (APPLY, "\\doc-misc", t[1]);
    return;
  }
  int i, n=N(t);
  for (i=0; i<n; i++)
    get_acm_title_notes (t[i], r);
}

#define catm clean_acm_title_markup

static tree
clean_acm_title_markup (tree t) {
  if (is_atomic (t)) return t;
  if (is_acm_titlenote (t) ||
      is_tuple (t, "\\tmacmmisc", 1) || is_tuple (t, "\\tmacmsubtitle", 1))
    return concat();
  if (is_tuple (t, "\\ttlit")) {
    t[0]= "\\it";
    return t;
  }
  tree r (L(t));
  int i, n=N(t);
  for (i=0; i<n; i++)
      r << clean_acm_title_markup (t[i]);
  return r;
}

static array<tree>
get_acm_author_datas (tree t) {
  int i, n=N(t);
  bool line_break= true;
  array<tree> r;
  tree u;
  tree author_data (APPLY, "\\author-data");
  tree author_name (CONCAT);
  for (i=0; i<n; i++) {
    u= t[i];
    if (is_tuple (u, "\\alignauthor")) {
      line_break= false;
      if (N(author_name) > 1) {
        tree tmp= concat();
        for (int j=0; j<N(author_name); j++)
          if (j+1 < N(author_name) || !is_line_break (author_name[j]))
            tmp << author_name[j];
        if (N(tmp) > 1)
          author_data << tree (APPLY, "\\author-name", tmp);
        author_name= tree (CONCAT);
      }
      if (N(author_data) > 1) {
        r << author_data;
        author_data= tree (APPLY, "\\author-data");
      }
      line_break= true;
    }
    else if (is_acm_titlenote (u))
      author_data << tree (APPLY, "\\author-note", u[1]);
    else if (is_tuple (u, "\\affaddr", 1))
      author_data << tree (APPLY, "\\author-affiliation", u[1]);
    else if (is_tuple (u, "\\email", 1))
      author_data << tree (APPLY, "\\author-email", u[1]);
    else if (is_tuple (u, "\\tmacmhomepage", 1))
      author_data << tree (APPLY, "\\author-homepage", u[1]);
    else if (is_tuple (u, "\\tmacmmisc", 1))
      author_data << tree (APPLY, "\\author-misc", u[1]);
    else if (!line_break || !is_line_break (u)) {
      if (!line_break || (u != " " && u != concat (" ") && u != concat ()))
        author_name << u;
      if (is_line_break (u)) line_break= true;
      else if (u != " " && u != concat (" ") && u != concat ())
        line_break= false;
    }
  }

  if (N(author_name) > 1) {
    tree tmp= concat();
    for (int j=0; j<N(author_name); j++)
      if (j+1 < N(author_name) || !is_line_break (author_name[j]))
        tmp << author_name[j];
    if (N(tmp) > 1)
      author_data << tree (APPLY, "\\author-name", tmp);
    author_name= tree (CONCAT);
  }
  if (N(author_data) > 1) {
    r << author_data;
    author_data= tree (APPLY, "\\author-data");
  }
  return r;
}

tree
collect_metadata_acm (tree t) {
  int i, n=N(t);
  tree u, r (CONCAT);
  tree doc_data (APPLY, "\\doc-data");
  tree abstract_data (APPLY, "\\abstract-data");
  array<tree> doc_notes;
  for (i=0; i<n; i++) {
    u= t[i];
    if (is_tuple (u, "\\date", 1))
      doc_data << tree (APPLY, "\\doc-date", u[1]);
    else if (is_tuple (u, "\\title", 1)) {
      get_acm_title_notes (u[1], doc_notes);
      doc_data << tree (APPLY, "\\doc-title", catm (u[1]));
    }
    else if (is_tuple (u, "\\subtitle", 1)) {
      get_acm_title_notes (u[1], doc_notes);
      doc_data << tree (APPLY, "\\doc-subtitle", catm (u[1]));
    }
    else if (is_tuple (u, "\\author", 1)) {
      array<tree> author_datas= get_acm_author_datas (u[1]);
      for (int j=0; j<N(author_datas); j++)
        doc_data << tree (APPLY, "\\doc-author", author_datas[j]);
    }
    else if (is_tuple (u, "\\additionalauthors", 1)) {
      doc_data << tree (APPLY, "\\doc-author",
                             tree (APPLY, "\\author-data",
                                        tree (APPLY, "\\author-name", u[1])));
    }
    else if (is_tuple (u, "\\begin-abstract")) {
      tree abstract_text (CONCAT);
      i++;
      while (i<n && !is_tuple (t[i], "\\end-abstract"))
        abstract_text << t[i++];
      abstract_data << tree (APPLY, "\\abstract", abstract_text);
    }
    else if (is_tuple (u, "\\keywords", 1) || is_tuple (u, "\\terms", 1)) {
      array<tree> tmp= tokenize_concat (u[N(u)-1], A(concat (",", ";",
              tree (TUPLE, "\\tmsep"), tree (TUPLE, "\\tmSep"))));
      if (N(tmp) > 0) {
        tree kw= tree (APPLY, "\\abstract-keywords");
        kw << tmp;
        abstract_data << kw;
      }
    }
    else if (is_tuple (u, "\\category") || is_tuple (u, "\\category*")) {
      tree tmp (APPLY, "\\abstract-msc");
      for (int j=1; j<N(u); j++) tmp << u[j];
      abstract_data << tmp;
    }
  }
  if (N(doc_notes) > 0) doc_data << doc_notes;
  if (N(doc_data) > 1) r << doc_data << "\n";
  if (N(abstract_data) > 1) r << abstract_data << "\n";
  return r;
}

#undef catm
