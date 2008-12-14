
/******************************************************************************
* MODULE     : verbatim.cpp
* DESCRIPTION: routines for converting between TeXmacs trees and text
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "convert.hpp"
#include "converter.hpp"
#include "vars.hpp"
#include "drd_std.hpp"

/******************************************************************************
* Verbatim input
******************************************************************************/

static void
tree_to_verbatim (string& buf, tree t, bool wrap, string enc) {
  if (is_atomic (t)) {
    string s;
    if (enc == "utf-8") s= cork_to_utf8 (t->label);
    else s= tm_decode (t->label);
    buf << s;
  }
  else if (is_func (t, SURROUND, 3)) {
    tree_to_verbatim (buf, t[0], wrap, enc);
    tree_to_verbatim (buf, t[2], wrap, enc);
    tree_to_verbatim (buf, t[1], wrap, enc);
  }
  else if (is_compound (t, "TeXmacs", 0))
    tree_to_verbatim (buf, "TeXmacs", wrap, enc);
  else {
    int i, n= N(t);
    for (i=0; i<n; i++)
      if (std_drd->is_accessible_child (t, i)) {
	if (is_document (t) && (i>0)) {
	  buf << "\n";
	  if (wrap) buf << "\n";
	}
	tree_to_verbatim (buf, t[i], wrap, enc);
      }
  }
}

string
unix_to_dos (string s) {
  int i, n= N(s);
  string r;
  for (i=0; i<n; i++)
    if (s[i] == '\n') r << "\r\n";
    else r << s[i];
  return r;
}

string
tree_to_verbatim (tree t, bool wrap, string enc) {
  if (!is_snippet (t)) t= extract (t, "body");
  string buf;
  tree_to_verbatim (buf, t, wrap, enc);
  if (wrap) {
    int i= 0, n= N(buf);
    while (i<n) {
      int start= i;
      while (i<n && buf[i]!='\n') i++;
      while (i-start > 78) {
	int mid= start+78;
	while (mid>start && buf[mid] != ' ') mid--;
	if (mid<=start) while (mid<i && buf[mid] != ' ') mid++;
	if (mid<i) { buf[mid]= '\n'; start= mid+1; }
	else break;
      }
      if (i<n) i++;
    }
  }
#ifdef OS_WIN32
  return unix_to_dos (buf);
#else
  return buf;
#endif
}

/******************************************************************************
* Verbatim output
******************************************************************************/

static string
un_special (string s) {
  int i, j;
  string r;
  for (i=0, j=0; i<N(s); i++, j++)
    if (s[i] == '\t') {
      do {
	r << " "; j++;
      } while ((j&7) != 0);
      j--;
    }
    else if ((s[i] == '\b') && (N(r)>0) && (r[N(r)-1]!='\n'))
      r->resize (N(r)-1);
    else r << s[i];
  return r;
}

static string
encode (string s, string enc) {
  if (enc == "utf-8") return utf8_to_cork (s);
  else return tm_encode (s);
}

tree
verbatim_to_tree (string s, string enc) {
  int i, j;
  for (i=0; i<N(s); i++)
    if (s[i]=='\n') {
      tree t (DOCUMENT);
      for (i=0, j=0; i<N(s); i++)
	if (s[i]=='\n') {
	  t << encode (un_special (s (j, i)), enc);
	  j= i+1;
	}
      t << encode (un_special (s (j, i)), enc);
      return t;
    }
  return encode (un_special (s), enc);
}

string
dos_to_unix (string s) {
  int i, n= N(s);
  string r;
  for (i=0; i<n; i++)
    if (s[i] == '\r' && i+1<n && s[i+1] == '\n') { r << "\n"; i++; }
    else r << s[i];
  return r;
}

string
mac_to_unix (string s) {
  int i, n= N(s);
  string r;
  for (i=0; i<n; i++)
    if (s[i] == '\r') r << "\n";
    else r << s[i];
  return r;
}

tree
verbatim_to_tree (string s, bool wrap, string enc) {
  s= mac_to_unix (dos_to_unix (s));
  if (wrap) {
    string r;
    int i, n= N(s);
    for (i=0; i<n; ) {
      if (s[i] == '\n' || s[i] == ' ' || s[i] == '\t') {
	int lf= 0;
	while (i<n && (s[i] == '\n' || s[i] == ' ' || s[i] == '\t')) {
	  if (s[i] == '\n') lf++;
	  i++;
	}
	if (lf <= 1) r << " ";
	else r << "\n";
      }
      else r << s[i++];
    }
    s= r;
  }
  return verbatim_to_tree (s, enc);
}

tree
verbatim_document_to_tree (string s, bool wrap, string enc) {
  tree t    = verbatim_to_tree (s, wrap, enc);
  tree init = tree (COLLECTION,
		    tree (ASSOCIATE, LANGUAGE, "verbatim"),
		    tree (ASSOCIATE, FONT_FAMILY, "tt"),
		    tree (ASSOCIATE, PAR_FIRST, "0cm"));
  return tree (DOCUMENT, compound ("body", t), compound ("initial", init));
}
