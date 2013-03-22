
/******************************************************************************
* MODULE     : convert.hpp
* DESCRIPTION: various conversion routines
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef CONVERT_H
#define CONVERT_H
#include "analyze.hpp"
#include "hashmap.hpp"
typedef tree scheme_tree;
class url;

/*** Miscellaneous ***/
bool   is_snippet (tree doc);
void   set_file_focus (url u);
url    get_file_focus ();

/*** Generic ***/
string suffix_to_format (string suffix);
string format_to_suffix (string format);
string get_format (string s, string suffix);
tree   generic_to_tree (string s, string format);
string tree_to_generic (tree doc, string format);

/*** Texmacs ***/
tree   texmacs_to_tree (string s);
tree   texmacs_document_to_tree (string s);
string tree_to_texmacs (tree t);
tree   extract (tree doc, string attr);
tree   extract_document (tree doc);
tree   change_doc_attr (tree doc, string attr, tree val);
hashmap<string,int> get_codes (string version);
tree   string_to_tree (string s, string version);
tree   upgrade (tree t, string version);
tree   substitute (tree t, tree which, tree by);

/*** Scheme ***/
string scheme_tree_to_string (scheme_tree t);
string scheme_tree_to_block (scheme_tree t);
scheme_tree tree_to_scheme_tree (tree t);
string tree_to_scheme (tree t);
scheme_tree string_to_scheme_tree (string s);
scheme_tree block_to_scheme_tree  (string s);
tree   scheme_tree_to_tree (scheme_tree t);
tree   scheme_tree_to_tree (scheme_tree t, string version);
tree   scheme_to_tree (string s);
tree   scheme_document_to_tree (string s);

/*** Verbatim ***/
string tree_to_verbatim (tree t, bool wrap= false, string enc= "default");
tree   verbatim_to_tree (string s, bool wrap= false, string enc= "default");
tree   verbatim_document_to_tree (string s, bool w= false, string e= "default");

/*** Latex ***/
tree   parse_latex (string s, bool change= false, bool using_cork= false);
tree   parse_latex_document (string s, bool change= false);
tree   latex_to_tree (tree t);
tree   latex_document_to_tree (string s);
tree   latex_class_document_to_tree (string s);
string latex_verbarg_to_string (tree t);
string get_latex_style (tree t);
string string_arg (tree t, bool url= false);
array<tree> tokenize_concat (tree t, array<tree> a);

/*** Xml / Html / Mathml ***/
tree   parse_xml (string s);
tree   parse_html (string s);
tree   tmml_upgrade (scheme_tree t);
tree   upgrade_mathml (tree t);

/*** BibTeX ***/
tree   parse_bib (string s);

/*** Post corrections ***/
bool   seems_buggy_html_paste (string s);
string correct_buggy_html_paste (string s);
bool   seems_buggy_paste (string s);
string correct_buggy_paste (string s);
tree   default_with_simplify (tree t);

#endif // defined CONVERT_H
