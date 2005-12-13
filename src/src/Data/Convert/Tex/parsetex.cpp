
/******************************************************************************
* MODULE     : parsetex.cpp
* DESCRIPTION: conversion of tex/latex strings into logical tex/latex trees
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "Tex/convert_tex.hpp"
#include "converter.hpp"

/******************************************************************************
* The latex_parser structure
*******************************************************************************
*
* During the parsing, the following global variables are used:
*
*     command_type   Contains the types of all currently defined tex commands.
*                    This is either 'command' 'modifier' 'operator'
*                    'environment' 'list' 'symbol' 'big-symbol' or 'user'.
*     command_arity  Contains the corresponding arity.
*     command_def    Contains the definitions of user commands.
*
* The command_type hashmap also contains come special fields
*
*     \<sub>         Stands for the subscript command
*     \<sup>         Stands for the supscript command
*
*     !mode          Gives the current mode ("text" or "math").
*     !verbatim      Verbatim mode ("true" or "false")
*     !em            Emphasized mode ("true" or "false")
*
*******************************************************************************
* WARNING: we recently put the standard LaTeX macros in latex_type and
* latex_arity instead of command_type and command_arity.
******************************************************************************/

struct latex_parser {
  bool unicode;
  latex_parser (bool unicode2): unicode (unicode2) {}
  void latex_error (string s, int i, string message);

  tree parse           (string s, int& i, string stop= "", bool change= false);
  tree parse_backslash (string s, int& i);
  tree parse_symbol    (string s, int& i);
  tree parse_command   (string s, int& i, string which);
  tree parse_unknown   (string s, int& i, string which);
  tree parse_verbatim  (string s, int& i, string end);

  tree parse           (string s);
};

/******************************************************************************
* Error handling
******************************************************************************/

void
latex_parser::latex_error (string s, int i, string message) {
  cerr << "Latex error] " << message << "\n";
  if (i>30) s= "..." * s (i-27, N(s));
  if (N(s)>60) s= s (0, 57) * "...";
  cerr << "Latex error] in " << s << "\n";
}

/******************************************************************************
* Main parsing routine
******************************************************************************/

tree
latex_parser::parse (string s, int& i, string stop, bool change) {
  bool no_error= true;
  int n= N(s);
  tree t (CONCAT);

  command_type ->extend ();
  command_arity->extend ();
  command_def  ->extend ();

  while ((i<n) && is_space (s[i])) i++;
  while ((i<n) && no_error &&
	 (s[i] != '\0' || N (stop) != 0) &&
	 (N(stop) != 1 || s[i] != stop[0]) &&
	 (s[i] != '$' || stop != "$$" || i+1>=n || s[i+1] != '$')) {
    switch (s[i]) {
    case '~':
      if (command_type ["!mode"] == "math") t << tuple ("\\sim");
      else t << tuple ("\\nbsp");
      i++;
      break;
    case ' ':
    case '\t':
    case '\r':
      while ((i<n) && ((s[i]==' ') || (s[i]=='\t') || (s[i]=='\r'))) i++;
      if ((i<n) && (s[i]!='\n')) t << " ";
      break;
    case '\n': {
      int ln=0;
      while ((i<n) && is_space (s[i]))
	if (s[i++]=='\n') ln++;
      if (i<n) {
	if (ln == 1) t << " ";
	else t << "\n";
      }
      break;
    }
    case '%':
      if (test (s, i, "%%%%%%%%%% Start TeXmacs macros\n")) {
	while ((i<n) && (!test (s, i, "%%%%%%%%%% End TeXmacs macros\n")))
	  i++;
	i += 30;
      }
      else {
	while ((i<n) && (s[i]!='\n')) i++;
	if (i<n) i++;
	int ln=0;
	while ((i<n) && is_space (s[i]))
	  if (s[i++]=='\n') ln++;
	if (ln > 0) {
	  if ((N(t)>0) && ((t[N(t)-1]==" ") || (t[N(t)-1]=="\n")))
	    t[N(t)-1]= "\n";
	  else t << "\n";
	}
      }
      break;
    case '#':
      i++;
      if (i==n) return t;
      if (is_numeric (s[i])) {
	t << s (i-1, i+1);
	i++;
      }
      else t << s (i-1, i);
      break;
    case '\\':
      if (((i+7)<n) && (s(i,i+5)=="\\over") && (!is_alpha (s (i+5, i+7)))) {
	i+=5;
	tree arg= parse_command (s, i, "\\over");
	while ((N(t)>=1) &&
	       ((t[N(t)-1] == " ") || (t[N(t)-1] == tree (TUPLE, "\\ "))))
	  t= t (0, N(t)-1);
	if (is_tuple (arg, "\\over", 1) && (N(t)>=1))
	  t[N(t)-1]= tree (TUPLE, "\\frac", t[N(t)-1], arg[1]);
      }
      else if (((i+5)<n) && (s(i,i+3)=="\\sp") && (!is_alpha(s[i+3]))) {
	i+=3;
	t << parse_command (s, i, "\\<sup>");
      }
      else if (((i+5)<n) && (s(i,i+3)=="\\sb") && (!is_alpha(s[i+3]))) {
	i+=3;
	t << parse_command (s, i, "\\<sub>");
      }
      else if (((i+10)<n) && (s(i,i+8)=="\\pmatrix")) {
	i+=8;
	tree arg= parse_command (s, i, "\\pmatrix");
	if (is_tuple (arg, "\\pmatrix", 1)) arg= arg[1];
	t << tree (TUPLE, "\\begin-pmatrix");
	if (is_concat (arg)) t << A (arg);
	else t << arg;
	t << tree (TUPLE, "\\end-pmatrix");
      }
      else  {
	tree u= parse_backslash (s, i);
	if (u != "") t << u;
      }
      break;
    case '\'':
      i++;
      if (command_type ["!mode"] == "math") {
	int start= i-1;
	while ((i < N(s)) && (s[i] == '\'')) i++;
	t << tuple ("\\prime", s (start, i));
      }
      else t << s (i-1, i);
      break;
    case '*':
      if (command_type ["!mode"] == "math") t << tree (TUPLE, "\\ast");
      else t << "*";
      i++;
      break;
    case '_':
      i++;
      t << parse_command (s, i, "\\<sub>");
      /*
      if (command_type ["!mode"] == "math")
	t << parse_command (s, i, "\\<sub>");
      else t << s (i-1, i);
      */
      break;
    case '^':
      i++;
      t << parse_command (s, i, "\\<sup>");
      /*
      if (command_type ["!mode"] == "math")
	t << parse_command (s, i, "\\<sup>");
      else t << s (i-1, i);
      */
      break;
    case '<':
      t << tree (TUPLE, "\\<less>");
      i++;
      break;
    case '>':
      t << tree (TUPLE, "\\<gtr>");
      i++;
      break;
    case '\244':
      i++;
      t << parse_verbatim (s, i, "\244");
      break;
    case '{': {
      i++;
      t << parse (s, i, "}");
      if ((i<n) && (s[i]=='}')) i++;

      int ln=0;
      if ((i<n) && (!is_space (s[i]))) break;
      while ((i<n) && is_space (s[i]))
	if (s[i++]=='\n') ln++;
      if (ln >= 2) t << "\n"; else t << tree (TUPLE, "\\ ");
      break;
    }
    case '$': {
      i++;
      if ((i<n) & (s[i]=='$')) {
	i++;
	t << tree (TUPLE, "\\begin-displaymath");
	command_type ("!mode")= "math";
	t << parse (s, i, "$$");
	command_type ("!mode")= "text";
	if ((i<n) && (s[i]=='$')) i++;
	if ((i<n) && (s[i]=='$')) i++;
	t << tree (TUPLE, "\\end-displaymath");
      }
      else {
	t << tree (TUPLE, "\\begin-math");
	command_type ("!mode")= "math";
	t << parse (s, i, "$");
	command_type ("!mode")= "text";
	if ((i<n) && (s[i]=='$')) i++;
	t << tree (TUPLE, "\\end-math");
      }
      break;
    }
    default:
      if (unicode && ((unsigned char) s[i]) >= 128) {
	unsigned int code= decode_from_utf8 (s, i);
	t << tree (TUPLE, "\\#" * as_hexadecimal (code));
      }
      else if (!unicode && is_iso_alpha (s[i])) {
	// If we encounter too much text in math mode, then return
	int start= i;
	while ((i<n) && is_iso_alpha (s[i])) i++;
	int end= i;
	if ((i >= start+3) && (command_type ["!mode"] == "math")) {
	  while ((i<n) && (is_iso_alpha (s[i]) ||
			   is_punctuation (s[i]) ||
			   is_space (s[i])))
	    i++;
	  if (i >= start+20) {
	    int last= i, words= 0, letters= 0;
	    for (i=start; i<last; i++) {
	      if (is_iso_alpha (s[i])) {
		letters++;
		if ((i==start) || (!is_iso_alpha (s[i-1]))) words++;
	      }
	    }
	    if ((words > 3) && (letters/words >= 3) && (letters >= 15)) {
	      i= start;
	      no_error= false;
	    }
	  }
	}
	if (no_error)
	  for (i=start; i<end; i++)
	    t << s(i, i+1);
      }
      else {
	t << s (i, i+1);
	i++;
      }
      break;
    }
  }

  if (change) {
    command_type ->merge ();
    command_arity->merge ();
    command_def  ->merge ();
  }
  else {
    command_type ->shorten ();
    command_arity->shorten ();
    command_def  ->shorten ();
  }

  if (N(t)==0) return "";
  if (N(t)==1) return t[0];
  return t;
}

/******************************************************************************
* Parsing commands
******************************************************************************/

tree
latex_parser::parse_backslash (string s, int& i) {
  int n= N(s);
  if (((i+7)<n) && (s(i,i+5)=="\\verb")) {
    i+=6;
    return parse_verbatim (s, i, s(i-1,i));
  }
  if (((i+29)<n) && (s(i,i+16)=="\\begin{verbatim}")) {
    i+=16;
    return parse_verbatim (s, i, "\\end{verbatim}");
  }
  if (((i+6)<n) && (s(i,i+5)=="\\url{")) {
    i+=5;
    return parse_verbatim (s, i, "}");
  }

  /************************ special commands *********************************/
  i++;
  if (i==n) return "";
  if (s[i]==' ') {
    i++;
    return tree (TUPLE, "\\ ");
  }
  if (!is_alpha(s[i])) {
    i++;
    if (s[i-1]=='(') return parse_command (s, i, "\\begin-math");
    if (s[i-1]==')') return parse_command (s, i, "\\end-math");
    if (s[i-1]=='[') return parse_command (s, i, "\\begin-displaymath");
    if (s[i-1]==']') return parse_command (s, i, "\\end-displaymath");
    return parse_command (s, i, s (i-2, i));
  }

  /************************* normal commands *********************************/
  int start= i-1;
  while ((i<n) && is_alpha (s[i])) i++;
  if ((i<n) && (s[i]=='*') && latex_type (s (start, i+1)) != "undefined") i++;
  string r= s (start, i);
  if ((r == "\\begin") || (r == "\\end")) {
    while ((i<n) && is_space (s[i])) i++;
    if ((i==n) || (s[i]!='{')) {
      latex_error (s, i, "begin or end which environment ?");
      return "";
    }
    i++; start= i;
    while ((i<n) && (s[i]!='}')) i++;
    r = r * "-" * s (start, i);
    if (i<n) i++;
  }
  return parse_command (s, i, r);
}

static string
sharp_to_arg (string s, tree args) {
  int i;
  string r;
  for (i=0; i<N(s); i++)
    if ((s[i]=='#') && ((i+1)<N(s)) && (s[i+1]>='1') && (s[i+1]<='9')) {
      int nr= ((int) s[++i]) - ((int) '0');
      if (N(args)>nr) r << as_string (args[nr]);
    }
    else r << s[i];
  return r;
}

tree
latex_parser::parse_symbol (string s, int& i) {
  int start= i;
  if ((s[i] == '*') && (command_type ["!mode"] == "math")) {
    i++; return tree (TUPLE, "\\ast"); }
  if (s[i] == '<') { i++; return tree (TUPLE, "\\<less>"); }
  if (s[i] == '>') { i++; return tree (TUPLE, "\\<gtr>"); }
  if (s[i] != '\\') { i++; return s(start, i); }
  i++;
  if (i == N(s)) return tree (TUPLE, "\\backslash");
  if (!is_alpha (s[i])) { i++; return s(start, i); }
  while ((i<N(s)) && is_alpha (s[i])) i++;
  if ((i<N(s)) && (s[i]=='*')) i++;
  return s(start,i);
}

tree
latex_parser::parse_command (string s, int& i, string cmd) {
  /*
  cout << cmd << " [" << latex_type (cmd) << ", "
       << command_type ["!mode"] << "]" << LF;
  */
  if (cmd == "\\newcommand") cmd= "\\def";
  if (cmd == "\\renewcommand") cmd= "\\def";
  if (cmd == "\\renewenvironment") cmd= "\\newenvironment";
  if (cmd == "\\begin-split") cmd= "\\begin-eqsplit";
  if (cmd == "\\end-split") cmd= "\\end-eqsplit";
  if (cmd == "\\begin-split*") cmd= "\\begin-eqsplit*";
  if (cmd == "\\end-split*") cmd= "\\end-eqsplit*";
  if (latex_type (cmd) == "undefined")
    return parse_unknown (s, i, cmd);

  if (latex_type (cmd) == "math-environment") {
    if (cmd (0, 6) == "\\begin") command_type ("!mode") = "math";
    else command_type ("!mode") = "text";
  }

  bool mbox_flag=
    ((cmd == "\\text") || (cmd == "\\mbox")) &&
    (command_type ["!mode"] == "math");
  if (mbox_flag) command_type ("!mode") = "text";

  int  n     = N(s);
  int  arity = latex_arity (cmd);
  bool option= (arity<0);
  if (option) arity= -1-arity;

  /************************ retrieve arguments *******************************/
  tree t (TUPLE, copy (cmd)); // parsed arguments
  tree u (TUPLE, copy (cmd)); // unparsed arguments
  while (i<n && arity>=0 && (arity>0 || option)) {
    int j= i;
    while ((j<n) && is_space (s[j])) j++;
    if (j==n) break;
    if (option && (s[j]=='[')) {
      j++;
      i=j;
      t << parse (s, i, "]");
      u << s (j, i);
      if ((i<n) && (s[i]==']')) i++;
      t[0]->label= t[0]->label * "*";
      option= false;
    }
    else if ((arity>0) && (s[j]=='{')) {
      j++;
      i=j;
      if ((N(t)==1) && (cmd == "\\def")) {
	while ((i<n) && (s[i]!='}')) i++;
	t << s (j, i);
      }
      else t << parse (s, i, "}");
      u << s (j, i);
      if ((i<n) && (s[i]=='}')) i++;
      arity--;
    }
    else if (option && (s[j]=='#') && (cmd == "\\def")) {
      while ((j+3 <= n) && is_numeric (s[j+1]) && (s[j+2] == '#')) j+=2;
      if (j+2<=n) {
	t << s (j+1, j+2);
	u << s (j+1, j+2);
	i= j+2;
      }
      t[0]->label= t[0]->label * "*";
      option= false;
    }
    else {
      if (arity>0) {
	i=j;
	tree st= parse_symbol (s, i);
	t << st;
	u << st;
	arity--;
      }
      else break;
    }
  }
  if (arity>0) latex_error (s, i, "too little arguments for " * cmd);

  /******************** new commands and environments ************************/
  if (is_tuple (t, "\\def", 2)) {
    string var= as_string (t[1]);
    command_type  (var)= "user";
    command_arity (var)= 0;
    command_def   (var)= as_string (u[2]);
  }
  if (is_tuple (t, "\\def*", 3)) {
    string var= as_string (t[1]);
    command_type  (var)= "user";
    command_arity (var)= as_int (t[2]);
    command_def   (var)= as_string (u[3]);
  }
  if (is_tuple (t, "\\newenvironment", 3)) {
    string var= "\\begin-" * as_string (t[1]);
    command_type  (var)= "user";
    command_arity (var)= 0;
    command_def   (var)= as_string (u[2]);
    var= "\\end-" * as_string (t[1]);
    command_type  (var)= "user";
    command_arity (var)= 0;
    command_def   (var)= as_string (u[3]);
  }
  if (is_tuple (t, "\\newenvironment*", 4)) {
    string var= "\\begin-" * as_string (t[1]);
    command_type  (var)= "user";
    command_arity (var)= as_int (t[2]);
    command_def   (var)= as_string (u[3]);
    var= "\\end-" * as_string (t[1]);
    command_type  (var)= "user";
    command_arity (var)= 0;
    command_def   (var)= as_string (u[4]);
  }

  /***************** environment changes for user commands  ******************/
  if (latex_type (cmd) == "user") {
    int pos= 0;
    string body= command_def[cmd];
    if (count_occurrences ("\\begin", body) ==
	count_occurrences ("\\end", body))
      (void) parse (sharp_to_arg (body, u), pos, "", true);
    else t= parse (sharp_to_arg (body, u), pos, "", true);
    // replaces macros by their definitions in the case when
    // the user defined shorthands for \\begin{env} and \\end{env}
  }

  if (mbox_flag) command_type ("!mode") = "math";
  return t;
}

tree
latex_parser::parse_unknown (string s, int& i, string cmd) {
  int  n     = N(s);
  bool option= false;

  tree t (TUPLE, copy (cmd));
  while (i<n) {
    int j=i;
    while ((j<n) && is_space (s[j])) j++;
    if (j==n) break;
    if (option && (s[j]=='[')) {
      j++;
      i=j;
      t << parse (s, i, "]");
      if ((i<n) && (s[i]==']')) i++;
      t[0]->label= t[0]->label * "*";
      option= false;
    }
    else if (s[j]=='{') {
      j++;
      i=j;
      t << parse (s, i, "}");
      if ((i<n) && (s[i]=='}')) i++;
    }
    else break;
  }
  return t;
}

/******************************************************************************
* Parsing verbatim text
******************************************************************************/

tree
latex_parser::parse_verbatim (string s, int& i, string end) {
  int start=i, n= N(s), e= N(end);
  while ((i<(n-e)) && (s(i,i+e)!=end)) i++;
  i+=e;
  return tree (CONCAT,
	       tree (TUPLE, "\\begin-verbatim"),
	       s(start,i-e),
	       tree (TUPLE, "\\end-verbatim"));
}

/******************************************************************************
* This routine may be used to transform accented characters to the Cork format
******************************************************************************/

static char Cork_unaccented[128]= {
  'A', ' ', 'C', 'C', 'D', 'E', ' ', 'G',
  'L', 'L', ' ', 'N', 'N', ' ', 'O', 'R',
  'R', 'S', 'S', 'S', 'T', 'T', 'U', 'U',
  'Y', 'Z', 'Z', 'Z', ' ', 'I', 'd', ' ',
  'a', ' ', 'c', 'c', 'd', 'e', ' ', 'g',
  'l', 'l', ' ', 'n', 'n', ' ', 'o', 'r',
  'r', 's', 's', 's', 't', 't', 'u', 'u',
  'y', 'z', 'z', 'z', ' ', ' ', ' ', ' ',
  'A', 'A', 'A', 'A', 'A', 'A', ' ', 'C',
  'E', 'E', 'E', 'E', 'I', 'I', 'I', 'I',
  'D', 'N', 'O', 'O', 'O', 'O', 'O', ' ',
  ' ', 'U', 'U', 'U', 'U', 'Y', ' ', ' ',
  'a', 'a', 'a', 'a', 'a', 'a', ' ', 'c',
  'e', 'e', 'e', 'e', 25 , 25 , 25 , 25 ,
  'd', 'n', 'o', 'o', 'o', 'o', 'o', ' ',
  ' ', 'u', 'u', 'u', 'u', 'y', ' ', ' '
};

static char Cork_accent[128]= {
  'u' , ' ' , '\'', 'v' , 'v' , 'v' , ' ' , 'u' ,
  '\'', ' ' , ' ' , '\'', 'v' , ' ' , 'H' , '\'',
  'v' , '\'', 'v' , 'c' , 'v' , 'c' , 'H' , ' ' ,
  '\"', '\'', 'v' , '.' , ' ' , '.' , '=' , ' ' , // "
  'u' , ' ' , '\'', 'v' , 'v' , 'v' , ' ' , 'u' ,
  '\'', ' ' , ' ' , '\'', 'v' , ' ' , 'H' , '\'',
  'v' , '\'', 'v' , 'c' , 'v' , 'c' , 'H' , ' ' ,
  '\"', '\'', 'v' , '.' , ' ' , ' ' , ' ' , ' ' , // "
  '`' , '\'', '^' , '~' , '\"', ' ' , ' ' , 'c' , // "
  '`' , '\'', '^' , '\"', '`' , '\'', '^' , '\"', // "
  '=' , '~' , '`' , '\'', '^' , '~' , '\"', ' ' , // "
  ' ' , '`' , '\'', '^' , '\"', '\'', ' ' , ' ' , // "
  '`' , '\'', '^' , '~' , '\"', ' ' , ' ' , 'c' , // "
  '`' , '\'', '^' , '\"', '`' , '\'', '^' , '\"', // "
  '=' , '~' , '`' , '\'', '^' , '~' , '\"', ' ' , // "
  ' ' , '`' , '\'', '^' , '\"', '\'', ' ' , ' '   // "
};

tree
accented_to_Cork (tree t) {
  if (arity (t) == 0) return t;
  int i, n=N(t);
  tree r (t, n);
  for (i=0; i<n; i++) r[i]= accented_to_Cork (t[i]);
  if (is_compound (t[0])) return r;

  string s= t[0]->label;
  if ((N(s)==2) && (s[0]=='\\') && (n==2) &&
      is_atomic (r[1]) && (N(r[1]->label)<=2)) {
    string v= r[1]->label;
    if (N(v)==0) {
      if (s[1] == '`' ) {
	string ret_s (1);
	ret_s[0]= '\000';
	return ret_s;
      }
      if (s[1] == '\'') return "\001";
      if (s[1] == '^' ) return "\136";
      if (s[1] == '\"') return "\004"; // "
      if (s[1] == '~' ) return "\176";
      if (s[1] == '=' ) return "\026";
      if (s[1] == '.' ) return "\137";
      if (s[1] == 'u' ) return "\025";
      if (s[1] == 'v' ) return "\024";
      if (s[1] == 'H' ) return "\175";
      if (s[1] == 'c' ) return "\030";
    }
    else {
      char c1= v[0], c2= s[1];
      if (v == "\\i") c1= (char) 25;
      if ((N(v)==1) || (v=="\\i"))
	for (i=0; i<127; i++)
	  if ((Cork_unaccented[i]==c1) && (Cork_accent[i]==c2))
	    return tree (string ((char) (i+128)));
    }
  }
  if (r == tuple ("\\i")) return "\\i";
  return r;
}

/******************************************************************************
* Interface
******************************************************************************/

tree
latex_parser::parse (string s) {
  command_type ->extend ();
  command_arity->extend ();
  command_def  ->extend ();

  // We first cut the string into pieces at strategic places
  // This reduces the risk that the parser gets confused
  array<string> a;
  int i, start=0, n= N(s);
  for (i=0; i<n; i++)
    if (s[i]=='\n') {
      while ((i<n) && is_space (s[i])) i++;
      if (test (s, i, "\\begin{document}") ||
	  test (s, i, "\\begin{abstract}") ||
	  test (s, i, "\\chapter") ||
	  test (s, i, "\\section") ||
	  test (s, i, "\\subsection") ||
	  test (s, i, "\\subsubsection") ||
	  test (s, i, "\\paragraph") ||
	  test (s, i, "\\subparagraph") ||
	  test (s, i, "\\newcommand") ||
	  test (s, i, "\\def"))
	{
	  a << s (start, i);
	  start= i;
	}
      if (i == n) break;
    }
  a << s (start, i);

  // We now parse each of the pieces
  tree t (CONCAT);
  for (i=0; i<N(a); i++) {
    int j=0;
    while (j<N(a[i])) {
      int start= j;
      command_type ("!mode") = "text";
      command_type ("!em") = "false";
      tree u= parse (a[i], j, "", true);
      if ((N(t)>0) && (t[N(t)-1]!='\n') && (start==0)) t << "\n";
      if (is_concat (u)) t << A(u);
      else t << u;
      if (j == start) j++;
    }
  }

  command_type ->shorten ();
  command_arity->shorten ();
  command_def  ->shorten ();
  return t;
}

static bool
japanese_tex (string& s) {
  if (search_forwards (s, "documentclass{jarticle}")) {
    replace (s, "documentclass{jarticle}", "documentclass{article}");
    s= convert (s, "ISO-2022-JP", "UTF-8");
    return true;
  }
  if (search_forwards (s, "documentclass{jbook}")) {
    replace (s, "documentclass{jbook}", "documentclass{book}");
    s= convert (s, "ISO-2022-JP", "UTF-8");
    return true;
  }
  return false;
}

tree
parse_latex (string s) {
  s= dos_to_better (s);
  bool unicode= japanese_tex (s);
  latex_parser ltx (unicode);
  return accented_to_Cork (ltx.parse (s));
}

tree
parse_latex_document (string s) {
  return compound ("!file", parse_latex (s));
}
