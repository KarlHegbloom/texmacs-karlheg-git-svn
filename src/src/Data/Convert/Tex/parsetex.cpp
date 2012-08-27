
/******************************************************************************
* MODULE     : parsetex.cpp
* DESCRIPTION: conversion of tex/latex strings into logical tex/latex trees
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "Tex/convert_tex.hpp"
#include "converter.hpp"

extern bool textm_class_flag;
hashmap<string,int> textm_recursion_level (0);

string string_arg (tree t);
tree latex_symbol_to_tree (string s);

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
  int level;
  bool unicode;
  latex_parser (bool unicode2): level (0), unicode (unicode2) {}
  void latex_error (string s, int i, string message);

  tree parse             (string s, int& i, string stop= "", bool ch= false);
  tree parse_backslash   (string s, int& i);
  tree parse_symbol      (string s, int& i);
  tree parse_command     (string s, int& i, string which);
  tree parse_argument    (string s, int& i);
  tree parse_unknown     (string s, int& i, string which);
  bool can_parse_length  (string s, int i);
  tree parse_length      (string s, int& i);
  tree parse_length      (string s, int& i, int e);
  tree parse_length_name (string s, int& i);
  tree parse_verbatim    (string s, int& i, string end);

  tree parse             (string s, bool change);
};

/******************************************************************************
* Error handling
******************************************************************************/

void
latex_parser::latex_error (string s, int i, string message) {
  if (!textm_class_flag) {
    cerr << "Latex error] " << message << "\n";
    if (i>30) s= "..." * s (i-27, N(s));
    if (N(s)>60) s= s (0, 57) * "...";
    cerr << "Latex error] in " << s << "\n";
  }
}

/******************************************************************************
* Main parsing routine
******************************************************************************/

static bool
is_regular (tree t) {
  if (!is_tuple (t)) return true;
  if (N(t) == 0 || !is_atomic (t[0])) return false;
  string s= t[0]->label;
  return !starts (s, "\\begin-") && !starts (s, "\\end-");
}

static bool
is_tex_alpha (char c) {
  return is_alpha (c) || c == '@';
}

static bool
is_tex_alpha (string s) {
  for (int i=0; i<N(s); i++)
    if (!is_alpha (s[i]) && s[i] != '@') return false;
  return true;
}

tree
latex_parser::parse (string s, int& i, string stop, bool change) {
  bool no_error= true;
  int n= N(s);
  tree t (CONCAT);

  level++;
  command_type ->extend ();
  command_arity->extend ();
  command_def  ->extend ();

  while ((i<n) && is_space (s[i])) i++;
  while ((i<n) && no_error &&
	 (s[i] != '\0' || N (stop) != 0) &&
	 (N(stop) != 1 || s[i] != stop[0]) &&
	 (s[i] != '$' || stop != "$$" || i+1>=n || s[i+1] != '$') &&
	 (stop != "denom" ||
	  (s[i] != '$' && s[i] != '}' &&
	   (i+2>n || s(i,i+2) != "\\]") &&
	   (i+2>n || s(i,i+2) != "\\)") &&
	   (i+4>n || s(i,i+4) != "\\end")))) {
    switch (s[i]) {
    case '~':
      t << tuple ("\\nbsp");
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
    case '%': {
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
      break;
    }
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
      if (s (i+1, i+6) == "hskip" || s (i+1, i+6) == "vskip"){
        string skip = s (i+1, i+6);
        i+=7;
        bool tmp_textm_class_flag = textm_class_flag;
        textm_class_flag = true;
        if (can_parse_length (s, i)) {
          if (skip == "hskip")
            t << tuple ("\\hspace", parse_length (s, i));
          else
            t << tuple ("\\vspace", parse_length (s, i));
        }
        textm_class_flag = tmp_textm_class_flag;
      }
      else if (((i+7)<n && !is_tex_alpha (s (i+5, i+7)) &&
	  (s (i, i+5) == "\\over" || s (i, i+5) == "\\atop")) ||
	  ((i+9)<n && !is_tex_alpha (s (i+7, i+9)) && s (i, i+7) == "\\choose"))
	{
    int start = i;
	  i++;
    while (i<n && is_alpha (s[i])) i++;
	  string fr_cmd= s(start, i);
	  if (fr_cmd == "\\over") fr_cmd= "\\frac";
	  if (fr_cmd == "\\atop") fr_cmd= "\\ontop";
	  int j;
	  for (j=N(t); j>0 && is_regular (t[j-1]); j--) {}
	  tree num= t (j, N(t));
	  if (N(num) == 0) num= "";
	  t= t (0, j);
	  while (i<n && (s[i] == ' ' || s[i] == '\n' || s[i] == '\t')) i++;
	  tree den= parse (s, i, "denom");
	  t << tree (TUPLE, fr_cmd, num, den);
	}
      else if ((i+5) < n && s(i,i+3) == "\\sp" && !is_tex_alpha (s[i+3])) {
	i+=3;
	t << parse_command (s, i, "\\<sup>");
      }
      else if ((i+5) < n && s(i,i+3) == "\\sb" && !is_tex_alpha (s[i+3])) {
	i+=3;
	t << parse_command (s, i, "\\<sub>");
      }
      else if ((i+10) < n && s(i,i+8) == "\\pmatrix") {
	i+=8;
	tree arg= parse_command (s, i, "\\pmatrix");
	if (is_tuple (arg, "\\pmatrix", 1)) arg= arg[1];
	t << tree (TUPLE, "\\begin-pmatrix");
	if (is_concat (arg)) t << A (arg);
	else t << arg;
	t << tree (TUPLE, "\\end-pmatrix");
      }
      else if (can_parse_length (s, i))
	t << parse_length (s, i);
      else {
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
      if (ln >= 2) t << "\n";
      else if (i<n) t << " ";
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
      if ((s[i] == '-' || (s[i] >= '0' && s[i] <= '9')) &&
	  can_parse_length (s, i))
	t << parse_length (s, i);
      else if (unicode && ((unsigned char) s[i]) >= 128) {
	unsigned int code= decode_from_utf8 (s, i);
        string c = utf8_to_cork(encode_as_utf8(code));
        if (c(0,1) == "<#")
	  t << tree (TUPLE, "\\" * c(1, N(c)-1));
        else
          t << c;
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

  level--;
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
  if (((i+5)<n) && (s(i,i+4)=="\\url") && !is_tex_alpha (s[i+5])) {
    i+=4;
    while (i<n && (s[i] == ' ' || s[i] == '\n' || s[i] == '\t')) i++;
    string ss;
    if (i<n && s[i] == '{') {
      i++;
      int start= i;
      while ((i<n) && s[i] != '}') i++;
      ss= s (start, i++);
    }
    return tree (TUPLE, "\\url", ss);
  }
  if (((i+6)<n) && (s(i,i+5)=="\\href")) {
    i+=5;
    while (i<n && (s[i] == ' ' || s[i] == '\n' || s[i] == '\t')) i++;
    string ss;
    if (i<n && s[i] == '{') {
      i++;
      int start= i;
      while ((i<n) && s[i] != '}') i++;
      ss= s (start, i++);
    }
    tree u= "";
    while (i<n && (s[i] == ' ' || s[i] == '\n' || s[i] == '\t')) i++;
    if (i<n && s[i] == '{') { i++; u= parse (s, i, "}"); i++; }
    return tree (TUPLE, "\\href", ss, u);
  }

  /************************ special commands *********************************/
  i++;
  if (i==n) return "";
  if (s[i]==' ') {
    i++;
    return tree (TUPLE, "\\ ");
  }
  if (!is_tex_alpha(s[i])) {
    i++;
    if (s[i-1]=='(') return parse_command (s, i, "\\begin-math");
    if (s[i-1]==')') return parse_command (s, i, "\\end-math");
    if (s[i-1]=='[') return parse_command (s, i, "\\begin-displaymath");
    if (s[i-1]==']') return parse_command (s, i, "\\end-displaymath");
    return parse_command (s, i, s (i-2, i));
  }

  /************************* normal commands *********************************/
  int start= i-1;
  while ((i<n) && is_tex_alpha (s[i])) i++;
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
      if (N(args)>nr) r << string_arg (args[nr]);
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
  if (!is_tex_alpha (s[i])) { i++; return s(start, i); }
  while ((i<N(s)) && is_tex_alpha (s[i])) i++;
  if ((i<N(s)) && (s[i]=='*')) i++;
  return s(start,i);
}

static bool
is_math_environment (tree t) {
  //cout << "t= " << t << "\n";
  tree b= t[N(t)-2];
  tree e= t[N(t)-1];
  if (!is_concat (b)) b= tree (CONCAT, b);
  if (!is_concat (e)) e= tree (CONCAT, e);
  int i, j;
  for (i=N(b)-1; i>=0; i--)
    if (is_tuple (b[i]) && N(b[i])>0 && is_atomic (b[i][0]))
      if (latex_type (b[i][0]->label) == "math-environment")
	break;
  for (j=0; j<N(e); j++)
    if (is_tuple (e[j]) && N(e[j])>0 && is_atomic (e[j][0]))
      if (latex_type (e[j][0]->label) == "math-environment")
	break;
  if (i >= 0 && j < N(e)) {
    string bs= b[i][0]->label;
    string es= e[j][0]->label;
    bool ok=
      starts (bs, "\\begin-") &&
      starts (es, "\\end-") &&
      bs (7, N(bs)) == es (5, N(es));
    //cout << t[1] << " -> " << ok << "\n";
    return ok;
  }
  return false;
}

static bool
is_text_argument (string cmd, int remaining_arity) {
  // FIXME: this test should be improved using DRD properties
  (void) remaining_arity;
  return cmd == "\\label" || cmd == "\\ref";
}

void
skip_linespaces (string s, int& i) {
  int n=N(s);
  skip_spaces (s, i);
  if ((i<n) && (s[i]=='\n')) i++;
  skip_spaces (s, i);
}

bool
latex_parser::is_opening_option (char c) {
  if (c == '[') return true;
  if (loaded_package["algorithm2e"] && c == '(') return true;
  return false;
}

tree
latex_parser::parse_command (string s, int& i, string cmd) {
  bool delimdef = false;
  // cout << cmd << " [" << latex_type (cmd) << ", "
  // << command_type ["!mode"] << ", " << latex_arity (cmd) << "]" << LF;
  if (cmd == "\\gdef" || cmd == "\\xdef" || cmd == "\\edef") cmd= "\\def";
  if (cmd == "\\def" && s[i] == '\\') delimdef = true;
  if (cmd == "\\newcommand") cmd= "\\def";
  if (cmd == "\\providecommand") cmd= "\\def";
  if (cmd == "\\renewcommand") cmd= "\\def";
  if (cmd == "\\DeclareMathOperator") cmd= "\\def";
  if (cmd == "\\DeclareMathOperator*") cmd= "\\def";
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

  if (textm_class_flag && level <= 1 && latex_type (cmd) == "length") {
    //cout << "Parse length " << cmd << "\n";
    int n= N(s);
    while (i<n && (is_space (s[i]) || s[i] == '=')) i++;
    tree len= parse_length (s, i);
    //cout << "Got " << len << "\n";
    return tree (TUPLE, "\\setlength", copy (cmd), copy (len));
  }

  if (cmd == "\\setlength") {
    tree name= parse_length_name (s, i);
    tree arg = parse_argument (s, i);
    return tuple (cmd, name, arg);
  }

  if (cmd == "\\category") {
    tree a= parse_argument (s, i);
    skip_linespaces (s, ++i);
    tree b= parse_argument (s, i);
    skip_linespaces (s, ++i);
    tree c= parse_argument (s, i);
    skip_linespaces (s, ++i);
    if (s[i] == '[') {
      i++;
      tree d= parse (s, i, ']'); i++;
      return tree (TUPLE, cmd * "*", a, b, c, d);
    }
    return tuple (cmd, a, b, c);
  }

  bool mbox_flag=
    ((cmd == "\\text") || (cmd == "\\mbox")) &&
    (command_type ["!mode"] == "math");
  if (mbox_flag) command_type ("!mode") = "text";

  int  n     = N(s);
  string type= latex_type (cmd);
  int  arity = latex_arity (cmd);
  bool option= (arity<0);
  if (option) arity= -1-arity;

  tree t, u;

  // parsing exception for delimited parameters \\def
  if (delimdef) {
    int start = i;
    string name, args= "";
    i++;
    if (is_alpha (s[i])) {
      while (i < N(s) && ((!textm_class_flag && is_alpha (s[i])) ||
            (textm_class_flag && is_tex_alpha (s[i]))))
        i++;
    }
    else
      start = i++;
    name = s(start, i);
    while (i < N(s) && s[i] != '{') {
      if (i < N(s) && s[i] == '#') {
        while (i < N(s) && s[i] == '#') i++;
        if (i < N(s) && is_digit (s[i])) args = s[i];
      }
      else
        i++;
    }
    i++;
    tree st= parse (s, i, "}");
    i++;
    if (args == "")
      t = tree (TUPLE, "\\def", name, st);
    else
      t = tree (TUPLE, "\\def*", name, args, st);
    u = t;
  }
  else {
/************************ retrieve arguments *******************************/
    t = tree(TUPLE, copy (cmd)); // parsed arguments
    u = tree(TUPLE, copy (cmd)); // unparsed arguments
    // Should be in a drd.
    bool option2= (cmd == "\\def" || cmd == "\\newenvironment");

    while (i<n && arity>=0 && (arity>0 || option)) {
      int j= i;
      while ((j<n) && is_space (s[j])) j++;
      if (j==n) break;
      if (s[i]=='$') break; // in most cases, this should not be an argument
      if (option && (is_opening_option (s[j]) ||
                    (type == "algorithm2e" && s[j] == '{'))) {
        char ec= closing_delimiter (s[j]);
        j++;
        i=j;
        tree opt= parse (s, i, ec);
        if (cmd != "\\newtheorem" && cmd != "\\newtheorem*")
          t << opt;
        u << s (j, i);
        if ((i<n) && (s[i]==ec)) i++;
        if (cmd != "\\newtheorem" && cmd != "\\newtheorem*")
          t[0]->label= t[0]->label * "*";
        option= false;
        if (!option && option2) {
          option = true;
          option2= false;
          continue;
        }
      }
      else if ((arity>0) && (s[j]=='{')) {
        bool text_arg=
          (command_type["!mode"] == "math") && is_text_argument (cmd, arity);
        j++;
        i=j;
        if (text_arg) command_type ("!mode")= "text";
        if ((N(t)==1) && (cmd == "\\def")) {
          while ((i<n) && (s[i]!='}')) i++;
          t << s (j, i);
        }
        else t << parse (s, i, "}");
        if (text_arg) command_type ("!mode")= "math";
        u << s (j, i);
        if ((i<n) && (s[i]=='}')) i++;
        arity--;
        if (arity == 0) option= option2= false;
      }
      else if (s[j] == '}') break;
      else if (option && (s[j]=='#') && (cmd == "\\def")) {
        while ((j+3 <= n) && is_numeric (s[j+1]) && (s[j+2] == '#')) j+=2;
        if (j+2<=n) {
          t << s (j+1, j+2);
          u << s (j+1, j+2);
          i= j+2;
        }
        t[0]->label= t[0]->label * "*";
        option= option2= false;
      }
      else if (s[i] == '%') {
        while(i < N(s) && s[i] != '\n') i++;
      }
      else {
        if (arity>0) {
          i=j;
          tree st= parse_symbol (s, i);
          t << st;
          u << st;
          arity--;
          if (arity == 0) option= option2= false;
        }
        else break;
      }
    }
    if (arity>0) latex_error (s, i, "too little arguments for " * cmd);
  }

  /******************** new commands and environments ************************/
  if (is_tuple (t, "\\def", 2)) {
    string var= string_arg (t[1]);
    command_type  (var)= "user";
    command_arity (var)= 0;
    command_def   (var)= as_string (u[2]);
    if (is_func (t[2], TUPLE, 1) && 
        latex_type (as_string (t[2][0])) != "undefined") {
      command_arity (var)= latex_arity (as_string (t[2][0]));
    }
  }
  if (is_tuple (t, "\\def*", 3)) {
    string var= string_arg (t[1]);
    command_type  (var)= "user";
    command_arity (var)= as_int (t[2]);
    command_def   (var)= as_string (u[3]);
  }
  if (is_tuple (t, "\\def**", 4)) {
    string var= string_arg (t[1]);
    command_type  (var)= "user";
    command_arity (var)= - as_int (t[2]);
    command_def   (var)= as_string (u[4]);
  }
  if (is_tuple (t, "\\declaretheorem*", 2) || 
      is_tuple (t, "\\declaretheorem",  1)) {
    string var= "\\begin-" * string_arg (t[N(t)-1]);
    command_type  (var)= "environment";
    command_arity (var)= 0;
    var= "\\end-" * string_arg (t[N(t)-1]);
    command_type  (var)= "environment";
    command_arity (var)= 0;
  }
  if (is_tuple (t, "\\newtheorem", 2) || is_tuple (t, "\\newtheorem*", 2)) {
    string var= "\\begin-" * string_arg (t[1]);
    command_type  (var)= "environment";
    command_arity (var)= 0;
    var= "\\end-" * string_arg (t[1]);
    command_type  (var)= "environment";
    command_arity (var)= 0;
  }
  if (is_tuple (t, "\\newdimen", 1) || is_tuple (t, "\\newlength", 1)
      || is_tuple (t, "\\newskip", 1)) {
    string var= string_arg (t[1]);
    command_type  (var)= "length";
    command_arity (var)= 0;
  }
  if (is_tuple (t, "\\newenvironment", 3)) {
    string var= "\\begin-" * string_arg (t[1]);
    command_type  (var)= "user";
    command_arity (var)= 0;
    command_def   (var)= as_string (u[2]);
    if (is_math_environment (t)) command_type (var)= "math-environment";
    var= "\\end-" * string_arg (t[1]);
    command_type  (var)= "user";
    command_arity (var)= 0;
    command_def   (var)= as_string (u[3]);
    if (is_math_environment (t)) command_type (var)= "math-environment";
  }
  if (is_tuple (t, "\\newenvironment*", 4)) {
    string var= "\\begin-" * string_arg (t[1]);
    command_type  (var)= "user";
    command_arity (var)= as_int (t[2]);
    command_def   (var)= as_string (u[3]);
    if (is_math_environment (t)) command_type (var)= "math-environment";
    var= "\\end-" * string_arg (t[1]);
    command_type  (var)= "user";
    command_arity (var)= 0;
    command_def   (var)= as_string (u[4]);
    if (is_math_environment (t)) command_type (var)= "math-environment";
  }
  if (is_tuple (t, "\\newenvironment**", 5)) {
    string var= "\\begin-" * string_arg (t[1]);
    command_type  (var)= "user";
    command_arity (var)= -as_int (t[2]);
    command_def   (var)= as_string (u[4]);
    if (is_math_environment (t)) command_type (var)= "math-environment";
    var= "\\end-" * string_arg (t[1]);
    command_type  (var)= "user";
    command_arity (var)= 0;
    command_def   (var)= as_string (u[5]);
    if (is_math_environment (t)) command_type (var)= "math-environment";
  }

  /***************** environment changes for user commands  ******************/
  if ((is_tuple (t, "\\def", 2) || is_tuple (t, "\\def*", 3))
      && latex_type (cmd) == "user") {
    int pos= 0;
    string body= command_def[cmd];
    textm_recursion_level (cmd)++;
    if (textm_recursion_level [cmd] <= 5) {
      if (count_occurrences ("\\begin", body) ==
	  count_occurrences ("\\end", body))
	(void) parse (sharp_to_arg (body, u), pos, "", true);
      else t= parse (sharp_to_arg (body, u), pos, "", true);
    }
    textm_recursion_level (cmd)--;
    // replaces macros by their definitions in the case when
    // the user defined shorthands for \\begin{env} and \\end{env}
  }

  if (mbox_flag) command_type ("!mode") = "math";
  return t;
}

tree
latex_parser::parse_argument (string s, int& i) {
  skip_spaces (s, i);
  if (s[i] == '{') {
    i++;
    return parse (s, i, "}");
  }
  else return parse_symbol (s, i);
}

tree
latex_parser::parse_unknown (string s, int& i, string cmd) {
  int  n     = N(s);
  bool option= true;

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
* Parsing lengths
******************************************************************************/

bool
latex_parser::can_parse_length (string s, int i) {
  if (!textm_class_flag) return false;
  int initial= i;
  int stage= 0;
  int n= N(s);
  while (i<n) {
    if (is_numeric (s[i]) || s[i] == '.' || s[i] == '-') { stage= 1; i++; }
    else if (is_space (s[i]) && stage > 0) i++;
    else if (read (s, i, "plus") || read (s, i, "\\@plus") ||
	     read (s, i, "minus") || read (s, i, "\\@minus"))
      return stage >= 2;
    else if (is_tex_alpha (s[i])) {
      if      (read (s, i, "cm")) stage= 2;
      else if (read (s, i, "mm")) stage= 2;
      else if (read (s, i, "pt")) stage= 2;
      else if (read (s, i, "in")) stage= 2;
      else if (read (s, i, "em")) stage= 2;
      else if (read (s, i, "ex")) stage= 2;
      else if (read (s, i, "pc")) stage= 2;
      else if (read (s, i, "bp")) stage= 2;
      else if (read (s, i, "dd")) stage= 2;
      else if (read (s, i, "cc")) stage= 2;
      else if (read (s, i, "sp")) stage= 2;
      else return false;
      return stage >= 2;
      if (i<n && is_tex_alpha (s[i])) return false;
    }
    else if (s[i] == '\\') {
      i++;
      int start= i;
      while (i<n && is_tex_alpha (s[i])) i++;
      if (latex_type (s (start, i)) != "length") return false;
      return s[initial] != '\\' || level > 1;
    }
    else return false;
  }
  return false;
}

tree
latex_parser::parse_length (string s, int& i) {
  return parse_length (s, i, 0);
}

tree
latex_parser::parse_length (string s, int& i, int e) {
  int n= N(s);
  tree r= tree (CONCAT);
  while (i<n) {
    if (is_numeric (s[i]) || s[i] == '.' || s[i] == '-')
      r << s (i, i+1);
    else if (read (s, i, "plus") || read (s, i, "\\@plus")) {
      tree next= parse_length (s, i);
      if (is_tuple (next, "\\tex-len", 3)) {
	//ASSERT (next[2] == "0pt", "invalid multiple plus");
	return tuple ("\\tex-len", r, next[1], next[3]);
      }
      else return tuple ("\\tex-len", r, next, "0pt");
    }
    else if (read (s, i, "minus") || read (s, i, "\\@minus")) {
      tree next= parse_length (s, i);
      if (is_tuple (next, "\\tex-len", 3)) {
	//ASSERT (next[3] == "0pt", "invalid multiple minus");
	return tuple ("\\tex-len", r, next[2], next[1]);
      }
      else return tuple ("\\tex-len", r, "0pt", next);
    }
    else if (is_tex_alpha (s[i]) && N(r) > 0 && is_atomic (r[N(r)-1]) &&
	     (is_numeric (r[N(r)-1]->label) ||
	      r[N(r)-1] == "." || r[N(r)-1] == "-")) {
      for (;i<n && is_tex_alpha (s[i]); i++) {
	r << s (i, i+1);
        e += 1;
      }
      continue;
    }
    else if (s[i] == '\\') {
      int start= i++;
      while (i<n && is_tex_alpha (s[i])) i++;
      if (latex_type (s (start+1, i)) != "length" || e > 0) { 
        i= start; 
        break;
      }
      r << as_string (latex_symbol_to_tree (s (start, i)));
      e += 1;
      continue;
    }
    else if (is_space (s[i]));
    else break;
    i++;
  }
  return r;
}

tree
latex_parser::parse_length_name (string s, int& i) {
  skip_spaces (s, i);
  if (s[i] == '{') {
    i++;
    tree r= parse_length_name (s, i);
    skip_spaces (s, i);
    if (s[i] == '}') i++;
    return r;
  }
  else if (s[i] == '\\') {
    int start= i;
    i++;
    while (i<N(s) && is_tex_alpha (s[i])) i++;
    return s (start, i);
  }
  else return "";
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
  'A', 'A', 'C', 'C', 'D', 'E', 'E', 'G',
  'L', 'L', ' ', 'N', 'N', ' ', 'O', 'R',
  'R', 'S', 'S', 'S', 'T', 'T', 'U', 'U',
  'Y', 'Z', 'Z', 'Z', ' ', 'I', 'd', ' ',
  'a', 'a', 'c', 'c', 'd', 'e', 'e', 'g',
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
  'u' , 'k' , '\'', 'v' , 'v' , 'v' , 'k' , 'u' ,
  '\'', 'v' , ' ' , '\'', 'v' , ' ' , 'H' , '\'',
  'v' , '\'', 'v' , 'c' , 'v' , 'c' , 'H' , 'r' ,
  '\"', '\'', 'v' , '.' , ' ' , '.' , '=' , ' ' , // "
  'u' , 'k' , '\'', 'v' , 'v' , 'v' , 'k' , 'u' ,
  '\'', 'v' , ' ' , '\'', 'v' , ' ' , 'H' , '\'',
  'v' , '\'', 'v' , 'c' , 'v' , 'c' , 'H' , 'r' ,
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
  return r;
}

/******************************************************************************
* Interface
******************************************************************************/

tree
latex_parser::parse (string s, bool change) {
  command_type ->extend ();
  command_arity->extend ();
  command_def  ->extend ();

  // We first cut the string into pieces at strategic places
  // This reduces the risk that the parser gets confused
  array<string> a;
  int i, start=0, n= N(s);
  for (i=0; i<n; i++)
    if (s[i]=='\n' || (s[i] == '\\' && test (s, i, "\\nextbib"))) {
      while ((i<n) && is_space (s[i])) i++;
      if (test (s, i, "%%%%%%%%%% Start TeXmacs macros\n")) {
	a << s (start, i);
	while ((i<n) && (!test (s, i, "%%%%%%%%%% End TeXmacs macros\n")))
	  i++;
	i += 30;
	start= i;
	continue;
      }
      if (test (s, i, "\\begin{document}") ||
	  test (s, i, "\\begin{abstract}") ||
	  test (s, i, "\\chapter") ||
	  test (s, i, "\\section") ||
	  test (s, i, "\\subsection") ||
	  test (s, i, "\\subsubsection") ||
	  test (s, i, "\\paragraph") ||
	  test (s, i, "\\subparagraph") ||
	  test (s, i, "\\nextbib") ||
	  test (s, i, "\\newcommand") ||
	  test (s, i, "\\def") ||
	  test (s, i, "\\input{") ||
	  test (s, i, "\\include{"))
	{
	  a << s (start, i);
	  start= i;
          if (test (s, i, "\\input{") || test (s, i, "\\include{")) {
	    while (i<N(s) && s[i] != '{') i++;
	    int start_name= i+1;
            while (i<N(s) && s[i] != '}') i++;
            string name= s (start_name, i);
            if (!ends (name, ".tex")) name= name * ".tex";
            url incl= relative (get_file_focus (), name);
            string body;
            if (!exists (incl) || load_string (incl, body, false)) i++;
            else {
              //cout << "Include " << name << " -> " << incl << "\n";
              s= s (0, start) * "\n" * body * "\n" * s (i+1, N(s));
              n= N(s);
              i= start + 1;
            }
            start= i;
          }
          while (i < n && test (s, i, "\\nextbib{}")) {
            i += 10;
            a << s (start, i);
            start= i;
          }
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
  //cout << "Parsed " << t << "\n";
  return t;
}

/******************************************************************************
* Internationalization
******************************************************************************/

string
clean_latex_comments (string s) {
  string r = "";
  int start = 0, stop = 0;
  while (stop < N(s)) {
    //cout << start << " : " << stop << "\n";
    stop = search_forwards ("%", stop, s);
    //cout << s[stop -1] << s[stop ] << s[stop +1];
    if (stop == -1) {
      r << s (start, N(s));
      return r;
    }
    else if (stop == 0 || s[stop -1] != '\\') {
      r << s (start, stop) << "\n";
      stop = search_forwards ("\n", stop, s) + 1;
      start = stop;
      continue;
    }
    else
      stop++;
  }
  if (start < N(s)) r << s (start, N(s));
  return r;
}

int
get_latex_package_idx (string s, string which) {
  int i = 0;
  while (search_forwards ("\\usepackage", i, s) != -1) {
    int state = 0;
    i = search_forwards ("\\usepackage", i, s) + 1;
    for (int j = i ; j < N(s) ; j++) {
      if      (test (s, j, "\n")  || test (s, j, "\\")) break;
      else if (test (s, j, "{")   && state == 0) state = 1;
      else if (test (s, j, "}")   && state == 1) break;
      else if (test (s, j, which) && state == 1)
        return search_backwards ("\\usepackage", j, s);
    }
  }
  return -1;
}

string
get_latex_language (string s) {
  s = clean_latex_comments (s);
  int start, stop;
  stop = get_latex_package_idx (s, "babel");
  if (stop == -1) return "";
  stop = search_forwards  ("babel", stop, s);
  stop = search_backwards ("]", stop, s) - 1;
  while (!is_alpha(s[stop])) stop--;
  for (start = stop ; stop > 0 ; start--)
    if (test(s, start, ' ') || test(s, start, '[') || test(s, start, ','))
      break;
  string r = s(start+1, stop+1);

  tree langs = concat(); 
  langs << "british" << "bulgarian" << "chinese" << "czech" << "danish"
    << "dutch" << "finnish" << "french" << "german" << "hungarian" << "italian"
    << "japanese" << "korean" << "polish" << "portuguese" << "romanian"
    << "russian" << "slovene" << "spanish" << "swedish" << "taiwanese"
    << "ukrainian"; 

  for (int i = 0 ; i < N(langs) ; i++)
    if (test(r, 0 , as_string(langs[i])))
      return as_string(langs[i]);
  if (r == "ngermanb") return "german";
  if (r == "magyar") return "hungarian";
  return "";
}

string
get_latex_encoding (string s) {
  s = clean_latex_comments (s);
  int start, stop;
  
  // Try if inputenc is called
  stop = get_latex_package_idx (s, "inputenc");
  if (stop != -1) {
    stop = search_forwards  ("inputenc", stop, s);
    stop = search_backwards ("]", stop, s);
    start = search_backwards ("[", stop, s) + 1;
    s = s(start, stop);
    s = replace(s, " ", "");
    s = replace(s, "\t", "");
    s = replace(s, ",", "");
    return s;
  }

  // Try if CJK is called
  stop = get_latex_package_idx (s, "CJK");
  if (stop != -1) {
    tree encs = concat();
    encs << "Bg5" <<  "GB" << "GBK" << "JIS" << "SJIS" << "KS" << "UTF8"
      << "EUC-TW" << "EUC-JP" << "EUC-KR";

    for (int i = 0 ; i < N(encs) ; i++)
      if (occurs ("\\begin{CJK}{"*as_string(encs[i]), s))
        return as_string(encs[i]);
  }

  // Try other tricky tests
  if (occurs ("\\documentclass[CJK]{cctart}", s) ||
      occurs ("\\documentclass{cctart}", s) || occurs ("\\kaishu", s))
    return "GB";

  if (occurs ("\\usepackage{hangul}", s) || occurs ("\\usepackage{hfont}", s) ||
      occurs ("]{hangul}", s) || occurs ("]{hfont}", s))
    return "KS";

  if (occurs ("\\usepackage{dhucs}", s) || occurs ("]{dhucs}", s) ||
      occurs ("\\usepackage{memhangul-ucs}", s) ||
      occurs ("]{memhangul-ucs}", s))
    return "UTF8";

  if (occurs ("\\documentclass{jarticle}", s) || occurs ("]{jarticle}", s) ||
      occurs ("\\documentclass{jbook}", s) || occurs ("]{jbook}", s) ||
      occurs ("\\documentclass{jreport}", s) || occurs ("]{jreport}", s))
    return "JIS";

  // Maybe a Cork/catcode generated by TeXmacs
  if (occurs ("TeXmacs macros", s) && occurs ("\\catcode", s))
    return "Cork";

  return "";
}

string
latex_encoding_to_iconv (string s) {
  // Encodings used for LaTeX import.
  // Thanks LyX developpers. Adapted from lyx-devel/lib/encodings

  if (s == "Cork")     return "Cork";
  if (s == "utf8")     return "UTF-8";
  if (s == "utf8x")    return "UTF-8";
  if (s == "UTF8")     return "UTF-8";
  // This encoding is used to typeset Armenian using the armTeX package
  if (s == "armscii8") return "ARMSCII-8";
  if (s == "latin1")   return "ISO-8859-1";
  if (s == "latin2")   return "ISO-8859-2";
  if (s == "latin3")   return "ISO-8859-3";
  if (s == "latin4")   return "ISO-8859-4";
  if (s == "latin5")   return "ISO-8859-9";
  if (s == "latin9")   return "ISO-8859-15";
  if (s == "latin10")  return "ISO-8859-16";
  if (s == "iso88595") return "ISO-8859-5";
  // Not standard, see http://tug.ctan.org/tex-archive/language/arabic/arabi/arabi/texmf/latex/arabi/
  if (s == "8859-6")     return "ISO-8859-6";
  if (s == "iso-8859-7") return "ISO-8859-7";
  if (s == "8859-8")     return "ISO-8859-8";
  // Not standard, see http://www.vtex.lt/tex/littex/index.html
  if (s == "l7xenc")   return "ISO-8859-13";
  if (s == "applemac") return "Macintosh";
  if (s == "cp437")    return "CP437";
  // cp437, but on position 225 is sz instead of beta
  if (s == "cp437de") return "CP437";
  if (s == "cp850")   return "CP850";
  if (s == "cp852")   return "CP852";
  if (s == "cp855")   return "CP855";
  if (s == "cp858")   return "CP858";
  if (s == "cp862")   return "CP862";
  if (s == "cp865")   return "CP865";
  if (s == "cp866")   return "CP866";
  if (s == "cp1250")  return "CP1250";
  if (s == "cp1251")  return "CP1251";
  if (s == "ansinew") return "CP1252";
  if (s == "cp1252")  return "CP1252";
  if (s == "cp1255")  return "CP1255";
  // Not standard, see http://tug.ctan.org/tex-archive/language/arabic/arabi/arabi/texmf/latex/arabi/
  if (s == "cp1256")  return "CP1256";
  if (s == "cp1257")  return "CP1257";
  if (s == "koi8-r")  return "KOI8-R";
  if (s == "koi8-u")  return "KOI8-U";
  if (s == "pt154")   return "PT154";
  if (s == "pt254")   return "PT254";
  // For simplified chinese
  if (s == "GB")      return "EUC-CN";
  if (s == "GBK")     return "GBK";
  // For japanese
  if (s == "JIS")     return "ISO-2022-JP";
  // For korean
  if (s == "KS")      return "EUC-KR";
  // For traditional chinese
  if (s == "EUC-TW")  return "EUC-TW";
  // For japanese
  if (s == "EUC-JP")  return "EUC-JP";

  return "";
}

tree
parse_latex (string s, bool change) {
  s= dos_to_better (s);
  string lan= get_latex_language (s);
  string encoding= latex_encoding_to_iconv (get_latex_encoding (s));
  if (encoding != "UTF-8" && encoding != "Cork" && encoding != "")
    s= convert (s, encoding, "UTF-8");
  else if (encoding == "")
    s= convert (s, "ISO-8859-1", "UTF-8");

  latex_parser ltx (encoding  != "Cork");
  tree r= ltx.parse (s, change);
  r= accented_to_Cork (ltx.parse (s, change));
  if (lan == "") return r;
  return compound ("!language", r, lan);
}

tree
parse_latex_document (string s, bool change) {
  return compound ("!file", parse_latex (s, change));
}
