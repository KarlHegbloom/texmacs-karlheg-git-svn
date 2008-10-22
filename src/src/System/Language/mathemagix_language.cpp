
/******************************************************************************
* MODULE     : mathemagix_language.cpp
* DESCRIPTION: the "mathemagix" language
* COPYRIGHT  : (C) 2008  Francis Jamet
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "analyze.hpp"
#include "impl_language.hpp"
#include "Scheme/object.hpp"

static void parse_number (string s, int& pos);
static void parse_string (string s, int& pos);
static void parse_alpha (string s, int& pos);

mathemagix_language_rep::mathemagix_language_rep (string name):
  language_rep (name), colored ("")
{ 
  eval ("(use-modules (utils misc tm-keywords))");
  list<string> l= as_list_string (eval ("(map symbol->string highlight-any)"));
  while (!is_nil (l)) {
    colored (l->item)= "blue";
    l= l->next;
  }
}

text_property
mathemagix_language_rep::advance (string s, int& pos) {
  if (pos==N(s)) return &tp_normal_rep;
  char c= s[pos];
  if (c == ' ') {
    pos++; return &tp_space_rep; }
  if (c >= '0' && c <= '9') {
    parse_number (s, pos); return &tp_normal_rep; }
  if ((c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') ||
      (c == '_') || (c == '$')) {
    parse_alpha (s, pos); return &tp_normal_rep; }
  tm_char_forwards (s, pos);
  return &tp_normal_rep;
}

array<int>
mathemagix_language_rep::get_hyphens (string s) {
  int i;
  array<int> penalty (N(s)+1);
  penalty[0]= HYPH_INVALID;
  for (i=1; i<N(s); i++)
    if (s[i-1] == '-' && is_alpha (s[i]))
      penalty[i]= HYPH_STD;
    else penalty[i]= HYPH_INVALID;
  penalty[i]= HYPH_INVALID;
  return penalty;
}

void
mathemagix_language_rep::hyphenate (
  string s, int after, string& left, string& right)
{ 
  left = s(0, after);
  right= s(after, N(s));
}

static void
mathemagix_color_setup_constants (hashmap<string, string> & t) {
  string c= "#2060c0";
  t ("cpp_flags")= c;
  t ("cpp_libs")= c;
  t ("cpp_preamble")= c;
  t ("true")= c;
  t ("false")= c;
  t ("mmout")= c;
  t ("mmin")= c;
  t ("mmr")= c;
  t ("nil")= c;
}

static void
mathemagix_color_setup_keywords (hashmap<string, string> & t)  {
  string c= "#8020c0";
  t ("abstract")= c;
  t ("alias")= c;
  t ("and")= c;
  t ("assume")= c;
  t ("begin")= c;
  t ("break")= c;
  t ("case")= c;
  t ("cast")= c;
  t ("catch")= c;
  t ("category")= c;
  t ("class")= c;
  t ("concrete")= c;
  t ("constant")= c;
  t ("constructor")= c;
  t ("continue")= c;
  t ("convert")= c;
  t ("destructor")= c;
  t ("direct")= c;
  t ("div")= c;
  t ("do")= c;
  t ("downto")= c;
  t ("downgrade")= c;
  t ("else")= c;
  t ("evolutive")= c;
  t ("exists")= c;
  t ("explode")= c;
  t ("export")= c;
  t ("extend")= c;
  t ("extern")= c;
  t ("for")= c;
  t ("forall")= c;
  t ("foreach")= c;
  t ("foreign")= c;
  t ("from")= c;
  t ("fuse")= c;
  t ("generate")= c;
  t ("has")= c;
  t ("hidden")= c;
  t ("holds")= c;
  t ("if")= c;
  t ("import")= c;
  t ("in")= c;
  t ("include")= c;
  t ("indirect")= c;
  t ("infix")= c;
  t ("inherit")= c;
  t ("inline")= c;
  t ("interactive")= c;
  t ("intern")= c;
  t ("join")= c;
  t ("keyword")= c;
  t ("literal")= c;
  t ("lambda")= c;
  t ("literal_integer")= c;
  t ("literal_floating")= c;
  t ("literal_string")= c;
  t ("locked")= c;
  t ("loop")= c;
  t ("macro")= c;
  t ("map")= c;
  t ("melt")= c;
  t ("method")= c;
  t ("mod")= c;
  t ("module")= c;
  t ("mutable")= c;
  t ("operator")= c;
  t ("or")= c;
  t ("packed")= c;
  t ("postfix")= c;
  t ("prefix")= c;
  t ("private")= c;
  t ("protected")= c;
  t ("public")= c;
  t ("outline")= c;
  t ("quit")= c;
  t ("raise")= c;
  t ("require")= c;
  t ("return")= c ;
  t ("split")= c;
  t ("step")= c;
  t ("supports?")= c;
  t ("then")= c;
  t ("this")= c;
  t ("to")= c;
  t ("try")= c;
  t ("type")= c;
  t ("unpacked")= c;
  t ("until")= c;
  t ("upgrade")= c;
  t ("use")= c;
  t ("value")= c;
  t ("while")= c;
  t ("with")= c;
  t ("xor")= c;
}

static void
mathemagix_color_setup_otherlexeme (hashmap<string, string>& t) {
  string c= "black";
  t ("==<gtr>")= c; 
  t ("==")= c;
  t (":=")= c;
  t ("+=")= c;
  t ("-=")= c; 
  t ("*=")= c;
  t ("/=")= c;
  t (":=<gtr>")= c;
  t ("yield")= c;   
  t (",")= c;
  t (";")= c;
  // t ("(")= c;
  t (")")= c;
  t ("[")= c;
  t ("]")= c;
  t ("{")= c;
  t ("}")= c;
  t ("<less><less>")= c;
  t ("<less><less>*")= c;
  t ("<less><less>%")= c;
  t ("<gtr><gtr>")= c;
}

static void
parse_identifier (hashmap<string, string>& t, string s, int& pos) {
  if ('0'<=s[pos] && s[pos]<='9') return;
  int i=pos;
  while ((i<N(s)) &&
	 ((s[i]<='z' && s[i]>='a') ||
	  (s[i]<='Z' && s[i]>='A') ||
	  (s[i]<='9' && s[i]>='0') ||
	  (s[i]=='_' || s[i]=='$') ||
	  (s[i]=='?')))
    i++;
  if (!(t->contains (s (pos,i)))) pos= i;
}


static void
parse_alpha (string s, int& pos) {
  static hashmap<string,string> empty;
  parse_identifier (empty, s, pos);
}

static void
parse_whitespace (string s, int& pos) {
  while (s[pos] == ' ') pos++;
}

static void
parse_string (string s, int& pos) {
  switch (s[pos])  {
  case '\042':
    do pos++;
    while((pos<N(s)) &&
	  (s[pos-1]=='\\' && s[pos]=='\042' || s[pos]!='\042'));
    if (s[pos]=='\042') pos++;
    return;
  case '/':
    if (pos+1<N(s) && s[pos+1]=='\042') {
      pos=pos+2;
      do {
	if (pos+1<N(s) && s[pos]=='\042' && s[pos+1]=='/') {
	  pos=pos+2; return; }
	pos++;
      } while (pos<N(s));
    }
  }
}

static void
parse_keyword (hashmap<string,string>& t, string s, int& pos) {
  int i= pos;
  if (s[i]<='9' && s[i]>='0') return;
  while ((i<N(s)) && 
	 ((s[i]<='9' && s[i]>='0') ||
	  (s[i]<='Z' && s[i]>='A') ||
	  (s[i]<='z' && s[i]>='a') ||
	  s[i]=='_' || s[i]=='$' || s[i]=='?')) i++;
  string r= s (pos, i);
  if (t->contains (r) && t(r)=="#8020c0") { pos=i; return; }
}

static void
parse_constant (hashmap<string,string>& t, string s, int& pos) {
  int i=pos;
  if (s[i]<='9' && s[i]>='0') return;
  while ((i < N(s)) &&
	 ((s[i]<='9' && s[i]>='0') ||
	  (s[i]<='Z' && s[i]>='A') ||
	  (s[i]<='z' && s[i]>='a') ||
	  s[i]=='_' || s[i]=='$' || s[i]=='?')) i++;
  string r= s (pos, i);
  if (t->contains (r) && t(r)=="#2060c0") { pos=i; return; }
}

static void
parse_other_lexeme (hashmap<string,string>& t, string s, int& pos) {
  int i;
  for (i=5; i>=1; i--) {
    string r=s(pos,pos+i);
    if (t->contains(r) && t(r)=="black") {
      pos=pos+i; return; }
  }
}

static void
parse_number (string s, int& pos) {
  int i= pos;
  if (s[i] == '.') return;
  while (i<N(s) && 
	 (s[i] >= '0' && s[i]<= '9' ||
	  (s[i] == '.' && (i+1<N(s)) &&
	   ((s[i+1] >= '0' && s[i+1] <= '9') ||
	    s[i+1] == 'e' || s[i+1] == 'E')))) i++;
  if (i == pos) return;
  if (i<N(s) && (s[i] == 'e' || s[i] == 'E')) {
    i++;
    if (i<N(s) && s[i] == '-') i++;
    while (i<N(s) && (s[i] <= '9' && s[i] >= '0')) i++;
  }
  pos= i;
}

static void
parse_no_declare_type (string s, int& pos) {}

static void
parse_declare_type (string s, int& pos) {
  if (s[pos]!=':') return;
  if (pos+1<N(s) && s[pos+1]=='=') return;
  pos++;
  if (!test (s, pos, "<gtr>")) return;
  pos+=5;
}

static void
parse_comment (string s, int& pos) {
  if (s[pos]!='/') return;
  if (pos+1<N(s) && s[pos+1]=='/') {pos=N(s);return;}
  if (pos+1<N(s) && s[pos+1]=='{') {
    pos= pos+2;
    while ((pos<N(s) && s[pos]!='}') || (pos+1<N(s) && s[pos+1]!='/')) pos++;
    pos= min(pos+2,N(s));
  }
}

static void
parse_parenthesized (string s, int& pos) {
  int i=pos;
  if (s[i]!='(') return;
  int nbpar=0;
  while(i<N(s)) {
    switch (s[i]) {
    case '(':
      nbpar++;break;
    case ')':if (nbpar>0) nbpar--;
      if (nbpar==0) {i++;pos=i;return;}
      break;
    case '/':
      if (i+1<N(s) && 
	  (s[i+1]=='\042' || s[i+1]=='{' || s[i+1]=='/')) {
	pos= i; return; }
      break;
    case '\042':
      pos=i;
      return;
    }
    i++;
  }
  pos=i;
}

static void
parse_declare_function (string s, int& pos) {
  if (pos+1>=N(s)) return;
  if (s[pos]==':' && s[pos+1]=='=') { pos=pos+2; return; }
  if (s[pos]=='=' && s[pos+1]=='=') { pos=pos+2; return; }
}

string
mathemagix_language_rep::get_color (tree t, int start, int end) {
  static bool setup_done= false;
  if (!setup_done) {
    mathemagix_color_setup_constants (colored);
    mathemagix_color_setup_keywords (colored);
    mathemagix_color_setup_otherlexeme (colored);
    setup_done= true;
  }

  static string none= "";
  if (start >= end) return none;
  string s= t->label;
  int pos=0;int opos;
  bool possible_function=true;
  bool possible_type=false;
  bool possible_future_type=false;
  bool possible_future_function=true;
  string type;
  do {
    type= none;
    do {
      possible_function= possible_future_function;
      possible_type= possible_future_type;
      opos= pos;
      parse_whitespace (s, pos);
      if (opos<pos) break;
      parse_string (s, pos);
      if (opos<pos) {
	type= "string";
	possible_future_function= false;
	possible_future_type= false;
	possible_type= false;
	break;
      }
      parse_comment (s, pos);
      if (opos<pos) {
	type= "comment";
	possible_future_type= false;
	possible_type= false;
	break;
      }
      parse_keyword (colored, s, pos);
      if (opos<pos) {
	type= "keyword";
	possible_future_type= false;
	possible_type= false;
	possible_function= false;
	break;
      }
      parse_other_lexeme (colored, s, pos);  //not left parenthesis
      if (opos<pos) {
	type= "other_lexeme";
	possible_function= false;
	possible_future_function= true;
	possible_future_type= false;
	possible_type= false;
	break;
      }
      parse_constant (colored, s, pos);
      if (opos<pos) {
	type= "constant";
	possible_future_function= false;
	break;
      }
      parse_number (s, pos);
      if (opos<pos) {
	type= "number";
	possible_future_function= false;
	break;
      }
      parse_no_declare_type (s, pos); // ': :: 
      if (opos<pos) {
	type= "no_declare_type";
	possible_type= false;
	possible_future_type= false;
	possible_function= false;
	possible_future_function= false;
	break;
      }
      parse_declare_type (s, pos); // : and :>
      if (opos<pos) {
	type= "declare_type";
	possible_future_type=true; 
	possible_function= false;
	possible_future_function= false;
	break;
      }
      parse_identifier (colored, s, pos);
      if (opos<pos) {
	type="identifier";possible_future_function=false; break;
      }
      parse_parenthesized (s, pos);
      // stops after well parenthesized ) or before  // or /{ or " or /"
      if (opos<pos && pos<=start) {
	type="left_parenthesis";
	possible_function= false;
	possible_future_function= true;
	break;
      }
      if (opos<pos && possible_type==true)
	return "dark green";
      pos= opos;
      pos++;
    }
    while (false);
  }
  while (pos<=start);

  if (possible_type) return "dark green";
  if (type=="string") return "#a06040";// or #d07040
  if (type=="comment") return "brown";
  if (type=="keyword") return "#8020c0";
  if (type=="other_lexeme") return none;
  if (type=="constant") return "#2060c0";
  if (type=="number") return "#2060c0";
  if (type=="no_declare_type") return none;
  if (type=="declare_type") return none;
  if (type=="left_parenthesis") return none;
  if (type=="identifier" && possible_function==false) return none;
  if (type=="identifier") {
    possible_function= false;
    do {
      do {
	opos=pos;
	parse_whitespace (s, pos);
	if (opos<pos) break;
	parse_identifier (colored, s, pos);
	if (opos<pos) { possible_function= true; break; }
	parse_number (s, pos);
	if (opos<pos) { possible_function= true; break; }
	parse_constant (colored, s, pos);
	if (opos<pos) { possible_function= true; break; }
	parse_comment (s, pos);
	if (opos<pos) break;
	parse_parenthesized (s, pos);
	if (opos<pos) { possible_function= true; break; }
      }
      while (false);
    }
    while (opos!=pos);
    if (!possible_function) return none;
    do {
      do {
	opos=pos;
	parse_whitespace (s, pos);
	if (opos<pos) break;
	parse_identifier (colored, s, pos);
	if (opos<pos) break;
	parse_number(s,pos);
	if (opos<pos) break;
	parse_constant (colored, s, pos);
	if (opos<pos) break;
	parse_comment(s,pos);
	if (opos<pos) break;
	parse_parenthesized (s, pos);
	if (opos<pos) break;
	parse_no_declare_type (s, pos);
	if (opos<pos) break;
	parse_declare_type (s, pos);
	if (opos<pos) break;
	parse_declare_function (s, pos);
	if (opos<pos) return "#0000e0";
	parse_other_lexeme (colored, s, pos);
	if (opos<pos) return none;
      }
      while (false);
      pos++;
    }
    while (pos<N(s));
  }
  return none;
}
