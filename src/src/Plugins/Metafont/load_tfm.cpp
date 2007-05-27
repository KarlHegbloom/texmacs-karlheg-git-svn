
/******************************************************************************
* MODULE     : load_tfm.cpp
* DESCRIPTION: load TeX font metric file
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "load_tex.hpp"
#include "analyze.hpp"
#include "timer.hpp"

RESOURCE_CODE(tex_font_metric);

/******************************************************************************
* Constructors and destructors for tex_font_metric
******************************************************************************/

// FIXME: work around compiler bug
typedef rep<tex_font_metric> rep_tex_font_metric;

tex_font_metric_rep::tex_font_metric_rep (string name):
  rep_tex_font_metric (name)
{
  header   = NULL;
  char_info= NULL;
  width    = NULL;
  height   = NULL;
  depth    = NULL;
  italic   = NULL;
  lig_kern = NULL;
  kern     = NULL;
  exten    = NULL;
  param    = NULL;
}

tex_font_metric_rep::~tex_font_metric_rep () {
  if (header != NULL) delete[] header;
  if (char_info != NULL) delete[] char_info;
  if (width != NULL) delete[] width;
  if (height != NULL) delete[] height;
  if (depth != NULL) delete[] depth;
  if (italic != NULL) delete[] italic;
  if (lig_kern != NULL) delete[] lig_kern;
  if (kern != NULL) delete[] kern;
  if (exten != NULL) delete[] exten;
  if (param != NULL) delete[] param;
}

/******************************************************************************
* Interpretation of tex_font_metric instances
******************************************************************************/

#define byte0(i) (((i)>>24)&255)
#define byte1(i) (((i)>>16)&255)
#define byte2(i) (((i)>>8)&255)
#define byte3(i) ((i)&255)
#define word0(i) (((i)>>16)&65535)
#define word1(i) ((i)&65535)

#define byte1a(i) (((i)>>20)&15)
#define byte1b(i) (((i)>>16)&15)
#define byte2x(i) (((i)>>10)&63)
#define word1x(i) ((i)&32767)

int tex_font_metric_rep::w (QN c) {
  if ((c<bc) || (c>ec)) return 0;
  return width [byte0 (char_info[c-bc])]; }
int tex_font_metric_rep::h (QN c) {
  if ((c<bc) || (c>ec)) return 0;
  return height [byte1a (char_info[c-bc])]; }
int tex_font_metric_rep::d (QN c) {
  if ((c<bc) || (c>ec)) return 0;
  return depth [byte1b (char_info[c-bc])]; }
int tex_font_metric_rep::i (QN c) {
  if ((c<bc) || (c>ec)) return 0;
  return italic [byte2x (char_info[c-bc])]; }
int tex_font_metric_rep::tag (QN c) { return (char_info [c-bc]>>8)&3; }
int tex_font_metric_rep::rem (QN c) { return char_info  [c-bc] & 255; }
QN  tex_font_metric_rep::top (QN c) { return (QN) byte0 (exten [rem (c)]); }
QN  tex_font_metric_rep::mid (QN c) { return (QN) byte1 (exten [rem (c)]); }
QN  tex_font_metric_rep::bot (QN c) { return (QN) byte2 (exten [rem (c)]); }
QN  tex_font_metric_rep::rep (QN c) { return (QN) byte3 (exten [rem (c)]); }
int tex_font_metric_rep::design_size () { return header[1]; }
int tex_font_metric_rep::parameter (int i) { return (i<np)? param [i]: 0; }
int tex_font_metric_rep::spc () { return parameter (1); }
int tex_font_metric_rep::spc_stretch () { return parameter (2); }
int tex_font_metric_rep::spc_shrink () { return parameter (3); }
int tex_font_metric_rep::x_height () { return parameter (4); }
int tex_font_metric_rep::spc_quad () { return parameter (5); }
int tex_font_metric_rep::spc_extra () { return parameter (6); }

int
tex_font_metric_rep::list_len (QN c) {
  if (tag(c)!=2) return 1;
  return list_len (rem (c)) + 1;
}

QN
tex_font_metric_rep::nth_in_list (QN c, int n) {
  if ((n==1) || (tag(c)!=2)) return c;
  return nth_in_list (rem (c), n-1);
}

double
tex_font_metric_rep::slope () {
  double slope= ((double) parameter(0)) / ((double) (1<<20));
  if (slope >= 1.0) slope= 0.25;
  if (slope <= -1.0) slope= -0.25;
  return slope;
}

/******************************************************************************
* Execution of the ligature kerning program
*------------------------------------------------------------------------------
* (s, n) the input string of length n
* buf    the output string of maximal length m
* ker    the output kerning array of maximal length m
* m      at input : maximal length of buf and ker;
*        at output: the length of buf and ker.
******************************************************************************/

void
tex_font_metric_rep::execute (int* s, int n, int* buf, int* ker, int& m) {
  STACK_NEW_ARRAY (stack, int, m);
  int bp, sp=0, i;

  for (i=0; i<n; i++) stack[sp++]= s[n-1-i];
  sp--; bp= 0;

  while (sp>=0) {
    int cur_char= stack [sp]& 255;
    // cout << "Processing " << (char) cur_char << "\n";

    /***************** the ligature-kerning program ******************/
    if ((cur_char<bc) || (cur_char>ec)) sp--;
    else if ((tag (cur_char)==1) && (sp>0)) {
      register int next_char= stack [sp-1]& 255;
      register int pc= rem (cur_char);
      if (byte0 (lig_kern [pc]) > 128) pc= word1 (lig_kern [pc]);

      while (true) {
	register int instr= lig_kern [pc];

	if (byte0 (instr) >= 128) { // halt
	  // cout << "  Halt\n";
	  ker [bp]  = 0;
	  buf [bp++]= stack[sp--];
	  break;
	}

	if (byte1 (instr) != next_char) { // continue
	  // cout << "  " << (char) byte1 (instr) << " != " << (char) next_char
	  //      << " => pc := pc + " << (byte0 (instr)+1) << "\n";
	  pc += byte0 (instr)+1;
	  continue;
	}

	// cout << "  " << (char) byte1 (instr) << " == "
	//      << (char) next_char << " => ";

	if (byte2 (instr) < 128) { // ligature
	  // cout << "Ligature ";
	  int code= byte2 (instr);
	  int a   = code>>2;
	  int b   = (code>>1)&1;
	  int c   = code&1;
	  // cout << "(" << a << "," << b << "," << c << ")\n";
	  if (b==0) sp--;
	  stack [sp++]= byte3 (instr);
	  if (c!=0) stack[sp++]= cur_char;
	  sp--;
	  while (a>0) {
	    ker [bp]  = 0;
	    buf [bp++]= stack [sp--];
	    a--;
	  }
	  break;
	}

	else { // kerning
	  // cout << "Kerning (" << kern  [word1x (instr)] << ")\n";
	  ker [bp]  = kern  [word1x (instr)];
	  buf [bp++]= stack [sp--];
	  break;
	}
      }
    }
    else {
      ker [bp]  = 0;
      buf [bp++]= stack [sp--];
    }
    /***************** end ligature-kerning program ******************/

    if ((bp>=m-2) || (sp>=m-2)) {
      cerr << "\nString is ";
      for (i=0; i<n; i++) cerr << (char) s[i];
      cerr << "\n";
      fatal_error ("String too complex for ligature kerning",
		   "tex_font_metric_rep::execute",
		   "load-tfm.cpp");
    }
  }
  
  m= bp;
  STACK_DELETE_ARRAY (stack);
}

/******************************************************************************
* Get the individual horzontal offsets of characters
******************************************************************************/

#define conv(x) ((SI) (((double) (x))*unit))

#define ADVANCE(k)                         \
  x += conv (w(stack[sp--]) + k);          \
  x_bis= x;                                \
  if (pos < n-sp) xpos [pos++] = x;

#define SKIP                               \
  sp--;                                    \
  if (pos < n-sp) {                        \
    x_bis += conv (w(stack[sp+1]));        \
    xpos [pos++] = x_bis;                  \
  }

void
tex_font_metric_rep::get_xpositions (int* s, int n, double unit, SI* xpos) {
  SI  x    = 0;
  SI  x_bis= 0;
  int pos  = 1;

  int m= n + 16;
  STACK_NEW_ARRAY (stack, int, m);
  int bp, sp=0, i;

  for (i=0; i<n; i++) stack[sp++]= s[n-1-i];
  sp--; bp= 0;

  while (sp>=0) {
    int cur_char= stack [sp]& 255;

    /***************** the ligature-kerning program ******************/
    if ((cur_char<bc) || (cur_char>ec)) { SKIP; }
    else if ((tag (cur_char)==1) && (sp>0)) {
      register int next_char= stack [sp-1]& 255;
      register int pc= rem (cur_char);
      if (byte0 (lig_kern [pc]) > 128) pc= word1 (lig_kern [pc]);

      while (true) {
	register int instr= lig_kern [pc];
	if (byte0 (instr) >= 128) { ADVANCE (0); break; }
	if (byte1 (instr) != next_char) { pc += byte0 (instr)+1; continue; }
	if (byte2 (instr) < 128) {
	  int code= byte2 (instr);
	  int a   = code>>2;
	  int b   = (code>>1)&1;
	  int c   = code&1;
	  if (b==0) SKIP;
	  stack [sp++]= byte3 (instr);
	  if (c!=0) stack [sp++]= cur_char;
	  SKIP;
	  while (a>0) { ADVANCE (0); a--; }
	  break;
	}
	else { ADVANCE (kern [word1x (instr)]); break; }
      }
    }
    else ADVANCE (0);
    /***************** end ligature-kerning program ******************/

    if ((bp>=m-2) || (sp>=m-2)) {
      cerr << "\nString is ";
      for (i=0; i<n; i++) cerr << (char) s[i];
      cerr << "\n";
      fatal_error ("String too complex for ligature kerning",
		   "tex_font_metric_rep::get_xpositions",
		   "load-tfm.cpp");
    }
  }
  STACK_DELETE_ARRAY (stack);
}

#undef SKIP
#undef ADVANCE
#undef unit

/******************************************************************************
* Output of tex_font_metric instances
******************************************************************************/

static char* HOR_RULE= "---------------------------------------------------------------------------\n";

double
fixed (int i) {
  double x= ((double) i) / ((double) (1<<20));
  int j= (int) (1000*x);
  return ((double) j)*0.001;
}

void
print (tex_font_metric tfm) {
  int i;
  cout << HOR_RULE;
  cout << "name:        " << tfm->res_name << "\n";
  cout << HOR_RULE;
  cout << "checksum:    " << tfm->header[0] << "\n";
  cout << "design size: " << fixed (tfm->header[1]) << "\n";

  cout << HOR_RULE;
  for (i=tfm->bc; i<=tfm->ec; i++) {
    cout << "character ";
    if ((i&127)<32) cout << i << ":\t";
    else cout << ((char) i) << ":\t";
    cout << "w=" << fixed (tfm->w(i)) << ", ";
    cout << "h=" << fixed (tfm->h(i)) << ", ";
    cout << "d=" << fixed (tfm->d(i)) << ", ";
    cout << "i=" << fixed (tfm->i(i));
    switch (tfm->tag (i)) {
    case 1: cout << " [lig " << tfm->rem(i) << "]"; break;
    case 2: cout << " [list " << tfm->rem(i) << "]"; break;
    case 3: cout << " [ext "
		 << (int) tfm->top(i) << ", "
		 << (int) tfm->mid(i) << ", " 
		 << (int) tfm->bot(i) << ", " 
		 << (int) tfm->rep(i) << "]"; break;
    }
    cout << "\n";
  }

  cout << HOR_RULE;
  if (tfm->left!=-1)
    cout << "Left boundary character:  " << tfm->left << "\n";
  if (tfm->right!=-1)
    cout << "Right boundary character: " << tfm->right << "\n";
  if (tfm->left_prog!=-1) 
    cout << "Left boundary program:    " << tfm->left_prog << "\n";
  if (tfm->right_prog!=-1)
    cout << "Right boundary program:   " << tfm->right_prog << "\n";
  if ((tfm->left==-1) && (tfm->right==-1) &&
      (tfm->left_prog==-1) && (tfm->right_prog==-1))
    cout << "No boundary characters or programs\n";

  cout << HOR_RULE;
  cout << "Slope:         " << tfm->slope () << "\n";
  cout << "Space:         " << fixed (tfm->spc ()) << "\n";
  cout << "Space_stretch: " << fixed (tfm->spc_stretch ()) << "\n";
  cout << "Space_shrink:  " << fixed (tfm->spc_shrink ()) << "\n";
  cout << "X height:      " << fixed (tfm->x_height ()) << "\n";
  cout << "Quad space:    " << fixed (tfm->spc_quad ()) << "\n";
  cout << "Extra space:   " << fixed (tfm->spc_extra ()) << "\n";

  cout << HOR_RULE;
  for (i=7; i<tfm->np; i++)
    cout << "Parameter " << i << ": " << fixed (tfm->parameter (i)) << "\n";

  cout << HOR_RULE;
}

/******************************************************************************
* Main program for loading
******************************************************************************/

tex_font_metric
load_tfm (url file_name, string family, int size) {
  tex_font_metric tfm=
    new tex_font_metric_rep (family * as_string (size) * ".tfm");

  int i= 0;
  string s;
  (void) load_string (file_name, s, true);
  bench_start ("decode tfm");

  parse (s, i, tfm->lf);
  parse (s, i, tfm->lh);
  parse (s, i, tfm->bc);
  parse (s, i, tfm->ec);
  parse (s, i, tfm->nw);
  parse (s, i, tfm->nh);
  parse (s, i, tfm->nd);
  parse (s, i, tfm->ni);
  parse (s, i, tfm->nl);
  parse (s, i, tfm->nk);
  parse (s, i, tfm->ne);
  parse (s, i, tfm->np);

  if ((tfm->lf-6) !=
      (tfm->lh + (tfm->ec + 1 - tfm->bc) +
       tfm->nw + tfm->nh + tfm->nd + tfm->ni +
       tfm->nl + tfm->nk + tfm->ne + tfm->np))
    fatal_error ("invalid tfm file", "load_tfm", "load-tfm.cpp");
  
  parse (s, i, tfm->header, tfm->lh);
  parse (s, i, tfm->char_info, tfm->ec+1- tfm->bc);
  parse (s, i, tfm->width, tfm->nw);
  parse (s, i, tfm->height, tfm->nh);
  parse (s, i, tfm->depth, tfm->nd);
  parse (s, i, tfm->italic, tfm->ni);
  parse (s, i, tfm->lig_kern, tfm->nl);
  parse (s, i, tfm->kern, tfm->nk);
  parse (s, i, tfm->exten, tfm->ne);
  parse (s, i, tfm->param, tfm->np);
  
  tfm->left= tfm->right= tfm->left_prog= tfm->right_prog= -1;
  if (tfm->nl > 0) {
    int l= tfm->lig_kern [0];
    int r= tfm->lig_kern [tfm->nl- 1];
    if (byte0 (l) == 255) tfm->right= byte1 (l);
    if (byte0 (r) == 255) tfm->left_prog= word1 (r);
  }

  tfm->size= (tfm->header[1] + (1<<19)) >> 20;

  bench_cumul ("decode tfm");
  return tfm;
}
