
/******************************************************************************
* MODULE     : vars.hpp
* DESCRIPTION: environment variables for typesetting
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#ifndef VARS_H
#define VARS_H
#include "string.hpp"

/******************************************************************************
* System environment variables
******************************************************************************/

extern string DPI;
extern string SFACTOR;
extern string MAGNIFICATION;

extern string PREAMBLE;
extern string MODE;
extern string TEXT_LANGUAGE;
extern string TEXT_FONT;
extern string TEXT_FAMILY;
extern string TEXT_SHAPE;
extern string TEXT_SERIES;
extern string MATH_LANGUAGE;
extern string MATH_FONT;
extern string MATH_FAMILY;
extern string MATH_SHAPE;
extern string MATH_SERIES;
extern string PROG_LANGUAGE;
extern string PROG_FONT;
extern string PROG_FAMILY;
extern string PROG_SHAPE;
extern string PROG_SERIES;
extern string FONT_BASE_SIZE;
extern string FONT_SIZE;
extern string INDEX_LEVEL;
extern string DISPLAY_STYLE;
extern string MATH_CONDENSED;
extern string VERTICAL_POS;
extern string MATH_SPACING;
extern string COLOR;
extern string BACKGROUND_COLOR;
extern string LINE_WIDTH;
extern string LINE_STYLE;
extern string LINE_ARROWS;
extern string LINE_CAPS;
extern string FILL_MODE;
extern string FILL_COLOR;
extern string FILL_STYLE;
extern string THIS_SESSION;
extern string INFO_FLAG;

extern string ATOM_DECORATIONS;
extern string LINE_DECORATIONS;
extern string PAGE_DECORATIONS;
extern string XOFF_DECORATIONS;
extern string YOFF_DECORATIONS;

extern string PAR_MODE;
extern string PAR_HYPHEN;
extern string PAR_WIDTH;
extern string PAR_LEFT;
extern string PAR_RIGHT;
extern string PAR_FIRST;
extern string PAR_NO_FIRST;
extern string PAR_SEP;
extern string PAR_HOR_SEP;
extern string PAR_LINE_SEP;
extern string PAR_PAR_SEP;
extern string PAR_FNOTE_SEP;
extern string PAR_COLUMNS;
extern string PAR_COLUMNS_SEP;

extern string PAGE_MEDIUM;
extern string PAGE_TYPE;
extern string PAGE_ORIENTATION;
extern string PAGE_BREAKING;
extern string PAGE_FLEXIBILITY;
extern string PAGE_NR;
extern string PAGE_THE_PAGE;
extern string PAGE_WIDTH;
extern string PAGE_HEIGHT;
extern string PAGE_ODD;
extern string PAGE_EVEN;
extern string PAGE_RIGHT;
extern string PAGE_TOP;
extern string PAGE_BOT;
extern string PAGE_SHRINK;
extern string PAGE_EXTEND;
extern string PAGE_HEAD_SEP;
extern string PAGE_FOOT_SEP;
extern string PAGE_ODD_HEADER;
extern string PAGE_ODD_FOOTER;
extern string PAGE_EVEN_HEADER;
extern string PAGE_EVEN_FOOTER;
extern string PAGE_THIS_HEADER;
extern string PAGE_THIS_FOOTER;
extern string PAGE_REDUCE_LEFT;
extern string PAGE_REDUCE_RIGHT;
extern string PAGE_REDUCE_TOP;
extern string PAGE_REDUCE_BOT;
extern string PAGE_SHOW_HF;
extern string PAGE_FNOTE_SEP;
extern string PAGE_FNOTE_BARLEN;
extern string PAGE_FLOAT_SEP;
extern string PAGE_MNOTE_SEP;
extern string PAGE_MNOTE_WIDTH;

extern string TABLE_WIDTH;
extern string TABLE_HEIGHT;
extern string TABLE_HMODE;
extern string TABLE_VMODE;
extern string TABLE_HALIGN;
extern string TABLE_VALIGN;
extern string TABLE_ROW_ORIGIN;
extern string TABLE_COL_ORIGIN;
extern string TABLE_LSEP;
extern string TABLE_RSEP;
extern string TABLE_BSEP;
extern string TABLE_TSEP;
extern string TABLE_LBORDER;
extern string TABLE_RBORDER;
extern string TABLE_BBORDER;
extern string TABLE_TBORDER;
extern string TABLE_HYPHEN;
extern string TABLE_MIN_ROWS;
extern string TABLE_MIN_COLS;
extern string TABLE_MAX_ROWS;
extern string TABLE_MAX_COLS;

extern string CELL_FORMAT;
extern string CELL_DECORATION;
extern string CELL_BACKGROUND;
extern string CELL_ORIENTATION;
extern string CELL_WIDTH;
extern string CELL_HEIGHT;
extern string CELL_HPART;
extern string CELL_VPART;
extern string CELL_HMODE;
extern string CELL_VMODE;
extern string CELL_HALIGN;
extern string CELL_VALIGN;
extern string CELL_LSEP;
extern string CELL_RSEP;
extern string CELL_BSEP;
extern string CELL_TSEP;
extern string CELL_LBORDER;
extern string CELL_RBORDER;
extern string CELL_BBORDER;
extern string CELL_TBORDER;
extern string CELL_ROW_SPAN;
extern string CELL_COL_SPAN;
extern string CELL_VCORRECT;
extern string CELL_HYPHEN;
extern string CELL_ROW_NR;
extern string CELL_COL_NR;

extern string GR_FRAME;
extern string GR_CLIP;
extern string GR_MODE;
extern string GR_COLOR;
extern string GR_LINE_WIDTH;

extern string IDENTITY;
extern string TABULAR;

#endif // defined VARS_H
