
/******************************************************************************
* MODULE     : vars.cpp
* DESCRIPTION: the environment variables
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "vars.hpp"

/******************************************************************************
* Various important environment variables
******************************************************************************/

string DPI ("dpi");
string SFACTOR ("sfactor");
string PREAMBLE ("preamble");
string SAVE_AUX ("save-aux");
string MODE ("mode");
string INFO_FLAG ("info-flag");
string IDENTITY ("identity");
string TABULAR ("tabular");

/******************************************************************************
* Text properties for tex, math and prog modes
******************************************************************************/

string FONT ("font");
string FONT_FAMILY ("font-family");
string FONT_SERIES ("font-series");
string FONT_SHAPE ("font-shape");
string FONT_SIZE ("font-size");
string FONT_BASE_SIZE ("font-base-size");
string MAGNIFICATION ("magnification");
string COLOR ("color");
string BG_COLOR ("bg-color");
string LANGUAGE ("language");
string ATOM_DECORATIONS ("atom-decorations");
string LINE_DECORATIONS ("line-decorations");
string PAGE_DECORATIONS ("page-decorations");
string XOFF_DECORATIONS ("xoff-decorations");
string YOFF_DECORATIONS ("yoff-decorations");

string MATH_LANGUAGE ("math-language");
string MATH_FONT ("math-font");
string MATH_FONT_FAMILY ("math-font-family");
string MATH_FONT_SERIES ("math-font-series");
string MATH_FONT_SHAPE ("math-font-shape");
string MATH_LEVEL ("math-level");
string MATH_DISPLAY ("math-display");
string MATH_CONDENSED ("math-condensed");
string MATH_VPOS ("math-vpos");

string PROG_LANGUAGE ("prog-language");
string PROG_FONT ("prog-font");
string PROG_FONT_FAMILY ("prog-font-family");
string PROG_FONT_SERIES ("prog-font-series");
string PROG_FONT_SHAPE ("prog-font-shape");
string PROG_SESSION ("prog-session");

/******************************************************************************
* Environment variables for paragraphs
******************************************************************************/

string PAR_MODE ("par-mode");
string PAR_HYPHEN ("par-hyphen");
string PAR_WIDTH ("par-width");
string PAR_LEFT ("par-left");
string PAR_RIGHT ("par-right");
string PAR_FIRST ("par-first");
string PAR_NO_FIRST ("par-no-first");
string PAR_SEP ("par-sep");
string PAR_HOR_SEP ("par-hor-sep");
string PAR_VER_SEP ("par-ver-sep");
string PAR_LINE_SEP ("par-line-sep");
string PAR_PAR_SEP ("par-par-sep");
string PAR_FNOTE_SEP ("par-fnote-sep");
string PAR_COLUMNS ("par-columns");
string PAR_COLUMNS_SEP ("par-columns-sep");

/******************************************************************************
* Environment variables for pages
******************************************************************************/

string PAGE_MEDIUM ("page-medium");
string PAGE_TYPE ("page-type");
string PAGE_ORIENTATION ("page-orientation");
string PAGE_WIDTH_MARGIN ("page-width-margin");
string PAGE_HEIGHT_MARGIN ("page-height-margin");
string PAGE_SCREEN_MARGIN ("page-screen-margin");
string PAGE_BREAKING ("page-breaking");
string PAGE_FLEXIBILITY ("page-flexibility");
string PAGE_NR ("page-nr");
string PAGE_THE_PAGE ("page-the-page");
string PAGE_WIDTH ("page-width");
string PAGE_HEIGHT ("page-height");
string PAGE_ODD ("page-odd");
string PAGE_EVEN ("page-even");
string PAGE_RIGHT ("page-right");
string PAGE_ODD_SHIFT ("page-odd-shift");
string PAGE_EVEN_SHIFT ("page-even-shift");
string PAGE_TOP ("page-top");
string PAGE_BOT ("page-bot");
string PAGE_USER_HEIGHT ("page-user-height");
string PAGE_EXTEND ("page-extend");
string PAGE_SHRINK ("page-shrink");
string PAGE_HEAD_SEP ("page-head-sep");
string PAGE_FOOT_SEP ("page-foot-sep");
string PAGE_ODD_HEADER ("page-odd-header");
string PAGE_ODD_FOOTER ("page-odd-footer");
string PAGE_EVEN_HEADER ("page-even-header");
string PAGE_EVEN_FOOTER ("page-even-footer");
string PAGE_THIS_HEADER ("page-this-header");
string PAGE_THIS_FOOTER ("page-this-footer");
string PAGE_SCREEN_WIDTH ("page-screen-width");
string PAGE_SCREEN_HEIGHT ("page-screen-height");
string PAGE_SCREEN_LEFT ("page-screen-left");
string PAGE_SCREEN_RIGHT ("page-screen-right");
string PAGE_SCREEN_TOP ("page-screen-top");
string PAGE_SCREEN_BOT ("page-screen-bot");
string PAGE_SHOW_HF ("page-show-hf");
string PAGE_FNOTE_SEP ("page-fnote-sep");
string PAGE_FNOTE_BARLEN ("page-fnote-barlen");
string PAGE_FLOAT_SEP ("page-float-sep");
string PAGE_MNOTE_SEP ("page-mnote-sep");
string PAGE_MNOTE_WIDTH ("page-mnote-width");

/******************************************************************************
* Environment variables for tables
******************************************************************************/

string TABLE_WIDTH ("table-width");
string TABLE_HEIGHT ("table-height");
string TABLE_HMODE ("table-hmode");
string TABLE_VMODE ("table-vmode");
string TABLE_HALIGN ("table-halign");
string TABLE_VALIGN ("table-valign");
string TABLE_ROW_ORIGIN ("table-row-origin");
string TABLE_COL_ORIGIN ("table-col-origin");
string TABLE_LSEP ("table-lsep");
string TABLE_RSEP ("table-rsep");
string TABLE_BSEP ("table-bsep");
string TABLE_TSEP ("table-tsep");
string TABLE_LBORDER ("table-lborder");
string TABLE_RBORDER ("table-rborder");
string TABLE_BBORDER ("table-bborder");
string TABLE_TBORDER ("table-tborder");
string TABLE_HYPHEN ("table-hyphen");
string TABLE_MIN_ROWS ("table-min-rows");
string TABLE_MIN_COLS ("table-min-cols");
string TABLE_MAX_ROWS ("table-max-rows");
string TABLE_MAX_COLS ("table-max-cols");

/******************************************************************************
* Environment variables for cells of tables
******************************************************************************/

string CELL_FORMAT ("cell-format");
string CELL_DECORATION ("cell-decoration");
string CELL_BACKGROUND ("cell-background");
string CELL_ORIENTATION ("cell-orientation");
string CELL_WIDTH ("cell-width");
string CELL_HEIGHT ("cell-height");
string CELL_HPART ("cell-hpart");
string CELL_VPART ("cell-vpart");
string CELL_HMODE ("cell-hmode");
string CELL_VMODE ("cell-vmode");
string CELL_HALIGN ("cell-halign");
string CELL_VALIGN ("cell-valign");
string CELL_LSEP ("cell-lsep");
string CELL_RSEP ("cell-rsep");
string CELL_BSEP ("cell-bsep");
string CELL_TSEP ("cell-tsep");
string CELL_LBORDER ("cell-lborder");
string CELL_RBORDER ("cell-rborder");
string CELL_BBORDER ("cell-bborder");
string CELL_TBORDER ("cell-tborder");
string CELL_VCORRECT ("cell-vcorrect");
string CELL_HYPHEN ("cell-hyphen");
string CELL_ROW_SPAN ("cell-row-span");
string CELL_COL_SPAN ("cell-col-span");
string CELL_ROW_NR ("cell-row-nr");
string CELL_COL_NR ("cell-col-nr");

/******************************************************************************
* Environment variables for graphics
******************************************************************************/

string POINT_STYLE ("point-style");

string LINE_WIDTH ("line-width");
string LINE_STYLE ("line-style");
string LINE_ARROWS ("line-arrows");
string LINE_CAPS ("line-caps");
string FILL_MODE ("fill-mode");
string FILL_COLOR ("fill-color");
string FILL_STYLE ("fill-style");

string GR_FRAME ("gr-frame");
string GR_CLIP ("gr-clip");
string GR_MODE ("gr-mode");
string GR_COLOR ("gr-color");
string GR_LINE_WIDTH ("gr-line-width");
string GR_GRID ("gr-grid");
string GR_GRID_ASPECT ("gr-grid-aspect");

/******************************************************************************
* Environment variables for preamble mode
******************************************************************************/

string SRC_STYLE ("src-style");
string SRC_SPECIAL ("src-special");
string SRC_COMPACT ("src-compact");
string SRC_CLOSE ("src-close");
