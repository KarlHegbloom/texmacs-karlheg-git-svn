
/******************************************************************************
* MODULE     : qt_utilities.cpp
* DESCRIPTION: Utilities for QT
* COPYRIGHT  : (C) 2007  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "qt_utilities.hpp"

#include <QImage>
#include <QPrinter>
#include <QPainter>
#include <QCoreApplication>
#include <QLocale>
#include <QDateTime>
#include <QTextCodec>
#include <QHash>
#include <QStringList>
#include <QKeySequence>

#include <QPrinter>
#include <QPrintDialog>

#include "rgb_colors.hpp"
#include "dictionary.hpp"
#include "converter.hpp"
#include "language.hpp"
#include "scheme.hpp"

#include "qt_gui.hpp"    // gui_maximal_extents()

#ifdef USE_GS
#include "Ghostscript/gs_utilities.hpp"
#endif

/*! some debugging infrastucture */
tm_ostream&
operator << (tm_ostream& out, QRect rect) {
  return out << "(" << rect.x() << "," << rect.y() << ","
  << rect.width() << "," << rect.height() << ")";
}

QFont
to_qfont (int style, QFont font) {
  if (style & WIDGET_STYLE_MINI)  // use smaller text font inside widget
    font.setPointSize(10);
  if (style & WIDGET_STYLE_MONOSPACED)  // use monospaced font inside widget
    font.setFixedPitch(true);     //FIXME?
//if (style & WIDGET_STYLE_GREY)  // use grey text font
//    font.set += "color: #414141";
  if (style & WIDGET_STYLE_PRESSED)   // indicate that a button is currently pressed
    {}
  if (style & WIDGET_STYLE_INERT)  // only render but don't associate any action to widget
    {}
  if (style & WIDGET_STYLE_BUTTON)  // indicate that a button should explicitly rendered as a button
    {}
  if (style & WIDGET_STYLE_BOLD)
    font.setBold(true);
  return font;

}

QString
parse_tm_style (int style) {
  QString sheet;
  if (style & WIDGET_STYLE_MINI)  // use smaller text font inside widget
    sheet += "font-size: 10pt;";
  if (style & WIDGET_STYLE_MONOSPACED)  // use monospaced font inside widget
    sheet += "font-family: \"monospace\";";
  if (style & WIDGET_STYLE_GREY)  // use grey text font
    sheet += "color: #414141;";
  if (style & WIDGET_STYLE_PRESSED)   // indicate that a button is currently pressed
    sheet += "";
  if (style & WIDGET_STYLE_INERT)  // only render but don't associate any action to widget
    sheet += "";
  if (style & WIDGET_STYLE_BUTTON)  // indicate that a button should explicitly rendered as a button
    sheet += "";
  if (style & WIDGET_STYLE_CENTERED)  // use centered text
    sheet += "text-align: center;";
  if (style & WIDGET_STYLE_BOLD)
    sheet += "font-weight: bold;";
  if (DEBUG_QT)
    sheet += "border:1px solid rgb(255, 0, 0);";
  return sheet;
}

/*! */
QString
to_qstylesheet (int style) {
  if (DEBUG_QT) {
    return "* {" + parse_tm_style(style) + "}" + 
           "*:hover { background-color: rgb(255,220,220); }";
  }
  return "* {" + parse_tm_style(style) + "}";
}

QString
to_qstylesheet (int style, color c) {
  int r,g,b,a;
  get_rgb_color(c, r, g, b, a);
  a = a*100/255;
  return "* {" + parse_tm_style(style)
    + QString("color: rgba(%1, %2, %3, %4%);").arg(r).arg(g).arg(b).arg(a)
    + "}";
}


/*! Try to convert a TeXmacs lenght (em, px, w, h) into a QSize.
 
 This uses the widget's current size to compute relative sizes as specified
 with "FFw", where FF is the string representation of a double.
 A value of "1w" should not affect the widget size.
 
 FIXME: does 1w mean 100% of the contents' size or 100% of the available size?
 */
QSize
qt_decode_length (string width, string height, const QSize& ref, const QFontMetrics& fm) {
  QSize size = ref;

    // Width as a function of the default width
  if (ends (width, "w") && is_double (width (0, N(width) - 1))) {
    double x = as_double (width (0, N(width) - 1));
    size.rwidth() *= x;
  }
    // Width as a function of the default height
  else if (ends (width, "h") && is_double (width (0, N(width) - 1))) {
    double y = as_double (width (0, N(width) - 1));
    size.rwidth() = y * size.height();
  }
    // Absolute EM units
  else if (ends (width, "em") && is_double (width (0, N(width) - 2))) {
    double x = as_double (width (0, N(width) - 2));
    size.setWidth(x * fm.width("M")); 
  }
    // Absolute pixel units 
  else if (ends (width, "px") && is_double (width (0, N(width) - 2))) {
    double x = as_double (width (0, N(width) - 2));
    size.setWidth(x);
  }

    // Height as a function of the default width
  if (ends (height, "w") && is_double (height (0, N(height) - 1))) {
    double x = as_double (height (0, N(height) - 1));
    size.rheight() = x * size.width();
  }
    // Height as a function of the default height
  else if (ends (height, "h") && is_double (width (0, N(width) - 1))) {
    double y = as_double (height (0, N(height) - 1));
    size.rheight() *= y;
  }
  else if (ends (height, "em") && is_double (height (0, N(height) - 2))) {
    double y = as_double (height (0, N(height) - 2));
    size.setHeight(y * fm.width("M")); 
  }
  else if (ends (height, "px") && is_double (height (0, N(height) - 2))) {
    double y = as_double (height (0, N(height) - 2));
    size.setHeight(y);
  }
  return size;
}


// used only by to_qkeysequence
static string
conv_sub (string ks) {
  string r(ks);
#ifdef Q_WS_MAC
  r = replace (r, "S-", "Shift+");
  r = replace (r, "C-", "Meta+");
  r = replace (r, "A-", "Alt+");
  r = replace (r, "M-", "Ctrl+");
  //r = replace (r, "K-", "");
  r = replace (r, " ", ",");
#else
  r = replace (r, "S-", "Shift+");
  r = replace (r, "C-", "Ctrl+");
  r = replace (r, "A-", "Alt+");
  r = replace (r, "M-", "Meta+");
  //r = replace (r, "K-", "");
  r = replace (r, " ", ",");
#endif
  if (N(r) == 1 || (N(r) > 2 && r[N(r)-2] == '+')) {
    if (is_locase (r[N(r)-1]))
      r= r (0, N(r)-1) * upcase_all (r (N(r)-1, N(r)));
    else if (is_upcase (r[N(r)-1]))
      r= r (0, N(r)-1) * "Shift+" * upcase_all (r (N(r)-1, N(r)));
  }
  return r;
}

QKeySequence
to_qkeysequence (string s) {
  int i=0, k;
  string r;
  for (k=0; k<=N(s); k++)
    if (k == N(s) || s[k] == ' ') {
      r << conv_sub (s (i, k));
      i= k;
    }
  return QKeySequence(to_qstring(r));
}


coord4
from_qrect (const QRect & rect) {
  SI c1, c2, c3, c4;
  c1= rect.x() * PIXEL;
  c2= -(rect.y() + rect.height()) * PIXEL;       
  c3= (rect.x() + rect.width()) * PIXEL;
  c4= -rect.y() * PIXEL;
  return coord4 (c1, c2, c3, c4);
}


/*! Transforms a rectangle given by its lower left and upper right corners
 into one given by its upper left and width/height */
QRect
to_qrect (const coord4 & p) {
  float c= 1.0/PIXEL;
  return QRect (p.x1*c, -p.x4*c, (p.x3-p.x1+PIXEL-1)*c, (p.x4-p.x2+PIXEL-1)*c);
}

coord2
from_qsize (const QSize & s) {
  return coord2 (s.width() * PIXEL, s.height() * PIXEL);
}

QSize
to_qsize (const coord2 & p) {
  float c= 1.0/PIXEL;
  return QSize (p.x1*c, p.x2*c);
}

QSize
to_qsize (const SI& w, const SI& h) {
  float c= 1.0/PIXEL;
  return QSize (w*c, h*c);
}

coord2
from_qpoint (const QPoint & pt) {
  return coord2 (pt.x() * PIXEL, -pt.y() * PIXEL);
}

/*! Transforms texmacs coordinates, with origin at the lower left corner, into
 Qt coordinates, with origin at the upper left corner */
QPoint
to_qpoint (const coord2 & p) {
  float c= 1.0/PIXEL;
  return QPoint (p.x1*c, -p.x2*c);
}

array<string>
from_qstringlist(const QStringList& l) {
  array<string> tl (l.size());
  for(QStringList::const_iterator it = l.begin(); it != l.end(); ++it)
    tl << from_qstring(*it);
  return tl;
}

QStringList
to_qstringlist(array<string> l) {
  QStringList ql;
  for(int i=0; i<N(l); ++i)
    ql << to_qstring(l[i]);
  return ql;
}

QString
to_qstring (string s) {
  return utf8_to_qstring (cork_to_utf8 (s));
}

QString
utf8_to_qstring (string s) {
  char* p= as_charp (s);
  QString nss= QString::fromUtf8 (p, N(s));
  tm_delete_array (p);
  return nss;
}

string
from_qstring_utf8 (const QString &s) {
  QByteArray arr= s.toUtf8 ();
  const char* cstr= arr.constData ();
  return string ((char*) cstr);
}

string
from_qstring (const QString &s) {
  return utf8_to_cork (from_qstring_utf8(s));
}

// Although slow to build, this should provide better lookup times than
// linearly traversing the array of colors.
static QHash<QString, QColor> _NamedColors;

/*!
 * This needn't be called more than once. Takes RGBColors, defined in
 * rgb_colors.hpp and initializes our QHash
 */
void initNamedColors(void) {
  for(int i = 0; i < RGBColorsSize; ++i)
    _NamedColors.insert(QString(RGBColors[i].name), 
                        QColor(RGBColors[i].r, RGBColors[i].g, RGBColors[i].b));
}

/*!
 * Takes either an hexadecimal RGB color, as in #e3a1ff, or a named color
 * as those defined in src/Graphics/Renderer/rgb_colors.hpp and returns a QColor
 */
QColor
to_qcolor (const string& col) {
  QString _col = to_qstring(col);
  if(_col.startsWith("#"))
    return QColor(_col);
  if(_NamedColors.isEmpty())
    initNamedColors();
  if(_NamedColors.contains(_col))
    return _NamedColors[_col];
  if(DEBUG_QT)
    cout << "to_qcolor(" << col << "): name is not defined in RGBColors.\n";
  return QColor(100,100,100);  // FIXME? 
}

/*! Returns a color encoded as a string with hexadecimal RGB values, as in #e3a1ff */
string
from_qcolor (const QColor& col) {
  return from_qstring(col.name());
}

// FIXME: Unnecessary? QColor::setRgba() does this.
QColor
to_qcolor(color c) {
  int r, g, b, a;
  get_rgb_color (c, r, g, b, a);
  return QColor (r, g, b, a);
}


QString
qt_translate (string s) {
  string in_lan= get_input_language ();
  string out_lan= get_output_language ();
  return to_qstring(tm_var_encode (translate (s, in_lan, out_lan)));
}

//FIXME!?!?
bool
qt_supports (url u) {
  string s= suffix (u);
  if (s == "ps" || s == "eps" || s == "pdf") return false;
  return true;
}

void
qt_image_size (url image, int& w, int& h) {
  //cout <<  concretize (image) << LF;
  QImage im= QImage (utf8_to_qstring (concretize (image)));
  if (im.isNull ()) {
    if (as_bool (call ("file-converter-exists?", image, "x.png"))) {
      url temp= url_temp (".png");
      call ("file-convert", object (image), object (temp));
      qt_image_size (temp, w, h);
      remove (temp);
    }
    else {
      cerr << "TeXmacs] cannot read image file '" << image << "'" 
           << " in qt_image_size" << LF;
      w= 35; h= 35;
    }
  }
  else {
    w= im.width ();
    h= im.height ();
  }
}

void
qt_convert_image (url image, url dest, int w, int h) {
  QImage im (utf8_to_qstring (concretize (image)));
  if (im.isNull ())
    cerr << "TeXmacs] cannot read image file '" << image << "'" 
         << " in qt_convert_image" << LF;
  else {
    if (w > 0 && h > 0) 
      im= im.scaled (w, h, Qt::IgnoreAspectRatio, Qt::SmoothTransformation);
    im.scaled (w, h).save (utf8_to_qstring (concretize (dest)));
  }
}

void
qt_image_to_eps (url image, url eps, int w_pt, int h_pt, int dpi) {
  static const char* d= "0123456789ABCDEF";
  QImage im (utf8_to_qstring (concretize (image)));
  if (im.isNull ())
    cerr << "TeXmacs Cannot read image file '" << image << "'"
	 << " in qt_image_to_eps" << LF;
  else {
    if (dpi > 0 && w_pt > 0 && h_pt > 0) {
      int ww= w_pt * dpi / 72;
      int hh= h_pt * dpi / 72;
      if (ww < im.width () || hh < im.height ()) {
        im= im.scaled (ww, hh, Qt::IgnoreAspectRatio, Qt::SmoothTransformation);
      }
    }
    string res;
    string sw= as_string (im.width ());
    string sh= as_string (im.height ());
    res << "%!PS-Adobe-3.0 EPSF-3.0\n%%Creator: TeXmacs\n%%BoundingBox: ";
    res << "0 0 " << sw << " " << sh << "\n";
    res << "%%LanguageLevel: 2\n%%Pages: 1\n%%DocumentData: Clean7Bit\n";
    res <<  sw << " " << sh << " scale\n";
    res <<  sw << " " << sh << " 8 [" << sw << " 0 0 -" << sh << " 0 " << sh << "]\n";
    res << "{currentfile 3 " << sw << " mul string readhexstring pop} bind\nfalse 3 colorimage\n";
    int v, i= 0, j= 0, l= 0;
    for (j= 0; j < im.height (); j++) {
      for (i=0; i < im.width (); i++) {
        l++;
        QRgb p= im.pixel (i, j);
        v= qRed (p);
        res << d [(v >> 4)] << d [v % 16];
        v= qGreen (p);
        res << d [(v >> 4)] << d [v % 16];
        v= qBlue (p);
        res << d [(v >> 4)] << d [v % 16];
        if (l > 10) {
          res << "\n";
          l= 0;
        }
      }
    }
    res << "\n%%EOF";
    save_string (eps, res);
#ifdef USE_GS
    url temp= url_temp (".eps");
    gs_to_eps (eps, temp);
    copy (temp, eps);
    remove (temp);
#endif
  }
}

string 
qt_application_directory () {
  return  string (QCoreApplication::applicationDirPath () .toAscii() .constData());
  // return from_qstring (QCoreApplication::applicationDirPath ());
}

string
qt_get_date (string lan, string fm) {
  QDateTime localtime = QDateTime::currentDateTime();
  if (fm == "") {
    if ((lan == "british") || (lan == "english") || (lan == "american"))
      fm = "MMMM d, yyyy";
    else if (lan == "german")
      fm = "d. MMMM yyyy";
    else if (lan == "chinese" || lan == "japanese" ||
             lan == "korean" || lan == "taiwanese")
    {
      string y = as_string(localtime.date().year());
      string m = as_string(localtime.date().month());
      string d = as_string(localtime.date().day());
      if (lan == "japanese")
        return y * "<#5e74>" * m * "<#6708>" * d * "<#65e5>";
      if (lan == "korean")
        return y * "<#b144> " * m * "<#c6d4> " * d * "<#c77c>";
      return y * "," * m * "," * d;
    }
    else fm = "d MMMM yyyy";
  }
  QLocale loc = QLocale(to_qstring(language_to_locale(lan)));
#if (QT_VERSION >= 0x040400)
  QString date = loc.toString(localtime, to_qstring(fm));
#else
  QString date = localtime.toString(to_qstring(fm));
#endif
  return from_qstring(date);
}

#ifndef _MBD_EXPERIMENTAL_PRINTER_WIDGET  // this is in qt_printer_widget

#define PAPER(fmt)  case QPrinter::fmt : return "fmt"
static string 
qt_papersize_to_string( QPrinter::PaperSize sz ) {
  switch (sz) {
      PAPER (A0) ;
      PAPER (A1) ;
      PAPER (A2) ;
      PAPER (A3) ;
      PAPER (A4) ;
      PAPER (A5) ;
      PAPER (A6) ;
      PAPER (A7) ;
      PAPER (A8) ;
      PAPER (A9) ;
      PAPER (B0) ;
      PAPER (B1) ;
      PAPER (B2) ;
      PAPER (B3) ;
      PAPER (B4) ;
      PAPER (B5) ;
      PAPER (B6) ;
      PAPER (B7) ;
      PAPER (B8) ;
      PAPER (B9) ;
      PAPER (B10) ;      
      PAPER (Letter) ;
      
    default:
      return "A4";
  }
}
#undef PAPER


bool 
qt_print (bool& to_file, bool& landscape, string& pname, url& filename, 
          string& first, string& last, string& paper_type) {
  static QPrinter *qprinter = NULL;
  if (!qprinter) {
    qprinter = new QPrinter;
  }
  QPrintDialog pdialog(qprinter);
  if (pdialog.exec() == QDialog::Accepted) {
    to_file = !(qprinter->outputFileName().isNull());
    pname = from_qstring( qprinter->printerName() );
    filename = from_qstring( qprinter->outputFileName() );
    landscape = (qprinter->orientation() == QPrinter::Landscape);
    paper_type = qt_papersize_to_string(qprinter->paperSize());
    if (qprinter->printRange() == QPrinter::PageRange) {
      first = qprinter->fromPage(); 
      last = qprinter->toPage(); 
    }
    //cout << "Printer :" << pname << LF;
    //cout << "File :" << filename << LF;
    return true;
  }
  return false;
}

#endif //_MBD_EXPERIMENTAL_PRINTER_WIDGET
