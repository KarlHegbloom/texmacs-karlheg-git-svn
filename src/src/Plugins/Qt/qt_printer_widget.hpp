
/******************************************************************************
 * MODULE     : qt_printer_widget.hpp
 * DESCRIPTION: A dialog to manage printing of the document
 * COPYRIGHT  : (C) 2010 Miguel de Benito Delgado
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/
#ifndef QT_PRINTER_WIDGET_HPP
#define QT_PRINTER_WIDGET_HPP

#include "qt_widget.hpp"
#include "command.hpp"

class QTMPrinterSettings;

/*!
 * This implements a printer widget, using QTMPrinterDialog.
 *
 * The "factory" function for this widget is called printer_widget(), 
 * in qt_dialogues.cpp
 *
 * All printing options set by the user at this stage are applied as a
 * postprocessing of an already typeset postscript document.
 * Either we instruct the printing system to print specific pages, etc., or 
 * we take the Postscript file generated by TeXmacs and create a new temporary
 * one applying the options set by the user in the print dialog and then send
 * this new file to the printer.
 *
 * @see qt_printer_widget_rep::showDialog()
 */ 
class qt_printer_widget_rep: public qt_widget_rep { 
public:
  qt_printer_widget_rep (command, url);
  ~qt_printer_widget_rep () { };
  
  virtual void          send (slot s, blackbox val);
  widget plain_window_widget (string s, command q) { (void)s; (void) q; return this; }

  void showDialog ();

private:        
  static QTMPrinterSettings* _settings;
  command commandAfterExecution;    //! scheme closure to execute after printing
};

#endif  // QT_PRINTER_WIDGET_HPP

  