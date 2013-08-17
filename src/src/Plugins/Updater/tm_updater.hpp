/******************************************************************************
 * MODULE     : tm_updater.hpp
 * DESCRIPTION: Base class for auto-update frameworks like (Win)Sparkle
 * COPYRIGHT  : (C) 2013 Miguel de Benito Delgado
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#ifndef TM_UPDATER_HPP
#define TM_UPDATER_HPP

#include "url.hpp"

class tm_updater
{
protected:
  url appcast;
  int interval;
  
  tm_updater () { }
  tm_updater (const tm_updater&);
  void operator= (const tm_updater&);
  virtual ~tm_updater () { };
  
public:
  static tm_updater* instance ();
  
  virtual bool checkInBackground () { return false; }  // non-blocking
  virtual bool checkInForeground () { return false; }  // non-blocking
  virtual bool isRunning () const   { return false; }
   
  virtual bool setAutomaticChecks (bool enable) { (void) enable; return false; }
  virtual time_t lastCheck () const { return 0; }
  virtual bool getCheckInterval () const { return interval; }
  virtual bool setCheckInterval (int hours) { (void) hours; return false; }
  virtual url  getAppcast () const { return appcast; }
  virtual bool setAppcast (url _appcast) { (void) _appcast; return false; }
};


/******************************************************************************
 * Scheme interface
 ******************************************************************************/

bool updater_check_background ();
bool updater_check_foreground ();
bool updater_set_interval (int hours);
bool updater_set_automatic (bool enable);
time_t updater_last_check ();

#endif    // TM_UPDATER_HPP
