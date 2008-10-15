
/******************************************************************************
* MODULE     : pipe_link.cpp
* DESCRIPTION: TeXmacs links by pipes
* COPYRIGHT  : (C) 2000  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "pipe_link.hpp"
#include "sys_utils.hpp"
#include "hashset.hpp"
#include "iterator.hpp"
#include "timer.hpp"
#include <stdio.h>
#include <string.h>
#ifdef Q_WS_WIN
#ifdef OS_WIN32
#include <sys/pipe.h>
#endif
#else
#include <unistd.h>
#include <signal.h>
#include <sys/wait.h>
#endif
#ifndef __APPLE__
#include <malloc.h>
#endif

hashset<pointer> pipe_link_set;

/******************************************************************************
* Constructors and destructors for pipe_links
******************************************************************************/

pipe_link_rep::pipe_link_rep (string cmd2): cmd (cmd2) {
  pipe_link_set->insert ((pointer) this);
#ifdef OS_WIN32
  memset(&conn, 0, sizeof(PIPE_CONN));
#else
  in     = pp_in [0]= pp_in [1]= -1;
  out    = pp_out[0]= pp_out[1]= -1;
  err    = pp_err[0]= pp_err[1]= -1;
#endif
  outbuf = "";
  errbuf = "";
  alive  = false;
}

pipe_link_rep::~pipe_link_rep () {
  stop ();
  pipe_link_set->remove ((pointer) this);
}

tm_link
make_pipe_link (string cmd) {
  return new pipe_link_rep (cmd);
}

/******************************************************************************
* Routines for pipe_links
******************************************************************************/

#ifndef OS_WIN32
void
execute_shell (string s) {
  char *_s= as_charp (s);
  char *argv[4];
  argv[0] = const_cast<char*> ("sh");
  argv[1] = const_cast<char*> ("-c");
  argv[2] = _s;
  argv[3] = NULL;
  execve ("/bin/sh", argv, environ);
  delete[] _s;
}
#endif

string
pipe_link_rep::start () {
#ifndef Q_WS_WIN
  if (alive) return "busy";
  if (DEBUG_AUTO) cout << "TeXmacs] Launching '" << cmd << "'\n";

#ifdef OS_WIN32
  char *cmdString;
  bool success;
  cmdString = as_charp(cmd);
  success = PIPE_Create(cmdString, &conn);
  delete [] cmdString;
  if (!success) return "Error: Could not create pipe";
  else {
#else
  pipe (pp_in );
  pipe (pp_out);
  pipe (pp_err);
  pid= fork ();
  if (pid==0) { // the child
    setsid();
    close (pp_in  [OUT]);
    close (pp_out [IN ]);
    close (pp_err [IN ]);
    dup2  (pp_in  [IN ], STDIN );
    close (pp_in  [IN ]);
    dup2  (pp_out [OUT], STDOUT);
    close (pp_out [OUT]);
    dup2  (pp_err [OUT], STDERR);
    close (pp_err [OUT]);

    execute_shell (cmd);
    exit (127);
    // exit (system (cmd) != 0);
  }
  else { // the main process
    in = pp_in  [OUT];
    close (pp_in [IN]);
    out= pp_out [IN ];
    close (pp_out [OUT]);
    err= pp_err [IN ];
    close (pp_err [OUT]);
#endif

    alive= true;
    if (/* !banner */ true) return "ok";
    else {
      int r;
      char outbuf[1024];
#ifdef OS_WIN32
      r= PIPE_ReadStdout(&conn, outbuf, 1024);
#else
      r= ::read (out, outbuf, 1024);
#endif
      if (r == 1 && outbuf[0] == TERMCHAR) return "ok";
      alive= false;
#ifdef OS_WIN32
      PIPE_Close(&conn);
#else
      if (-1 != killpg(pid,SIGTERM)) {
	sleep(2);
	killpg(pid,SIGKILL);
      }
#endif
      wait (NULL);
      if (r == -1) return "Error: the application does not reply";
      else
	return "Error: the application did not send its usual startup banner";
    }
  }
#else
  return "Error: pipes not implemented";
#endif
}

static string
debug_io_string (string s) {
  int i, n= N(s);
  string r;
  for (i=0; i<n; i++) {
    unsigned char c= (unsigned char) s[i];
    if (c == DATA_BEGIN) r << "[BEGIN]";
    else if (c == DATA_END) r << "[END]";
    else if (c == DATA_COMMAND) r << "[COMMAND]";
    else if (c == DATA_ESCAPE) r << "[ESCAPE]";
    else r << s[i];
  }
  return r;
}

void
pipe_link_rep::write (string s, int channel) {
#ifndef Q_WS_WIN
  if ((!alive) || (channel != LINK_IN)) return;
  if (DEBUG_IO) cout << "[INPUT]" << debug_io_string (s);
  char* _s= as_charp (s);
#ifdef OS_WIN32
  PIPE_WriteStdin(&conn, _s, N(s));
#else
  ::write (in, _s, N(s));
#endif
  delete[] _s;
#endif
}

void
pipe_link_rep::feed (int channel) {
#ifndef Q_WS_WIN
  if ((!alive) || ((channel != LINK_OUT) && (channel != LINK_ERR))) return;
  int r;
  char tempout[1024];
#ifdef OS_WIN32
  if (channel == LINK_OUT) r = PIPE_ReadStdout(&conn, tempout, 1024);
  else r = PIPE_ReadStderr(&conn, tempout, 1024);
#else
  if (channel == LINK_OUT) r = ::read (out, tempout, 1024);
  else r = ::read (err, tempout, 1024);
#endif
  if (r == -1) {
    cerr << "TeXmacs] read failed for#'" << cmd << "'\n";
    wait (NULL);
  }
  else if (r == 0) {
#ifdef OS_WIN32
    PIPE_Close(&conn);
#else
    if (-1 != killpg(pid,SIGTERM)) {
      sleep(2);
      killpg(pid,SIGKILL);
    }
#endif
    alive= false;
  }
  else {
    if (DEBUG_IO) cout << debug_io_string (string (tempout, r));
    if (channel == LINK_OUT) outbuf << string (tempout, r);
    else errbuf << string (tempout, r);
  }
#endif
}

string&
pipe_link_rep::watch (int channel) {
  static string empty_string= "";
  if (channel == LINK_OUT) return outbuf;
  else if (channel == LINK_ERR) return errbuf;
  else return empty_string;
}

string
pipe_link_rep::read (int channel) {
  if (channel == LINK_OUT) {
    string r= outbuf;
    outbuf= "";
    return r;
  }
  else if (channel == LINK_ERR) {
    string r= errbuf;
    errbuf= "";
    return r;
  }
  else return string("");
}

void
pipe_link_rep::listen (int msecs) {
  if (!alive) return;
  int wait_until= texmacs_time () + msecs;
  while ((outbuf == "") && (errbuf == "")) {
    listen_to_pipes (); // FIXME: should listen more specifically
    if (texmacs_time () - wait_until > 0) break;
  }
}

void
pipe_link_rep::interrupt () {
#ifndef Q_WS_WIN
  if (!alive) return;
#ifdef OS_WIN32
  PIPE_Close(&conn);
#else
  killpg (pid, SIGINT);
#endif
#endif
}

void
pipe_link_rep::stop () {
#ifndef Q_WS_WIN
  if (!alive) return;
#ifdef OS_WIN32
  PIPE_Close(&conn);
#else
  if (-1 != killpg(pid,SIGTERM)) {
    sleep(2);
    killpg(pid,SIGKILL);
  }
  alive= false;
  close (in);
#endif
  alive= false;
  wait (NULL);
#endif
}

/******************************************************************************
* Listen to all active pipes (may be optimized for speed)
******************************************************************************/

void
listen_to_pipes () {
#ifndef Q_WS_WIN
#ifdef OS_WIN32
  while (true) {
    int max_fd = 0;
    iterator<pointer> it = iterate(pipe_link_set);
    while (it->busy()) {
      pipe_link_rep* con = (pipe_link_rep*) it->next();
      if (con->alive) {
	if (con->conn.isValid && PIPE_CheckStdout (&con->conn)) max_fd++;
	if (con->conn.isValid && PIPE_CheckStderr (&con->conn)) max_fd++;
      }
    }
    if(max_fd == 0) break;
	
    it= iterate (pipe_link_set);
    while (it->busy()) {
      pipe_link_rep* con= (pipe_link_rep*) it->next();
      if (con->alive && PIPE_CheckStdout (&con->conn)) con->feed (LINK_OUT);
      if (con->alive && PIPE_CheckStderr (&con->conn)) con->feed (LINK_ERR);
    }
  }
#else
  while (true) {
    fd_set rfds;
    FD_ZERO (&rfds);
    int max_fd= 0;
    iterator<pointer> it= iterate (pipe_link_set);
    while (it->busy()) {
      pipe_link_rep* con= (pipe_link_rep*) it->next();
      if (con->alive) {
	FD_SET (con->out, &rfds);
	FD_SET (con->err, &rfds);
	if (con->out >= max_fd) max_fd= con->out+1;
	if (con->err >= max_fd) max_fd= con->err+1;
      }
    }
    if (max_fd == 0) break;

    struct timeval tv;
    tv.tv_sec  = 0;
    tv.tv_usec = 0;
    int nr= select (max_fd, &rfds, NULL, NULL, &tv);
    if (nr==0) break;

    it= iterate (pipe_link_set);
    while (it->busy()) {
      pipe_link_rep* con= (pipe_link_rep*) it->next();
      if (con->alive && FD_ISSET (con->out, &rfds)) con->feed (LINK_OUT);
      if (con->alive && FD_ISSET (con->err, &rfds)) con->feed (LINK_ERR);
    }
  }
#endif
#endif
}

/******************************************************************************
* Emergency exit for all pipes
******************************************************************************/

void
close_all_pipes () {
#ifndef Q_WS_WIN
  iterator<pointer> it= iterate (pipe_link_set);
  while (it->busy()) {
    pipe_link_rep* con= (pipe_link_rep*) it->next();
    if (con->alive) {
#ifdef OS_WIN32
      PIPE_Close (&con->conn);
#else
      if (-1 != killpg(con->pid,SIGTERM)) {
	sleep(2);
	killpg(con->pid,SIGKILL);
      }
#endif
      con->alive= false;
    }
  }
#endif
}
