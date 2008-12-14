
/******************************************************************************
* MODULE     : connection.cpp
* DESCRIPTION: TeXmacs connections
* COPYRIGHT  : (C) 2000  Joris van der Hoeven
*******************************************************************************
* When the underlying link of a connection is "alive",
* then the status of the connection is either WAITING_FOR_OUTPUT
* (when waiting for output from the plugin) or WAITING_FOR_INPUT.
* If the underlying link is "dead", then the status is either
* CONNECTION_DEAD (usually) or CONNECTION_DYING (if we are still
* waiting for some residual output from the plugin).
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "connect.hpp"
#include "pipe_link.hpp"
#include "iterator.hpp"
#include "convert.hpp"
#include "scheme.hpp"
#include "resource.hpp"
#include "Generic/input.hpp"
#include "gui.hpp"

static tree connection_retrieve (string name, string session);

/******************************************************************************
* The connection resource
******************************************************************************/

RESOURCE(connection);
struct connection_rep: rep<connection> {
  string  name;          // name of the pipe type
  string  session;       // name of the session
  tm_link ln;            // the underlying link
  int     status;        // status of the connection
  texmacs_input tm_in;   // texmacs input handler for data from child
  texmacs_input tm_err;  // texmacs input handler for errors from child

public:
  connection_rep (string name, string session, tm_link ln);
  string start (bool again);
  void   write (string s);
  void   read (int channel);
  void   stop ();
  void   interrupt ();
};
RESOURCE_CODE(connection);

/******************************************************************************
* Routines for connections
******************************************************************************/

connection_rep::connection_rep (string name2, string session2, tm_link ln2):
  rep<connection> (name2 * "-" * session2),
  name (name2), session (session2), ln (ln2), status (CONNECTION_DEAD),
  tm_in ("output"), tm_err ("error") {}

string
connection_rep::start (bool again) {
  string message;
  if (ln->alive) {
    message= "Continuation of#" * name * "#session";
    status = WAITING_FOR_INPUT;
  }
  else {
    message= ln->start ();
    tm_in  = texmacs_input ("output");
    tm_err = texmacs_input ("error");
    status = WAITING_FOR_OUTPUT;
    if (again && (message == "ok")) {
      beep ();
      (void) connection_retrieve (name, session);
    }
  }
  tm_in ->bof ();
  tm_err->bof ();
  return message;
}

void
connection_rep::write (string s) {
  ln->write (s, LINK_IN);
  tm_in ->bof ();
  tm_err->bof ();
  status= WAITING_FOR_OUTPUT;
}

void
connection_rep::read (int channel) {
  if (channel == LINK_OUT) {
    string s= ln->read (LINK_OUT);
    int i, n= N(s);
    for (i=0; i<n; i++)
      if (tm_in->put (s[i])) {
	status= WAITING_FOR_INPUT;
	if (DEBUG_IO) cout << LF << HRULE;
      }
  }
  else if (channel == LINK_ERR) {
    string s= ln->read (LINK_ERR);
    int i, n= N(s);
    for (i=0; i<n; i++)
      (void) tm_err->put (s[i]);
  }
  if (!ln->alive) {
    tm_in ->eof ();
    tm_err->eof ();
    status= CONNECTION_DEAD;
  }
}

void
connection_rep::stop () {
  if (ln->alive) {
    ln->stop ();
    tm_in ->eof ();
    tm_err->eof ();
    if (status == WAITING_FOR_OUTPUT)
      status= CONNECTION_DYING;
  }
}

void
connection_rep::interrupt () {
  if (ln->alive) {
    ln->interrupt ();
    if (status == WAITING_FOR_OUTPUT)
      status= CONNECTION_DYING;
  }
}

/******************************************************************************
* Handle output from extern applications
******************************************************************************/

void
listen_to_connections () {
  iterator<string> it= iterate (connection::instances);
  while (it->busy()) {
    string name= it->next ();
    connection con (name);
    if ((con->status == WAITING_FOR_INPUT) ||
	(con->status == WAITING_FOR_OUTPUT))
      {
	tree t= connection_handlers (con->name);
	int i, n= N(t);
	if (n>0) con->read (LINK_ERR);
	for (i=0; i<n; i++) {
	  tree doc= con->tm_err->get (t[i][0]->label);
	  if (doc != "") call (t[i][1]->label, doc);
	}
      }
  }
}

/******************************************************************************
* Connection type information
******************************************************************************/

bool
connection_declared (string name) {
  return as_bool (call ("connection-defined?", name));
}

tree
connection_info (string name, string session) {
  return stree_to_tree (call ("connection-info", name, session));
}

tree
connection_handlers (string name) {
  static hashmap<string,tree> handlers (tuple ());
  if (!handlers->contains (name))
    handlers (name)= stree_to_tree (call ("connection-get-handlers", name));
  return handlers[name];
}

/******************************************************************************
* First part of interface (using a specific connection)
******************************************************************************/

string
connection_start (string name, string session, bool again) {
  // cout << "Start " << name << ", " << session << ", " << again << "\n";
  if (!connection_declared (name))
    return "Error: connection " * name * " has not been declared";

  connection con= connection (name * "-" * session);
  if (is_nil (con)) {
    if (DEBUG_VERBOSE)
      cout << "TeXmacs] Starting session '" << session << "'\n";
    tree t= connection_info (name, session);
    if (is_tuple (t, "pipe", 1)) {
      tm_link ln= make_pipe_link (t[1]->label);
      con= new connection_rep (name, session, ln);
    }
    else if (is_tuple (t, "socket", 2)) {
      tm_link ln= make_socket_link (t[1]->label, as_int (t[2]->label));
      con= new connection_rep (name, session, ln);
    }
    else if (is_tuple (t, "dynlink", 3)) {
      tm_link ln=
	make_dynamic_link (t[1]->label, t[2]->label, t[3]->label, session);
      con= new connection_rep (name, session, ln);
    }
  }

  return con->start (again);
}

void
connection_write (string name, string session, string s) {
  // cout << "Write " << name << ", " << session << ", " << s << "\n";
  connection con= connection (name * "-" * session);
  if (is_nil (con)) return;
  con->write (s);
}

void
connection_write (string name, string session, tree t) {
  // cout << "Write " << name << ", " << session << ", " << t << "\n";
  string s= as_string (call ("plugin-serialize", name, tree_to_stree (t)));
  connection_write (name, session, s);
}

tree
connection_read (string name, string session, string channel) {
  // cout << "Read " << name << ", " << session << ", " << channel << "\n";
  connection con= connection (name * "-" * session);
  if (is_nil (con)) return "";
  con->read (LINK_ERR);
  tree t= con->tm_err->get (channel);
  if (t == "") {
    con->read (LINK_OUT);
    t= con->tm_in->get (channel);
  }
  // cout << "Result " << t << "\n";
  return t;
}

void
connection_interrupt (string name, string session) {
  // cout << "Interrupt " << name << ", " << session << "\n";
  connection con= connection (name * "-" * session);
  if (is_nil (con)) return;
  con->interrupt ();
}

void
connection_stop (string name, string session) {
  // cout << "Stop " << name << ", " << session << "\n";
  connection con= connection (name * "-" * session);
  if (is_nil (con)) return;
  con->stop ();
}

int
connection_status (string name, string session) {
  // cout << "Status " << name << ", " << session << " -> ";
  connection con= connection (name * "-" * session);
  if ((!is_nil (con)) && (con->status == CONNECTION_DYING))
    return WAITING_FOR_OUTPUT;
  if (is_nil (con) || (!con->ln->alive)) return CONNECTION_DEAD;
  // cout << con->ln->status << "\n";
  return con->status;
}

/******************************************************************************
* Evaluation interface (using a specific connection)
******************************************************************************/

static connection
connection_get (string name, string session) {
  connection con= connection (name * "-" * session);
  if (is_nil (con)) {
    if (connection_start (name, session, true) != "ok") return con;
    con= connection (name * "-" * session);
  }
  return con;
}

static tree
connection_retrieve (string name, string session) {
  // cout << "Retrieve " << name << ", " << session << "\n";
  connection con= connection (name * "-" * session);
  if (is_nil (con)) return "";
  tree doc (DOCUMENT);
  while (true) {
    listen_to_pipes ();
    tree next= connection_read (name, session);
    if (next == "");
    else if (is_document (next)) doc << A (next);
    else doc << next;
    if (con->status == WAITING_FOR_INPUT) break;
  }
  if (N(doc) == 0) return "";
  // cout << "Retrieved " << doc << "\n";
  return doc;
}

tree
connection_eval (string name, string session, tree t) {
  // cout << "Evaluating " << name << ", " << session << ", " << t << LF;
  connection con= connection_get (name, session);
  if (is_nil (con)) return "";
  connection_write (name, session, t);
  return connection_retrieve (name, session);
}

tree
connection_eval (string name, string session, string s) {
  // cout << "Evaluating " << name << ", " << session << ", " << s << LF;
  connection con= connection_get (name, session);
  if (is_nil (con)) return "";
  connection_write (name, session, s);
  return connection_retrieve (name, session);
}

tree
connection_cmd (string name, string session, string cmd) {
  // cout << "Command " << name << ", " << session << ", " << cmd << LF;
  string s= as_string (call ("format-command", name, cmd));
  tree r= connection_eval (name, session, s);
  if (is_func (r, DOCUMENT, 1)) r= r[0];
  return r;
}
