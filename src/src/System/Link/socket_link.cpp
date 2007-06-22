
/******************************************************************************
* MODULE     : socket_link.cpp
* DESCRIPTION: TeXmacs links by sockets
* COPYRIGHT  : (C) 2003  Joris van der Hoeven
* THANKS     : Beej's Guide to Network Programming has been helpful
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "socket_link.hpp"
#include "sys_utils.hpp"
#include "hashset.hpp"
#include "iterator.hpp"
#include "timer.hpp"
#include "Scheme/object.hpp"
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/wait.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <errno.h>
#ifdef OS_WIN32
#include <sys/misc.h>
#endif

hashset<pointer> socket_link_set;

/******************************************************************************
* Constructors and destructors for socket_links
******************************************************************************/

socket_link_rep::socket_link_rep (string host2, int port2, int type2, int fd):
  host (host2), port (port2), type (type2)
{
  socket_link_set->insert ((pointer) this);
  io     = fd;
  outbuf = "";
  alive  = (fd != -1);
  if (type == SOCKET_SERVER) call ("server-add", object (io));
  else if (type == SOCKET_CLIENT) call ("client-add");
}

socket_link_rep::~socket_link_rep () {
  stop ();
  socket_link_set->remove ((pointer) this);
}

tm_link
make_socket_link (string host, int port, int type, int fd) {
  return new socket_link_rep (host, port, type, fd);
}

tm_link
find_socket_link (int fd) {
  iterator<pointer> it= iterate (socket_link_set);
  while (it->busy()) {
    socket_link_rep* con= (socket_link_rep*) it->next();
    if (con->io == fd && con->alive) return tm_link (con);
  }
  return tm_link ();
}

/******************************************************************************
* Routines for socket_links
******************************************************************************/

string
socket_link_rep::start () {
  if (alive) return "busy";
  if (DEBUG_AUTO)
    cout << "TeXmacs] Connecting to '" << host << ":" << port << "'\n";
  
  // getting host
  char* _host= as_charp (host);
#ifdef OS_WIN32
  struct hostent *hp = SOCKET_gethostbyname (_host);
#else
  struct hostent *hp = gethostbyname (_host);
#endif
  delete[] _host;
  if (hp == NULL) return "Error: no connection for '" * host * "'";

  // creating socket
#ifdef OS_WIN32
  io= SOCKET_socket (AF_INET, SOCK_STREAM, 0);
#else
  io= socket (AF_INET, SOCK_STREAM, 0);
#endif
  if (io < 0) return "Error: socket could not be created";

  // connecting to socket
  struct sockaddr_in insock;
  string where= host * ":" * as_string (port);
  memset ((char*) &insock, 0, sizeof (insock));
  insock.sin_family = AF_INET;
#ifdef OS_WIN32
  insock.sin_port = SOCKET_htons ((unsigned short) port);
#else
  insock.sin_port = htons ((unsigned short) port);
#endif
  memcpy ((char*) &insock.sin_addr, hp->h_addr, hp->h_length);
#ifdef OS_WIN32
  if (SOCKET_connect (io, (struct sockaddr*) &insock, sizeof (insock)) < 0)
#else
  if (connect (io, (struct sockaddr*) &insock, sizeof (insock)) < 0)
#endif
    return "Error: refused connection to '" * where * "'";

  // testing whether it works
  int flags = O_NONBLOCK;
#ifdef OS_WIN32
  if (SOCKET_fcntl (io, F_SETFL, flags) < 0)
#else
  if (fcntl (io, F_SETFL, flags) < 0)
#endif
    return "Error: non working connection to '" * where * "'";
  alive= true;
  return "ok";
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

static int
send_all (int s, char *buf, int *len) {
  int total= 0;          // how many bytes we've sent
  int bytes_left= *len;  // how many we have left to send
  int n= 0;

  while (total < *len) {
    n= send (s, buf + total, bytes_left, 0);
    if (n == -1) break;
    total += n;
    bytes_left -= n;
  }

  *len= total;
  return n==-1? -1: 0;
} 


void
socket_link_rep::write (string s, int channel) {
  if ((!alive) || (channel != LINK_IN)) return;
  if (DEBUG_IO) cout << "---> " << debug_io_string (s) << "\n";
  int len= N(s);
  if (send_all (io, &(s[0]), &len) == -1) {
    cerr << "TeXmacs] write to '" << host << ":" << port << "' failed\n";
    stop ();
  }
}

void
socket_link_rep::feed (int channel) {
  if ((!alive) || (channel != LINK_OUT)) return;
  char tempout[1024];
  int r= recv (io, tempout, 1024, 0);
  if (r <= 0) {
    if (r == 0) cout << "TeXmacs] '" << host << ":" << port << "' hung up\n";
    else cerr << "TeXmacs] read failed from '" << host << ":" << port << "'\n";
    stop ();
  }
  else if (r != 0) {
    if (DEBUG_IO) cout << debug_io_string (string (tempout, r));
    outbuf << string (tempout, r);
  }
}

string&
socket_link_rep::watch (int channel) {
  static string empty_string= "";
  if (channel == LINK_OUT) return outbuf;
  else return empty_string;
}

string
socket_link_rep::read (int channel) {
  if (channel == LINK_OUT) {
    string r= outbuf;
    outbuf= "";
    return r;
  }
  else return "";
}

void
socket_link_rep::listen (int msecs) {
  if (!alive) return;
  fd_set rfds;
  FD_ZERO (&rfds);
  FD_SET (io, &rfds);
  struct timeval tv;
  tv.tv_sec  = msecs / 1000;
  tv.tv_usec = 1000 * (msecs % 1000);
  int nr= select (io+1, &rfds, NULL, NULL, &tv);
  if (nr != 0 && FD_ISSET (io, &rfds)) feed (LINK_OUT);
}

void
socket_link_rep::interrupt () {
}

void
socket_link_rep::stop () {
  if (!alive) return;
  if (type == SOCKET_SERVER) call ("server-remove", object (io));
  else if (type == SOCKET_CLIENT) call ("client-remove");
  close (io);
  io= -1;
  alive= false;
  wait (NULL);
}

/******************************************************************************
* Listen to all active sockets (may be optimized for speed)
******************************************************************************/

void
listen_to_sockets () {
  while (true) {
    fd_set rfds;
    FD_ZERO (&rfds);
    int max_fd= 0;
    iterator<pointer> it= iterate (socket_link_set);
    while (it->busy()) {
      socket_link_rep* con= (socket_link_rep*) it->next();
      if (con->alive) {
	FD_SET (con->io, &rfds);
	if (con->io >= max_fd) max_fd= con->io+1;
      }
    }
    if (max_fd == 0) break;

    struct timeval tv;
    tv.tv_sec  = 0;
    tv.tv_usec = 0;
    int nr= select (max_fd, &rfds, NULL, NULL, &tv);
    if (nr==0) break;

    it= iterate (socket_link_set);
    while (it->busy()) {
      socket_link_rep* con= (socket_link_rep*) it->next();
      if (con->alive && FD_ISSET (con->io, &rfds)) con->feed (LINK_OUT);
    }
  }
}
