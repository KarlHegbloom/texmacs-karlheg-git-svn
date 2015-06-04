
/******************************************************************************
* MODULE     : QTMsockets.cpp
* DESCRIPTION: QT TeXmacs sockets manager
* COPYRIGHT  : (C) 2015 Denis RAUX
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "QTMsockets.hpp"
#include "scheme.hpp"
#include "iterator.hpp"

#ifndef __MINGW32__

#include <errno.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <stdint.h>
#include <fcntl.h>

#define CONNECT ::connect
#define CLOSE(a) close(a)
#define WRITE(a, b, c) ::write(a, b, c)
#define ERRNO errno
#define ERRSOC(a) a 
#else
#define CONNECT wsoc::connect
#define CLOSE(a) wsoc::closesocket(a)
#define WRITE(a, b, c) wsoc::send(a, b, c, 0) 
#define ERRNO wsoc::WSAGetLastError()
#define ERRSOC(a) WSA##a 
#endif

unsigned dbg_cnt;
int socket_link::id= 0;
int socket_basic::count= 0;
#ifdef __MINGW32__
wsoc::WSADATA socket_basic::wsadata;
#endif

string
show_addr (unsigned long a) {
  string s(12);
  s << as_string (a & 0xff) << "."; a >>= 8;
  s << as_string (a & 0xff) << "."; a >>= 8;
  s << as_string (a & 0xff) << "."; a >>= 8;
  s << as_string (a) ;
  return s; 
}

socket_basic::socket_basic (): st (ST_VOID) {
#ifdef __MINGW32__
  if (!count) {
    using namespace wsoc;
    err=wsoc::WSAStartup (MAKEWORD (2,0), &wsadata);
    if (err) {st= ST_WSA; return;}
  }
#endif
  count++;
};

socket_basic::~socket_basic () {
  if (count > 0) --count;
#ifdef __MINGW32__
  if (!count) wsoc::WSACleanup ();
#endif
};

socket_link::socket_link (int s, struct SOCKADDR_IN *addr) {
  id++; sock= s; qsnr= NULL; qsnw= NULL;
  if (st != ST_VOID) return;
  memcpy (&add, addr, sizeof(add));
  qsnr= tm_new <QSocketNotifier> (s, QSocketNotifier::Read);
  qsnw= tm_new <QSocketNotifier> (s, QSocketNotifier::Write);

  if (!qsnr || !qsnw) { err=errno; st=ST_NOTIF; return; }
  QObject::connect (qsnr, SIGNAL(activated(int)),
		    this, SLOT(data_set_ready(int)));
  QObject::connect (qsnw, SIGNAL(activated(int)),
		    this, SLOT(ready_to_send(int)));
  DBG_IO ("Socket Created fd=" << sock);
  st= ST_OK;
}

socket_link::socket_link(string host, u_short port) {
  ++id; qsnr= NULL; qsnw= NULL;
  if (st != ST_VOID) return;
  id++;
  //Create socket
  sock= NMSPC (socket (AF_INET , SOCK_STREAM , 0));
  if (sock == -1) { err= errno; st= ST_SOCKET; return; }

  // getting host
  struct NMSPC (hostent) *hp= NMSPC (gethostbyname (as_charp (host)));
  if (hp == NULL) { err= errno; st= ST_GETHOST; return; }

  //Prepare the sockaddr_in structure
  add.sin_family = AF_INET;
  add.sin_addr.s_addr= *((unsigned long*)(hp->h_addr));
  add.sin_port = NMSPC(htons(port));

  if (CONNECT (sock, (struct SOCKADDR *) &add, sizeof (add))) {
    err= errno; st= ST_CONNECTION; return; }
 #ifndef __MINGW32__
  if (fcntl (sock, F_SETFL, O_NONBLOCK) == -1) {
    err= errno; st= ST_FCNTL; return; }
 #else
  {
    unsigned long flags = -1;
    if (wsoc::ioctlsocket (sock, FIONBIO, &flags) == SOCKET_ERROR) {
      err= errno; st= ST_FCNTL; return; }
  }
#endif
  qsnr= tm_new <QSocketNotifier> (sock, QSocketNotifier::Read);
  qsnw= tm_new <QSocketNotifier> (sock, QSocketNotifier::Write);
 
  if (!qsnr || !qsnw) { err= errno; st= ST_NOTIF; return; }
  QObject::connect (qsnr, SIGNAL (activated(int)),
		    this, SLOT (data_set_ready(int)));
  qsnw->setEnabled (false);
  QObject::connect (qsnw, SIGNAL (activated(int)),
		    this, SLOT (ready_to_send(int)));
  DBG_IO ("Socket Created fd=" << sock);
  st= ST_OK;
}

socket_link::~socket_link() {
  DBG_IO ("Socket closing fd=" << sock);
  if (qsnr) { qsnr->disconnect (SIGNAL(activated(int))); tm_delete (qsnr); }
  if (qsnw) { qsnw->disconnect (SIGNAL(activated(int))); tm_delete (qsnw); }
  if (sock != -1) { CLOSE (sock); sock=-1;}
  st= ST_CLOSED;
}

string
socket_link::start () {
  string ret;
  switch(st) {
  case ST_OK:         return ""; break;
  case ST_VOID:       ret= "Socket not Initialised"; break;
  case ST_SOCKET:     ret= "Error in opening socket"; break;
  case ST_FCNTL:      ret= "Error in setting blocking mode"; break;
  case ST_BIND:       ret= "Error during bind"; break;
  case ST_LISTEN:     ret= "Error during listen"; break;
  case ST_CONNECTION: ret= "Error during connect"; break;
  case ST_GETHOST:    ret= "Error in getting host"; break;
  case ST_NOTIF:      ret= "Error in setting notifier"; break;
  default:            ret= "No error message";
  }
  return ret * " errno:" * strerror(err);
}

string&
socket_link::watch (int channel) {
  static string empty_string= "";
  if (channel == LINK_OUT) return inbuf;
  else return empty_string;
}

string
socket_link::read (int channel) {
  DBG_IO ("Socket read size=" << N(inbuf));
  if (channel == LINK_OUT && N(inbuf)) {
    string r= inbuf;
    inbuf= "";
    return r;
  }
  else return "";
}

void
socket_link::stop () {
  st= ST_HALTED;
  emit disconnection(this);
}

void
socket_link::data_set_ready (int s) {
  char data[2048];
  qsnr->setEnabled(false);
 
  int lgdata = NMSPC (recv (s , data , sizeof(data) , 0));
  DBG_IO ("Socket incomming code=" << lgdata);
  if (lgdata == 0)   {
    DBG_IO ("Client disconnected");   
    stop ();
  }
  else if (lgdata == -1) {
    switch (ERRNO) {
    case ERRSOC (EWOULDBLOCK):
    case ERRSOC (ECONNRESET):
    case ERRSOC (ECONNABORTED): DBG_IO ("Client disconnected"); break;
    default: DBG_IO ("Receiving error :" << ERRNO);
    }
    stop ();
  }
  else {
    inbuf << string (data, lgdata);
    DBG_IO ("Data Received:" << string(data, lgdata));
    tm_wake_up ();
    qsnr->setEnabled (true);
  }
}

void
socket_link::ready_to_send (int s) {
#ifdef __MINGW32__
  using namespace wsoc;
#endif
  qsnw->setEnabled (false);
  int sz= N (outbuf);
  if (sz) {
    char  *buf= as_charp (outbuf);
    int ret= WRITE (s, buf, sz);
    DBG_IO ("Socket outcomming code=" << ret);
    if (ret >0) {
      if (ret == sz) outbuf= ""; else outbuf= outbuf (ret,sz);
      sz -= ret;
      if (sz) qsnw->setEnabled (true);
    }
    else if (ret <0) {
      DBG_IO ("Sending error:" << strerror(errno));
      stop ();
    }
    else qsnw->setEnabled (true);
  }
}

void
socket_link::listen (int msecs) {
#ifdef __MINGW32__
  using namespace wsoc;
#endif
  if (!alive ()) return;
  ready_to_send (sock); // do some writing if any pending
  fd_set rfds;
  FD_ZERO (&rfds);
  FD_SET (sock, &rfds);
  struct timeval tv;
  tv.tv_sec  = msecs / 1000;
  tv.tv_usec = 1000 * (msecs % 1000);
  int nr= select (sock+1, &rfds, NULL, NULL, &tv);
  if (nr ==1) data_set_ready (sock); //collect data
  DBG_IO ("listen result :" << nr);
  if (nr == -1) stop();
}

void
socket_link::write (string s, int channel) {
  DBG_IO ("Socket write size=" << N(s));
  if ((!alive ()) || (channel != LINK_IN) || !N(s)) return;
  outbuf << s;
  qsnw->setEnabled(true);
}

socket_server::socket_server (in_addr_t add, u_short port) {
  struct SOCKADDR_IN server;

  //Create socket
  sock= NMSPC (socket (AF_INET , SOCK_STREAM , 0));
  if (sock == -1) { err= errno; st= ST_SOCKET; return;}

#ifndef __MINGW32__
  if (fcntl (sock, F_SETFL, O_NONBLOCK) == -1) {
    err= errno; st= ST_FCNTL; return; }
#else 
 {
   unsigned long flags = -1;
   if (wsoc::ioctlsocket (sock, FIONBIO, &flags) == SOCKET_ERROR) {
     err= errno; st= ST_FCNTL; return; }
 }
#endif
 
  //Prepare the sockaddr_in structure
  server.sin_family= AF_INET;
  server.sin_addr.s_addr= add;
  server.sin_port= NMSPC(htons(port));

  if (NMSPC (bind (sock, (struct SOCKADDR *)&server, sizeof(server)))) {
    err=errno; st=ST_BIND; return; }
  if (NMSPC(listen (sock , 3))) { err= errno; st= ST_LISTEN; return; }
  qsnc= new QSocketNotifier (sock, QSocketNotifier::Read);

  QObject::connect(qsnc, SIGNAL(activated(int)), this, SLOT(connection(int)));
  DBG_IO ("wait for connection");
}

void
socket_server::connection (int s) {
  int sclt; socket_link *clt;
  struct SOCKADDR_IN cltadd;
  socklen_t sz= sizeof (cltadd);
  if (!qsnc->isEnabled ()) return;
   //accept connection from an incoming client
  sclt= NMSPC (accept (s, (SOCKADDR*) &cltadd, &sz));
  if (sclt > 0) {
    clt= tm_new <socket_link> (sclt,  &cltadd);
    if (clt) {
      if (clt->alive ()) {
        connect (clt, SIGNAL (disconnection(class socket_link*)), this,
		 SLOT (disconnection (class socket_link*)));
        clts->insert (clt);
        call ("server-add", object (clt->getid ()));
        DBG_IO ("Client Connected " << show_addr (cltadd.sin_addr.s_addr)
		<< " id:" << clt->getid ());
      }
      else tm_delete (clt);
    }
  }
  else switch (ERRNO) {
    case ERRSOC (EWOULDBLOCK):
    case ERRSOC (ECONNABORTED): break;
    default: err=errno; qsnc->setEnabled (false); st=ST_CONNECTION;
  }
}

void 
socket_server::disconnection(class socket_link* clt) {
  call ("server-remove", object (clt->getid()));
  clts->remove(clt);
  tm_delete(clt);
}

string
socket_server::read (IdClt id) {
  class socket_link *clt= find_client (id);
  if (!clt) return "";
  bool success;
  string back= clt->read_packet (LINK_OUT, 0, success);
  return back;
}

void
socket_server::write (IdClt id, string s) {
  class socket_link *clt= find_client (id);
  if (clt) clt->write_packet(s, LINK_IN);
}

class socket_link *
socket_server::find_client(IdClt id) {
  iterator<class socket_link*> it= iterate (clts);
  while (it->busy ()) {
    class socket_link* clt= it->next ();
    if (clt->getid() == id) return clt;
  }
  DBG_IO ("Client not found Id " << id);
  return NULL;
}

socket_server::~socket_server() {
  iterator<class socket_link*> it= iterate (clts);
  while (it->busy ()) {
    class socket_link* clt= it->next ();
    disconnection (clt);
  }
}

string
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
