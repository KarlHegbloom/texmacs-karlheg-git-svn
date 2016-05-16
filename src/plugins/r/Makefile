
###############################################################################
# MODULE     : Make file for GNU R plugin
# COPYRIGHT  : (C) 1999-2008  Joris van der Hoeven
###############################################################################
# This software falls under the GNU general public license version 3 or later.
# It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
# in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
###############################################################################

CC = gcc
RM = rm -f

D_HAVE_PTY_H:=$(shell if [ -f /usr/include/pty.h ]; then echo -DHAVE_PTY_H=1; fi)


all: bin/tm_r

bin/tm_r: src/tm_r.c
	$(CC) $(CPPFLAGS) $(CFLAGS) $(D_HAVE_PTY_H) -I../../src/System src/tm_r.c -o bin/tm_r -lutil

clean:
	$(RM) bin/tm_r
