## Copyright (C) 1996, 1997 John W. Eaton
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2, or (at your option)
## any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, write to the Free
## Software Foundation, 59 Temple Place - Suite 330, Boston, MA
## 02111-1307, USA.

## -*- texinfo -*-
## @deftypefn {Function File} {} __plt2ss__ (@var{x}, @var{y}, @var{fmt})
## @end deftypefn

## Author: jwe

## Modified Jan. 2003 -- Added TeXmacs interface support
## Author: Michael Graffam <mgraffam@mathlab.sunysb.edu>

function __plt2ss__ (x, y, fmt)

  if (nargin < 2 || nargin > 3)
    msg = sprintf ("__plt2ss__ (x, y)");
    msg = sprintf ("%s              __plt2ss__ (x, y, fmt)", msg);
    usage (msg);
  elseif (nargin == 2)
    fmt = "";
  elseif (rows (fmt) > 1)
    fmt = fmt (1, :);
  endif

  [x_nr, x_nc] = size (x);
  [y_nr, y_nc] = size (y);

  if (length(getenv("TEXMACS_PATH"))>0)
	gset output '/tmp/tmplot.eps';
  endif

  if (x_nr == 1 && x_nr == y_nr && x_nc == 1 && x_nc == y_nc)
    tmp = [x, y];
    cmd = sprintf ("gplot tmp %s", fmt);
    eval (cmd);
          if (length(getenv("TEXMACS_PATH"))>0)
                 P=[2;112;115;58]; #P= "\002ps:"
                g=fopen("/tmp/tmplot.eps");
                while (g==-1)
                        sleep(1);
                        g=fopen("/tmp/tmplot.eps");
                endwhile
                while (!feof(g))
                        f=fread(g,2048);
                        if (length(f))
                                P=[P;f];
                        endif
                endwhile
                fclose(g);
                P=[P;5];
                disp(sprintf("%cverbatim:\n",2));
                disp(setstr(P'));
                system("rm /tmp/tmplot.eps");
        endif
  else
    error ("__plt2ss__: arguments must be scalars");
  endif

endfunction
