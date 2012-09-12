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
## Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
## 02110-1301, USA.

## -*- texinfo -*-
## @deftypefn {Function File} {} __plt2mv__ (@var{x}, @var{y}, @var{fmt})
## @end deftypefn

## Author: jwe

## Modified: Jan. 2003 -- Added TeXmacs interface support
## Author: Michael Graffam <mgraffam@mathlab.sunysb.edu>

function __plt2mv__ (x, y, fmt)

  if (nargin < 2 || nargin > 3)
    msg = sprintf ("__plt2mv__ (x, y)\n");
    msg = sprintf ("%s              __plt2mv__ (x, y, fmt)", msg);
    usage (msg);
  elseif (nargin == 2 || fmt == "")
    fmt = " ";  ## Yes, this is intentionally not an empty string!
  endif

  if (length(getenv("TEXMACS_PATH"))>0)
	gset output '/tmp/tmplot.eps';
  endif

  [x_nr, x_nc] = size (x);
  [y_nr, y_nc] = size (y);

  if (y_nr == 1)
    y = y';
    tmp = y_nr;
    y_nr = y_nc;
    y_nc = tmp;
  endif

  if (x_nr == y_nr)
    1;
  elseif (x_nc == y_nr)
    x = x';
    tmp = x_nr;
    x_nr = x_nc;
    x_nc = tmp;
  else
    error ("__plt2mv__: matrix dimensions must match");
  endif

  k = 1;
  fmt_nr = rows (fmt);
  if (x_nc > 0)
    tmp = [x, y];
    cmd = sprintf ("gplot tmp(:,%d:%d:%d) %s", 1, x_nc, x_nc+1,
                   deblank (fmt (k, :)));
    if (k < fmt_nr)
      k++;
    endif
    for i = 2:x_nc
      cmd = sprintf ("%s, tmp(:,%d:%d:%d) %s", cmd, i, x_nc-i+1, x_nc+1,
                     deblank (fmt (k, :)));
      if (k < fmt_nr)
        k++;
      endif
    endfor
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
    error ("__plt2mv__: arguments must be a matrices");
  endif

endfunction
