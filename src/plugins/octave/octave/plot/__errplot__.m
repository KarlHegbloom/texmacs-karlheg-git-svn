## Copyright (C) 2000, Teemu Ikonen
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
## @deftypefn {Function File} {} __errplot__ (@var{args})
## Really plot errorbar plots. User interface in function errorbar.
##
## @example
## __errplot__ (@var{arg1}, @var{arg2}, ..., @var{fmt})
## @end example
##
## @end deftypefn
## @seealso{semilogx, semilogy, loglog, polar, mesh, contour, __pltopt__
## bar, stairs, errorbar, gplot, gsplot, replot, xlabel, ylabel, and title}

## Created: 18.7.2000
## Author: Teemu Ikonen <tpikonen@pcu.helsinki.fi>
## Keywords: errorbar, plotting

## Modified Feb. 2003 -- Added TeXmacs interface support
## Author: Michael Graffam <mgraffam@mathlab.sunysb.edu>

function __errplot__ (...)

  if (nargin < 3) # atleast two data arguments needed
    usage ("__errplot__ (arg1, ..., fmt)");
  endif
 
  fstr = " ";

  ndata = 0;

  if (length(getenv("TEXMACS_PATH"))>0)
	gset output '/tmp/tmplot.eps';
  endif

  while (nargin--)
    a = va_arg ();
    if (! isstr (a))
      ndata++;
      eval (sprintf ("arg%d = a;", ndata));
    else
      fstr = a;
    endif
  endwhile

  fmt = __pltopt__ ("__errplot__", fstr);

  nplots = size (arg1, 2);
  len = size (arg1, 1);

  if (ndata == 2)
    for i = 1:nplots,
      tmp = [(1:len)', arg1(:,i), arg2(:,i)];
      cmd = sprintf ("gplot tmp %s", fmt(min(i, rows(fmt)), :) );
      eval (cmd);
    end
  elseif (ndata == 3)
    for i = 1:nplots,
      tstr = "tmp =[arg1(:,i)";
      for j = 2:ndata,
	tstr = [tstr, sprintf(", arg%d(:,i)", j)];
      endfor
      tstr = [tstr, "];"];
      eval (tstr);
      cmd = sprintf ("gplot tmp %s", fmt(min(i, rows(fmt)), :) );
      eval (cmd);
    endfor
  elseif (ndata == 4)
    for i = 1:nplots, # this is getting ugly
      if (index (fmt, "boxxy") || index (fmt, "xyerr"))
	tstr = "tmp = [arg1(:,i), arg2(:,i), arg3(:,i), arg4(:,i)];";
      elseif (index (fmt, "xerr"))
	tstr = "tmp = [arg1(:,i), arg2(:,i), arg1(:,i)-arg3(:,i), arg1(:,i)+arg4(:,i)];";
      else
	tstr = "tmp = [arg1(:,i), arg2(:,i), arg2(:,i)-arg3(:,i), arg2(:,i)+arg4(:,i)];";
      endif
      eval (tstr);
      cmd = sprintf ("gplot tmp %s", fmt(min(i, rows(fmt)), :) );
      eval (cmd);
    endfor
  elseif (ndata == 6)
    for i = 1:nplots,
      tstr = "tmp = [arg1(:,i), arg2(:,i), arg1(:,i)-arg3(:,i), arg1(:,i)+arg4(:,i), arg2(:,i)-arg5(:,i), arg2(:,i)+arg6(:,i)];";
      eval (tstr);
      cmd = sprintf ("gplot tmp %s", fmt(min(i, rows(fmt)), :) );
      eval (cmd);
    endfor
  else
    for i = 1:nplots,
      tstr = "tmp = [arg1(:,i)";
      for j = 2:ndata,
	tstr = [tstr, sprintf(", arg%d(:,i)", j)];
      endfor
      tstr = [tstr, "];"];
      eval (tstr);
      cmd = sprintf ("gplot tmp %s", fmt(min(i, rows(fmt)), :) );
      eval (cmd);
    endfor
  endif

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

  ## if (ndata == 2)
  ##   for i = 1:nplots,
  ##     tmp = [(1:len)', arg1(:,i), arg2(:,i)];
  ##     cmd = sprintf ("gplot tmp %s", fmt(min(i, rows(fmt)), :) );
  ##     eval (cmd);
  ##   endfor
  ## else
  ##   for i = 1:nplots,
  ##     tstr = "tmp =[arg1(:,i)";
  ##     for j = 2:ndata,
  ##       tstr = [tstr, sprintf(", arg%d(:,i)", j)];
  ##     endif
  ##     tstr = [tstr, "];"];
  ##     eval (tstr);
  ##     cmd = sprintf ("gplot tmp %s", fmt(min(i, rows(fmt)), :) );
  ##     eval (cmd);
  ##   endfor
  ## endif

endfunction
