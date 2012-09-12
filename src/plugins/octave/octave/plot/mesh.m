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
## @deftypefn {Function File} {} mesh (@var{x}, @var{y}, @var{z})
## Plot a mesh given matrices @var{x}, and @var{y} from @code{meshdom} and
## a matrix @var{z} corresponding to the @var{x} and @var{y} coordinates of
## the mesh.  If @var{x} and @var{y} are vectors, then a typical vertex
## is (@var{x}(j), @var{y}(i), @var{z}(i,j)).  Thus, columns of @var{z}
## correspond to different @var{x} values and rows of @var{z} correspond
## to different @var{y} values.
## @end deftypefn
## @seealso{plot, semilogx, semilogy, loglog, polar, meshgrid, meshdom,
## contour, bar, stairs, gplot, gsplot, replot, xlabel, ylabel, and title}

## Author: jwe

## Modified Jan. 2003 -- Added TeXmacs interface support
## Author: Michael Graffam <mgraffam@mathlab.sunysb.edu>

function mesh (x, y, z)

  ## XXX FIXME XXX -- the plot states should really just be set
  ## temporarily, probably inside an unwind_protect block, but there is
  ## no way to determine their current values.

  P=[2;112;115;58]; #P= "\002ps:"

  if (nargin == 1)
    z = x;
    if (is_matrix (z))
      gset hidden3d;
      gset data style lines;
      gset surface;
      gset nocontour;
      gset noparametric;
      gset view 60, 30, 1, 1
	if (length(getenv("TEXMACS_PATH"))>0)
		gset output '/tmp/tmplot.eps';
	endif
      gsplot (z');
	if (length(getenv("TEXMACS_PATH"))>0)
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
      error ("mesh: argument must be a matrix");
    endif
  elseif (nargin == 3)
    if (is_vector (x) && is_vector (y) && is_matrix (z))
      xlen = length (x);
      ylen = length (y);
      if (xlen == columns (z) && ylen == rows (z))
        if (rows (y) == 1)
          y = y';
        endif
        len = 3 * xlen;
        zz = zeros (ylen, len);
        k = 1;
        for i = 1:3:len
          zz(:,i)   = x(k) * ones (ylen, 1);
          zz(:,i+1) = y;
          zz(:,i+2) = z(:,k);
          k++;
        endfor
        gset hidden3d;
        gset data style lines;
        gset surface;
        gset nocontour;
        gset parametric;
        gset view 60, 30, 1, 1
	if (length(getenv("TEXMACS_PATH"))>0)
		gset output '/tmp/tmplot.eps';
	endif
        gsplot (zz);
        gset noparametric;
	if (length(getenv("TEXMACS_PATH"))>0)
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
        msg = "mesh: rows (z) must be the same as length (y) and";
        msg = sprintf ("%s\ncolumns (z) must be the same as length (x)", msg);
        error (msg);
      endif
    elseif (is_matrix (x) && is_matrix (y) && is_matrix (z))
      xlen = columns (z);
      ylen = rows (z);
      if (xlen == columns (x) && xlen == columns (y) &&
        ylen == rows (x) && ylen == rows(y))
        len = 3 * xlen;
        zz = zeros (ylen, len);
        k = 1;
        for i = 1:3:len
          zz(:,i)   = x(:,k);
          zz(:,i+1) = y(:,k);
          zz(:,i+2) = z(:,k);
          k++;
        endfor
        gset hidden3d;
        gset data style lines;
        gset surface;
        gset nocontour;
        gset parametric;
        gset view 60, 30, 1, 1
	if (length(getenv("TEXMACS_PATH"))>0)
		gset output '/tmp/tmplot.eps';
	endif
        gsplot (zz);
        gset noparametric;
	if (length(getenv("TEXMACS_PATH"))>0)
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
        error ("mesh: x, y, and z must have same dimensions");
      endif
    else
      error ("mesh: x and y must be vectors and z must be a matrix");
    endif
  else
    usage ("mesh (z)");
  endif

endfunction
