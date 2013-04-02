
/******************************************************************************
* MODULE     : raster.hpp
* DESCRIPTION: Raster pictures
* COPYRIGHT  : (C) 2013  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef RASTER_H
#define RASTER_H
#include "raster_operators.hpp"

/******************************************************************************
* Raster class
******************************************************************************/

template<typename C> class raster;
template<typename C> bool is_nil (raster<C> r);

template<typename C>
class raster_rep: public abstract_struct {
public:
  int w, h;
  int ox, oy;
  C* a;

public:
  raster_rep (int w2, int h2, int ox2, int oy2):
    w (w2), h (h2), ox (ox2), oy (oy2), a (NULL) {
      if (w * h != 0) a= tm_new_array<C> (w * h); }
  ~raster_rep () { if (w * h != 0) tm_delete_array (a); }
};

template<typename C> class raster {
  ABSTRACT_NULL_TEMPLATE(raster,C);
  inline raster (int w, int h, int ox, int oy):
    rep (tm_new<raster_rep<C> > (w, h, ox, oy)) { INC_COUNT(rep); }
};
ABSTRACT_NULL_TEMPLATE_CODE(raster,class,C);

template<typename C> void
print (raster<C> r) {
  for (int y=0; y<r->h; y++)
    for (int x=0; x<r->w; x++)
      cout << x << ", " << y << " -> " << r->a[y*r->w+x] << "\n";
}

/******************************************************************************
* Mappers
******************************************************************************/

template<typename Op, typename C> raster<C>
map (raster<C> r) {
  int w= r->w, h= r->h, n= w*h;
  raster<C> ret (w, h, r->ox, r->oy);
  for (int i=0; i<n; i++)
    ret->a[i]= Op::op (r->a[i]);
  return ret;
}

template<typename Op, typename C, typename S> raster<C>
map (raster<C> r1, raster<S> r2) {
  int w= r1->w, h= r1->h, n= w*h;
  ASSERT (r2->w == w && r2->h == h, "sizes don't match");
  ASSERT (r2->ox == r1->ox && r2->oy == r1->oy, "offsets don't match");
  raster<C> ret (w, h, r1->ox, r1->oy);
  for (int i=0; i<n; i++)
    ret->a[i]= Op::op (r1->a[i], r2->a[i]);
  return ret;
}

template<typename C> inline raster<C>
mul_alpha (raster<C> r) { return map<mul_alpha_op> (r); }
template<typename C> inline raster<C>
div_alpha (raster<C> r) { return map<div_alpha_op> (r); }
template<typename C> inline raster<C>
normalize (raster<C> r) { return map<normalize_op> (r); }

template<typename C> inline raster<C>
hypot (raster<C> r1, raster<C> r2) { return map<hypot_op> (r1, r2); }

/******************************************************************************
* Composition
******************************************************************************/

template<composition_mode M, typename C, typename S> void
draw_on (raster<C>& r, S s) {
  int w= r->w, h= r->h, n=w*h;
  for (int i=0; i<n; i++)
    composition_op<M>::set_op (r->a[i], s);
}

template<composition_mode M, typename C, typename S> raster<C>
compose (raster<C> r, S s) {
  int w= r->w, h= r->h, n=w*h;
  raster<C> ret (w, h, r->ox, r->oy);
  for (int i=0; i<n; i++)
    ret->a[i]= composition_op<M>::op (r->a[i], s);
  return ret;
}

template<typename C, typename S> void
draw_on (raster<C>& r, S s, composition_mode mode) {
  switch (mode) {
  case compose_destination:
    draw_on<compose_destination> (r, s);
    break;
  case compose_source:
    draw_on<compose_source> (r, s);
    break;
  case compose_source_over:
    draw_on<compose_source_over> (r, s);
    break;
  case compose_towards_source:
    draw_on<compose_towards_source> (r, s);
    break;
  default:
    break;
  }
}

template<typename C, typename S> raster<C>
compose (raster<C> r, S s, composition_mode mode) {
  raster<C> ret= map<copy_op> (r);
  draw_on (ret, s, mode);
  return ret;
}

template<composition_mode M, typename C, typename S> void
draw_on (raster<C>& dest, raster<S> src, int x, int y) {
  x -= src->ox - dest->ox;
  y -= src->oy - dest->oy;
  int dw= dest->w, dh= dest->h;
  int sw= src ->w, sh= src ->h;
  C* d= dest->a;
  S* s= src ->a;
  int sw2= sw;
  int sh2= sh;
  if (x < 0) { s -= x; sw2 += x; x= 0; }
  if (y < 0) { s -= y * sw; sh2 += y; y= 0; }
  int w = min (sw2, dw - x);
  int h = min (sh2, dh - y);
  if (w <= 0 || h <= 0) return;
  d += y * dw + x;
  for (int y=0; y<h; y++, d += dw, s +=sw)
    for (int x=0; x<w; x++)
      composition_op<M>::set_op (d[x], s[x]);
}

template<typename C, typename S> void
draw_on (raster<C>& r, raster<S> s, int x, int y, composition_mode mode) {
  switch (mode) {
  case compose_destination:
    draw_on<compose_destination> (r, s, x, y);
    break;
  case compose_source:
    draw_on<compose_source> (r, s, x, y);
    break;
  case compose_source_over:
    draw_on<compose_source_over> (r, s, x, y);
    break;
  case compose_towards_source:
    draw_on<compose_towards_source> (r, s, x, y);
    break;
  default:
    break;
  }
}

template<typename C, typename S> raster<C>
empty_join (raster<C> r1, raster<S> r2) {
  int w1 = r1->w , h1 = r1->h;
  int ox1= r1->ox, oy1= r1->oy;
  int w2 = r2->w , h2 = r2->h;
  int ox2= r2->ox, oy2= r2->oy;
  int x1 = min (-ox1, -ox2);
  int y1 = min (-oy1, -oy2);
  int x2 = max (w1-ox1, w2-ox2);
  int y2 = max (h1-oy1, h2-oy2);
  int w  = x2 - x1;
  int h  = y2 - y1;
  raster<C> ret (w, h, -x1, -y1);
  clear (ret);
  return ret;
}

template<composition_mode M, typename C, typename S> raster<C>
compose (raster<C> r1, raster<S> r2) {
  raster<C> ret= empty_join (r1, r2);
  draw_on<compose_source> (ret, r1, 0, 0);
  draw_on<M> (ret, r2, 0, 0);
  return ret;
}

template<typename C, typename S> raster<C>
compose (raster<C> r1, raster<S> r2, composition_mode mode) {
  raster<C> ret= empty_join (r1, r2);
  draw_on (ret, r1, 0, 0, compose_source);
  draw_on (ret, r2, 0, 0, mode);
  return ret;
}

/******************************************************************************
* Convolution and blur
******************************************************************************/

template<typename C> void
clear (raster<C>& r) {
  int w= r->w, h= r->h;
  for (int i=0; i<w*h; i++)
    clear (r->a[i]);
}

template<typename C, typename S> raster<C>
convolute (raster<C> s1, raster<S> s2) {
  if (s1->w * s1->h == 0) return s1;
  ASSERT (s2->w * s2->h != 0, "empty convolution argument");
  int s1w= s1->w, s1h= s1->h, s2w= s2->w, s2h= s2->h;
  int dw= s1w + s2w - 1, dh= s1h + s2h - 1;
  raster<C> d (dw, dh, s1->ox + s2->ox, s1->oy + s2->oy);
  clear (d);
  raster<C> temp= mul_alpha (s1);
  for (int y1=0; y1<s1h; y1++)
    for (int y2=0; y2<s2h; y2++) {
      int o1= y1 * s1w, o2= y2 * s2w, o= (y1 + y2) * dw;
      for (int x1=0; x1<s1w; x1++)
        for (int x2=0; x2<s2w; x2++)
          d->a[o+x1+x2] += temp->a[o1+x1] * s2->a[o2+x2];
    }
  return div_alpha (d);
}

template<typename C> raster<C>
gaussian (int R, double r) {
  int w= 2*R+1, h= 2*R+1;
  raster<C> ret (w, h, R, R);
  double lambda= 1.0 / (2.0 * acos (0.0) * r * r);
  double sq_r= r*r;
  for (int y=0; y<h; y++) {
    double sq_y= (y-R)*(y-R);
    for (int x=0; x<w; x++) {
      double sq_x= (x-R)*(x-R);
      ret->a[y*w+x]= C (lambda * (exp (- (sq_x + sq_y) / sq_r)));
    }
  }
  return ret;
}

template<typename C> raster<C>
blur (raster<C> ras, int R, double r) {
  raster<double> g= gaussian<double> (R, r);
  return convolute (ras, g);
}

/******************************************************************************
* Gravitational effects
******************************************************************************/

template<typename C> raster<C>
gravitation (int R, double expon, bool y_flag) {
  int w= 2*R+1, h= 2*R+1;
  raster<C> ret (w, h, R, R);
  for (int y=0; y<h; y++) {
    double sq_y= (y-R)*(y-R);
    for (int x=0; x<w; x++)
      if (x == R && y == R) ret->a[y*w+x]= 0.0;
      else {
        double sq_x= (x-R)*(x-R);
        double sq_r= sq_x + sq_y;
        double r   = sqrt (sq_r);
        double u   = (y_flag? (y-R): (x-R)) / r;
        ret->a[y*w+x]= u / pow (r, expon);
      }
  }
  return ret;
}

template<typename C> typename C::scalar_type
inner_max (raster<C> r, C s) {
  typedef typename C::scalar_type F;
  int w= r->w, h= r->h;
  F ret= 0.001;
  for (int y=0; y<h; y++)
    for (int x=0; x<w; x++)
      ret= max (ret, inner_max (r->a[y*w+x], s));
  return ret;
}

template<typename C, typename S> raster<C>
divide (raster<C> r, S s) {
  int w= r->w, h= r->h;
  raster<C> ret (w, h, r->ox, r->oy);
  for (int y=0; y<h; y++)
    for (int x=0; x<w; x++)
      ret->a[y*w+x]= r->a[y*w+x] / s;
  return ret;
}

template<typename C> raster<C>
gravitational_outline (raster<C> s, int R, double expon) {
  typedef typename C::scalar_type F;
  raster<F> gravx= gravitation<F> (R, expon, false);
  raster<F> gravy= gravitation<F> (R, expon, true );
  raster<C> convx= convolute (s, gravx);
  raster<C> convy= convolute (s, gravy);
  raster<C> d= hypot (convx, convy);
  F mc= inner_max (d, C (1.0, 1.0, 1.0, 0.0));
  F ma= inner_max (d, C (0.0, 0.0, 0.0, 1.0));
  C sc (mc, mc, mc, ma);
  d= divide (d, sc);
  return normalize (d);
}

/******************************************************************************
* Edge distances
******************************************************************************/

template<typename C> raster<double>
tl_distances (raster<C> r) {
  int w= r->w, h= r->h;
  raster<double> ret (w, h, r->ox, r->oy);
  for (int y=h-1; y>=0; y--)
    for (int x=0; x<w; x++) {
      double a = get_alpha (r->a[y*w+x]);
      double px= (x>0? ret->a[y*w+x-1]: 0.0);
      double py= (y+1<h? ret->a[(y+1)*w+x]: 0.0);
      if (a == 0.0) ret->a[y*w+x]= 0.0;
      else ret->a[y*w+x]= min (px, py) + a;
    }
  return ret;
}

template<typename C> raster<double>
br_distances (raster<C> r) {
  int w= r->w, h= r->h;
  raster<double> ret (w, h, r->ox, r->oy);
  for (int y=0; y<h; y++)
    for (int x=w-1; x>=0; x--) {
      double a= get_alpha (r->a[y*w+x]);
      double px= (x+1<w? ret->a[y*w+x+1]: 0.0);
      double py= (y>0? ret->a[(y-1)*w+x]: 0.0);
      if (a == 0.0) ret->a[y*w+x]= 0.0;
      else ret->a[y*w+x]= min (px, py) + a;
    }
  return ret;
}

#endif // defined RASTER_H