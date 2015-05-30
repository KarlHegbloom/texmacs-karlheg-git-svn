
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : cpp-wrap.scm
;; DESCRIPTION : wrappers around C++ functions
;; COPYRIGHT   : (C) 2015  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (utils library cpp-wrap))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Inserting general tags
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (insert t) (cpp-insert t))
(tm-define (insert-go-to t p) (cpp-insert-go-to t p))

(tm-define (make tag . opt-arity)
  (if (null? opt-arity)
      (cpp-make tag)
      (cpp-make-arity tag (car opt-arity))))

(tm-define (make-with var val) (cpp-make-with var val))
(tm-define (make-hybrid) (cpp-make-hybrid))

(tm-define (make-rigid) (cpp-make-rigid))
(tm-define (make-lprime s) (cpp-make-lprime s))
(tm-define (make-rprime s) (cpp-make-rprime s))
(tm-define (make-below) (cpp-make-below))
(tm-define (make-above) (cpp-make-above))
(tm-define (make-script r? sup?) (cpp-make-script r? sup?))
(tm-define (make-fraction) (cpp-make-fraction))
(tm-define (make-sqrt) (cpp-make-sqrt))
(tm-define (make-var-sqrt) (cpp-make-var-sqrt))
(tm-define (make-wide s) (cpp-make-wide s))
(tm-define (make-wide-under s) (cpp-make-wide-under s))
(tm-define (make-neg) (cpp-make-neg))
(tm-define (make-tree) (cpp-make-tree))
