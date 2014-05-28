
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : program-drd.scm
;; DESCRIPTION : data relation definitions for program tags
;; COPYRIGHT   : (C) 2009  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (dynamic program-drd)
  (:use (dynamic dynamic-drd)))

;; fold <-> unfold toggles

(define-toggle folded-prog-io unfolded-prog-io)
(define-toggle folded-prog-io-math unfolded-prog-io-math)
