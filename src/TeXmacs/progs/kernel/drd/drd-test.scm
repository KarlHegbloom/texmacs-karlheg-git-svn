
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : drd-test.scm
;; DESCRIPTION : examples of logical programs
;; COPYRIGHT   : (C) 2003  Joris van der Hoeven
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (kernel drd drd-test)
  (:use (kernel drd drd-rules) (kernel drd drd-query)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Examples of rules
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(drd-rules
  ((sun% Joris Piet))
  ((sun% Piet Opa))
  ((daughter% Geeske Opa))
  ((daughter% Jekke Opa))
  ((child% 'x 'y) (sun% 'x 'y))
  ((child% 'x 'y) (daughter% 'x 'y))
  ((descends% 'x 'y) (child% 'x 'y))
  ((descends% 'x 'z) (child% 'x 'y) (descends% 'y 'z)))

;; (??? (child% 'x Opa))
;; (??? (descends% 'x 'y))
;; (??? (descends% 'x 'y) (daughter% Joleen Opa))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Addional assumptions <-> creating modules
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(drd-rules
  (assume family%) ; added to constraints for all rules below
  ((sun% Joris Piet))
  ((sun% Piet Opa))
  ((daughter% Geeske Opa))
  ((daughter% Jekke Opa))
  ((child% 'x 'y) (sun% 'x 'y))
  ((child% 'x 'y) (daughter% 'x 'y))
  ((descends% 'x 'y) (child% 'x 'y))
  ((descends% 'x 'z) (child% 'x 'y) (descends% 'y 'z)))

;; (??? (child% 'x Opa))
;; (??? (child% 'x Opa) family%)
