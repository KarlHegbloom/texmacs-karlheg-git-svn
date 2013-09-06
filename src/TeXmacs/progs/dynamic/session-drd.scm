
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : session-drd.scm
;; DESCRIPTION : data relation definitions for session tags
;; COPYRIGHT   : (C) 2009  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (texmacs-module (dynamic session-drd)
;;   (:use (dynamic dynamic-drd)))

(define-module (dynamic session-drd)
  :use-module (texmacs-core))

(use-modules (dynamic dynamic-drd))

;; fold <-> unfold toggles

(define-toggle folded-io unfolded-io)
(define-toggle folded-io-math unfolded-io-math)
