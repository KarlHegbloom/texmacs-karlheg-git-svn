
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : wildcards-kbd.scm
;; DESCRIPTION : setup keyboard wildcards
;; COPYRIGHT   : (C) 1999  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (texmacs keyboard wildcards-kbd)
  (:use (texmacs keyboard config-kbd)))

(kbd-wildcards
  ("Mod1-" "" #t)
  ("Mod2-" "" #t)
  ("Mod3-" "" #t)
  ("Mod4-" "" #t)
  ("Mod5-" "" #t)

  ("tilde tilde" "tilde")
  ("hat hat" "hat")
  ("umlaut umlaut" "umlaut")
  ("acute acute" "acute")
  ("grave grave" "grave")
  ("cedilla cedilla" "cedilla")
  ("breve breve" "breve")
  ("check check" "check")
  ("doubleacute doubleacute" "doubleacute")
  ("abovering abovering" "abovering")
  ("abovedot abovedot" "abovedot")
  ("ogonek ogonek" "ogonek")

  ,@(compute-wildcard-lines))
