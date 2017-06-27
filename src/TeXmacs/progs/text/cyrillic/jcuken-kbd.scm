;;; -*- coding: raw-text -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : jcuken-kbd.scm
;; DESCRIPTION : typing russian using the jcuken keyboard encoding
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-module (text cyrillic jcuken-kbd)
  :use-module (text text-kbd))

(kbd-map
  (:mode in-cyrillic-jcuken?)

  ("q" "�")
  ("w" "�")
  ("e" "�")
  ("r" "�")
  ("t" "�")
  ("y" "�")
  ("u" "�")
  ("i" "�")
  ("o" "�")
  ("p" "�")
  ("[" "�")
  ("]" "�")
  ("a" "�")
  ("s" "�")
  ("d" "�")
  ("f" "�")
  ("g" "�")
  ("h" "�")
  ("j" "�")
  ("k" "�")
  ("l" "�")
  (";" "�")
  ("'" "�")
  ("z" "�")
  ("x" "�")
  ("c" "�")
  ("v" "�")
  ("b" "�")
  ("n" "�")
  ("m" "�")
  ("," "�")
  ("." "�")
  ("`" "�")

  ("Q" "�")
  ("W" "�")
  ("E" "�")
  ("R" "�")
  ("T" "�")
  ("Y" "�")
  ("U" "�")
  ("I" "�")
  ("O" "�")
  ("P" "�")
  ("{" "�")
  ("}" "�")
  ("A" "�")
  ("S" "�")
  ("D" "�")
  ("F" "�")
  ("G" "�")
  ("H" "�")
  ("J" "�")
  ("K" "�")
  ("L" "�")
  (":" "�")
  ("\"" "�")
  ("Z" "�")
  ("X" "�")
  ("C" "�")
  ("V" "�")
  ("B" "�")
  ("N" "�")
  ("M" "�")
  ("<" "�")
  (">" "�")
  ("~" "�")

  ("@" "\"")
  ("#" "'")
  ("$" "*")
  ("%" ":")
  ("^" ",")
  ("&" ".")
  ("*" ";")

  ("accent:umlaut t" "�")
  ("accent:umlaut T" "�"))
