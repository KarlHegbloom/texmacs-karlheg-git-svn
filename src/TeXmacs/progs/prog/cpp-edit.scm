
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : cpp-edit.scm
;; DESCRIPTION : editing C++ programs
;; COPYRIGHT   :
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;
;; TO-DO: this module should provide automatic indentation and other facilities
;;        for C++ source code.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (prog cpp-edit)
  (:use (prog prog-edit)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Automatic insertion, highlighting and selection of brackets and quotes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (cpp-bracket-open lbr rbr)
  (bracket-open lbr rbr "\\"))

(tm-define (cpp-bracket-close lbr rbr)
  (bracket-close lbr rbr "\\"))

; TODO: select strings first
(tm-define (kbd-select-enlarge)
  (:mode in-prog-cpp?)
  (program-select-enlarge "{" "}"))

(tm-define (notify-cursor-moved status)
  (:require prog-highlight-brackets?)
  (:mode in-prog-cpp?)
  (select-brackets-after-movement "{" "}" "\\"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Preferences for syntax highlighting
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (notify-cpp-pref var val)
   (syntax-read-preferences "cpp"))

(define-preferences
  ("syntax:cpp:none" "black" notify-cpp-pref)
  ("syntax:cpp:comment" "dark grey" notify-cpp-pref)
  ("syntax:cpp:keyword" "dark magenta" notify-cpp-pref)
  ("syntax:cpp:error" "dark red" notify-cpp-pref)
  ("syntax:cpp:preprocessor" "dark green" notify-cpp-pref)
  ("syntax:cpp:preprocessor_directive" "dark brown" notify-cpp-pref)
  ("syntax:cpp:constant" "#4040c0" notify-cpp-pref)
  ("syntax:cpp:constant_identifier" "#4040c0" notify-cpp-pref)
  ("syntax:cpp:constant_function" "#4040c0" notify-cpp-pref)
  ("syntax:cpp:constant_type" "#4040c0" notify-cpp-pref)
  ("syntax:cpp:constant_category" "#4040c0" notify-cpp-pref)
  ("syntax:cpp:constant_module" "#4040c0" notify-cpp-pref)
  ("syntax:cpp:constant_number" "#4040c0" notify-cpp-pref)
  ("syntax:cpp:constant_string" "dark red" notify-cpp-pref)
  ("syntax:cpp:variable" "#606060" notify-cpp-pref)
  ("syntax:cpp:variable_identifier" "#204080" notify-cpp-pref)
  ("syntax:cpp:variable_function" "#606060" notify-cpp-pref)
  ("syntax:cpp:variable_type" "#00c000" notify-cpp-pref)
  ("syntax:cpp:variable_category" "#00c000" notify-cpp-pref)
  ("syntax:cpp:variable_module" "#00c000" notify-cpp-pref)
  ("syntax:cpp:declare" "#0000c0" notify-cpp-pref)
  ("syntax:cpp:declare_identifier" "#0000c0" notify-cpp-pref)
  ("syntax:cpp:declare_function" "#0000c0" notify-cpp-pref)
  ("syntax:cpp:declare_type" "#0000c0" notify-cpp-pref)
  ("syntax:cpp:declare_category" "#d030d0" notify-cpp-pref)
  ("syntax:cpp:declare_module" "#0000c0" notify-cpp-pref))

