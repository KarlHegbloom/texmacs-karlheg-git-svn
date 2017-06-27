;;; -*- coding: utf-8 -*-
;;; ☮ ☯ ☭ ☺
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : calc-markup.scm
;; DESCRIPTION : callbacks for spreadsheets
;; COPYRIGHT   : (C) 2016  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (texmacs-module (dynamic calc-markup)
;;   (:use (utils library tree)
;;         (utils library cursor)
;;         (dynamic calc-edit)))

(define-module (dynamic calc-markup)
  :use-module (utils library tree)
  :use-module (utils library cursor)
  :use-module (dynamic calc-edit))


(tm-define (calc-check-callback v)
  (:secure #t)
  (calc-lazy-recheck v)
  "")
