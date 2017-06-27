;;; coding: utf-8
;;; ☮ ☯ ☭ ☺

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : init-xypic.scm
;; DESCRIPTION : Initialize XYpic plugin
;; COPYRIGHT   : (C) 2004 Nicolas Ratier.
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (xypic-present?)
  (and (url-exists-in-path? "latex")
       (cond ((url-exists-in-path? "kpsewhich")
              (!= (eval-system "kpsewhich xy.sty") ""))
             (else #f))))

(plugin-configure xypic
  (:require (xypic-present?))
  (:launch "tm_xypic --texmacs")
  (:session "XYpic"))
