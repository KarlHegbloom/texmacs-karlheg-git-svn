
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : document-edit.scm
;; DESCRIPTION : setting global document properties
;; COPYRIGHT   : (C) 2001  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (generic document-edit)
  (:use (utils base environment)
        (utils library length)
        (generic generic-edit)
        (generic document-style)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Projects
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-property (project-attach master)
  (:argument master "Master file"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Preamble mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (in-preamble?)
  (== (get-env "preamble") "true"))

(tm-define (toggle-preamble)
  (:synopsis "Toggle preamble mode.")
  (:check-mark "v" in-preamble?)
  (let ((new (if (string=? (get-env "preamble") "true") "false" "true")))
    (init-env "preamble" new)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global environment variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (test-default? . vals)
  (if (null? vals)
      #t
      (and (not (init-has? (car vals)))
	   (apply test-default? (cdr vals)))))

(tm-define (init-default . args)
  (:check-mark "*" test-default?)
  (for-each init-default-one args))

(tm-define (get-init-env s)
  (with t (get-init-tree s)
    (cond ((tree-atomic? t) (tree->string t))
          ((and (tree-func? t 'macro 1) (tree-atomic? (tree-ref t 0)))
           (tree->string (tree-ref t 0)))
          (else #f))))

(tm-define (test-init? var val)
  (== (get-init-tree var) (string->tree val)))

(tm-property (init-env var val)
  (:check-mark "*" test-init?))

(tm-define (set-init-env s val)
  (with old (get-init-tree s)
    (if (and (tree-func? old 'macro 1) (not (tm-is? val 'macro)))
        (init-env-tree s `(macro ,val))
        (init-env-tree s val))))

(tm-define (init-interactive-env var)
  (:interactive #t)
  (interactive (lambda (s) (set-init-env var s))
    (list (or (logic-ref env-var-description% var) var) "string"
          (get-init-env var))))

(tm-define (toggle-init-env var)
  (with new (if (== (get-init-env var) "true") "false" "true")
    (init-default var)
    (delayed
      (when (!= new (get-init-env var))
        (set-init-env var new)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Text and paragraph properties
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (test-document-language? s) (string=? (get-env "language") s))
(tm-define (init-language lan)
  (:check-mark "*" test-document-language?)
  (let ((cyrillic-before (in? (tree->stree (get-init-tree "language"))
			      '("bulgarian" "russian" "ukrainian")))
	(cyrillic-after (in? lan '("bulgarian" "russian" "ukrainian")))
	(chinese-before (in? (tree->stree (get-init-tree "language"))
			     '("chinese" "taiwanese")))
	(chinese-after (in? lan '("chinese" "taiwanese")))
	(japanese-before (in? (tree->stree (get-init-tree "language"))
			      '("japanese")))
	(japanese-after (in? lan '("japanese")))
	(korean-before (in? (tree->stree (get-init-tree "language"))
			    '("korean")))
	(korean-after (in? lan '("korean"))))
    (if (and cyrillic-before (not cyrillic-after)) (init-default "font"))
    (if (and chinese-before (not chinese-after)) (init-default "font"))
    (if (and japanese-before (not japanese-after)) (init-default "font"))
    (if (and korean-before (not korean-after)) (init-default "font"))
    (init-env "language" lan)
    (if (and cyrillic-after (not cyrillic-before))
	(init-env "font" "cyrillic"))
    (if (and chinese-after (not chinese-before))
	(init-env "font" "sys-chinese"))
    (if (and japanese-after (not japanese-before))
	(init-env "font" "sys-japanese"))
    (if (and korean-after (not korean-before))
	(init-env "font" "sys-korean"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main page layout
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (test-default-page-medium?) (test-default? "page-medium"))
(tm-define (init-default-page-medium)
  (:check-mark "*" test-default-page-medium?)
  (init-default "page-medium")
  (notify-page-change))

(define (test-page-medium? s) (string=? (get-env "page-medium") s))
(tm-define (init-page-medium s)
  (:check-mark "*" test-page-medium?)
  (init-env "page-medium" s)
  (notify-page-change))

(define (test-page-type? s) (string=? (get-env "page-type") s))
(tm-define (init-page-type s)
  (:check-mark "*" test-page-type?)
  (init-env "page-type" s)
  (init-default "page-width" "page-height")
  (notify-page-change))

(tm-define (init-page-size w h)
  (:argument w "Page width")
  (:argument h "Page height")
  (init-env "page-type" "user")
  (init-env "page-width" w)
  (init-env "page-height" h))

(define (test-default-page-orientation?) (test-default? "page-orientation"))
(tm-define (init-default-page-orientation)
  (:check-mark "*" test-default-page-orientation?)
  (init-default "page-orientation")
  (notify-page-change))

(define (test-page-orientation? s) (string=? (get-env "page-orientation") s))
(tm-define (init-page-orientation s)
  (:check-mark "*" test-page-orientation?)
  (init-env "page-orientation" s)
  (notify-page-change))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Further page layout settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (visible-header-and-footer?)
  (== (get-env "page-show-hf") "true"))

(tm-define (toggle-visible-header-and-footer)
  (:synopsis "Toggle visibility of headers and footers in 'page' paper mode.")
  (:check-mark "v" visible-header-and-footer?)
  (init-env "page-show-hf"
	    (if (== (get-env "page-show-hf") "true") "false" "true")))

(define (page-width-margin?)
  (== (get-env "page-width-margin") "true"))

(tm-define (toggle-page-width-margin)
  (:synopsis "Toggle mode for determining margins from paragraph width.")
  (:check-mark "v" page-width-margin?)
  (init-env "page-width-margin" (if (page-width-margin?) "false" "true")))

(define (not-page-screen-margin?)
  (== (get-env "page-screen-margin") "false"))

(tm-define (toggle-page-screen-margin)
  (:synopsis "Toggle mode for using special margins for screen editing.")
  (:check-mark "v" not-page-screen-margin?)
  (init-env "page-screen-margin"
	    (if (not-page-screen-margin?) "true" "false")))
