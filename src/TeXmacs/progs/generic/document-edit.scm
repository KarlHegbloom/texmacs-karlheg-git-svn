
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
        (utils library cursor)
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

(tm-define (test-init-true? var)
  (test-init? var "true"))

(tm-define (toggle-init-env var)
  (:check-mark "*" test-init-true?)
  (with new (if (== (get-init-env var) "true") "false" "true")
    (init-default var)
    (delayed
      (when (!= new (get-init-env var))
        (set-init-env var new)))))

(tm-define (init-multi l)
  (when (and (nnull? l) (nnull? (cdr l)))
    (init-env (car l) (cadr l))
    (init-multi (cddr l))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initial environment management in specific buffers
;; FIXME: should be rewritten using correct 'with-buffer'
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (initial-set-tree u var val)
  (when (tm? val)
    (with-buffer u
      (init-env-tree var val))))

(tm-define (initial-set u var val)
  (when (string? val)
    (with-buffer u
      (init-env var val))))

(tm-define (initial-get-tree u var)
  (or (with-buffer u
	(get-init-tree var))
      (tree "")))

(tm-define (initial-get u var)
  (or (with-buffer u
	(get-init var))
      ""))

(tm-define (initial-defined? u var)
  (with-buffer u
    (style-has? var)))

(tm-define (initial-has? u var)
  (with-buffer u
    (init-has? var)))

(tm-define (initial-default u . vars)
  (with-buffer u
    (apply init-default vars)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Text and paragraph properties
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (test-default-document-language?)
  (null? (list-intersection (get-style-list) supported-languages)))

(tm-define (set-default-document-language)
  (:check-mark "*" test-default-document-language?)
  (let* ((old (get-style-list))
         (new (list-difference old supported-languages)))
    (when (!= new old)
      (set-style-list new))))

(tm-define (get-document-language)
  (with l (list-intersection (get-style-list) supported-languages)
    (if (null? l) (get-init "language") (car l))))

(tm-define (test-document-language? s)
  (== s (get-document-language)))

(tm-define (set-document-language lan)
  (:check-mark "*" test-document-language?)
  (let* ((old (get-style-list))
         (rem (list-difference old supported-languages))
         (new (append rem (if (== lan "english") (list) (list lan)))))
    (when (!= new old)
      (set-style-list new))))

(define (search-env-var t which)
  (cond ((nlist? t) #f)
	((null? t) #f)
	((match? t '(associate "language" :%1)) (caddr t))
	(else (let ((val (search-env-var (car t) which)))
		(if val val (search-env-var (cdr t) which))))))

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
