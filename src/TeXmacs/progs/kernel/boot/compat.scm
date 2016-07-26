;;; coding: utf-8

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : compat.scm
;; DESCRIPTION : for compatability
;; COPYRIGHT   : (C) 2003  David Allouche, Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-module (kernel boot compat)
  :use-module (texmacs-core))

(cond-expand
 (os-win32
  (define cout-port
    (make-soft-port
     (vector (lambda (c) (win32-display (char->string c)))
             (lambda (s) (win32-display s))
             (lambda () (noop))
             (lambda () #\?)
             (lambda () (noop)))
     "w"))

  (set-current-output-port cout-port)
  (set-current-error-port cout-port))
 (else
   ))

;;; make eval from guile>=1.6.0 backwards compatible
(catch 'wrong-number-of-args
       (lambda () (eval 1))
       (lambda arg
	 (let ((default-eval eval))
	   (set! eval (lambda (form . env)
			(cond ((null? form) (list))
			      ((null? env) (primitive-eval form))
			      (else (default-eval form (car env)))))))))

;;; for old-style initialization files
(define-public (exec-file . args)
  (noop))

;;; certain Guile versions do not define 'filter'
(cond-expand
 (guile-2 (noop))
 (else
   (if (not (defined? 'filter))
       (define-public (filter pred? l)
         (apply append (map (lambda (x) (if (pred? x) (list x) (list))) l))))))
