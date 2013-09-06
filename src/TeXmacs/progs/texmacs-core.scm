
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : texmacs-core.scm
;; DESCRIPTION : some global variables, public macros, on-entry, on-exit and
;;               initialization of the TeXmacs module system
;; COPYRIGHT   : (C) 1999  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; temporary debug code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (define-public (debug-print-module module)
;;   (newline)
;;   (%print-module module (current-output-port))
;;   (newline))

;; (define-public (debug-print-module-bindings module)
;;   (let ((iface (resolve-interface '(guile))))
;;     (debug-print-module module)
;;     (unless (equal? iface module)
;;       (module-map (lambda (sym var)
;;                     (when (symbol? sym)
;;                       (display (symbol->string sym))
;;                       (newline)))
;;                   module)
;;       (newline))))

;; (define-public (debug-print-all-bindings)
;;   (let* ((m (current-module))
;;          (m-public (module-public-interface m))
;;          (m-uses (module-uses m)))
;;     (display "===> CURRENT MODULE:\n")
;;     (debug-print-module-bindings m)
;;     (display "===> PUBLIC INTERFACE:\n")
;;     (debug-print-module-bindings m-public)
;;     (display "===> USES INTERFACES:\n")
;;     (map (lambda (mod)
;;            (debug-print-module-bindings mod))
;;          m-uses)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Provide functions if not defined and public macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (define-macro (inherit-modules . which-list)
;;   (define (module-exports which)
;;     (let* ((m (resolve-module which))
;; 	   (m-public (module-ref m '%module-public-interface)))
;;       (module-map (lambda (sym var) sym) m-public)))
;;   (let ((l (apply append (map module-exports which-list))))
;;     `(begin
;;        (use-modules ,@which-list)
;;        (re-export ,@l))))
;; (export inherit-modules)
(defmacro-public inherit-modules which-list
  (define (module-exports which)
    (let* ((m (resolve-module which))
	   (m-public (module-ref m '%module-public-interface)))
      (module-map (lambda (sym var) sym) m-public)))
  (let ((l (apply append (map module-exports which-list))))
    `(begin
       (use-modules ,@which-list)
       (re-export ,@l))))

(define-public object-stack '(()))

(inherit-modules (texmacs-glue))

(define-public (guile-a?) (equal? (scheme-dialect) "guile-a"))
(define-public (guile-b?) (equal? (scheme-dialect) "guile-b"))
(define-public (guile-c?) (equal? (scheme-dialect) "guile-c"))
(define (guile-b-c?) (or (guile-b?) (guile-c?)))
(if (guile-c?) (use-modules (ice-9 rdelim) (ice-9 pretty-print)))
(define-public texmacs-user (resolve-module '(guile-user)))
(define-public temp-module (current-module))
(define-public temp-value #f)

;; (defmacro-public define-public-macro (head . body)
;;   `(begin
;;      (defmacro-public ,(car head) ,(cdr head) ,@body)))
(cond-expand
 (guile-2 ; guile-2 doesn't allow `define' in expression context
  (define-syntax-rule (define-public-macro (name . args) body ...)
    (begin
      (define-macro (name . args) body ...)
      (export name))))
 (else
   (if (guile-a?)
       (begin
         (define-macro (define-public-macro head . body)
           `(define-public ,(car head)
              ;; FIXME: why can't we use procedure->macro
              ;; for a non-memoizing variant?
              (procedure->memoizing-macro
               (lambda (cmd env)
                 (apply (lambda ,(cdr head) ,@body) (cdr cmd)))))))
       (define-macro (define-public-macro head . body)
         `(begin
            (define-macro ,(car head)
              (lambda ,(cdr head) ,@body))
            (export ,(car head)))))))
(export define-public-macro)

;; (define-public-macro (provide-public head . body)
;;   (if (or (and (symbol? head) (not (defined? head)))
;; 	  (and (pair? head) (symbol? (car head)) (not (defined? (car head)))))
;;       `(define-public ,head ,@body)
;;       '(noop)))
(define-syntax provide-public
  (syntax-rules ()
    ((_ (name . args) . body)
     (define-public name
       (if (defined? 'name)
           name
           (lambda args . body))))
    ((_ sym val)
     (define-public sym
       (if (defined? 'sym) sym val)))))
(export provide-public)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; On-entry and on-exit macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (quit-TeXmacs-scheme) (noop))

(define-public-macro (on-entry . cmd)
  `(begin ,@cmd))

(define-public-macro (on-exit . cmd)
  `(set! quit-TeXmacs-scheme (lambda () ,@cmd (,quit-TeXmacs-scheme))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Module handling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: recode for other guile versions
;; (define-public import-from use-modules)

;; (if (guile-a?)
;;     (begin
;;       (define import-from use-modules)
;;       (define re-export export)))

;; (if (guile-b-c?)
;;     (begin
;;       (define-macro (import-from . modules)
;; 	`(process-use-modules
;; 	  (list ,@(map (lambda (m)
;; 			 `(list ,@(compile-interface-spec m)))
;; 		       modules))))
;;       ;; FIXME: why does this not work?
;;       ;; (define-macro (import-from . modules)
;;       ;;   (define (import-from-body module)
;;       ;;     `(module-use! (current-module) (resolve-module ',module)))
;;       ;;   `(begin
;;       ;;     ,@(map import-from-body modules)))
;;       ))

;;; Doesn't work unless texmacs-module is defined in (guile) module.
;;; Commented out and use `define-module' instead.
;; (define-macro (texmacs-module name . options)
;;   (define (transform action)
;;     (cond ((not (pair? action)) (noop))
;; 	  ((equal? (car action) :use) (cons 'use-modules (cdr action)))
;; 	  ((equal? (car action) :inherit) (cons 'inherit-modules (cdr action)))
;; 	  ((equal? (car action) :export)
;; 	   (display "Warning] The option :export is no longer supported\n")
;; 	   (display "       ] Please use tm-define instead\n"))
;; 	  (else '(noop))))
;;   (let ((l (map-in-order transform options)))
;;     (if (guile-b-c?)
;; 	(set! l (cons `(module-use! (current-module) ,texmacs-user) l)))
;;     ;;(display "loading ") (display name) (display "\n")
;;     `(begin
;;        (define-module ,name)
;;        ,@l)))

(define-public (module-available? module-name)
  (catch #t
    (lambda () (resolve-interface module-name) #t)
    (lambda (key . args) #f)))

(define-public (module-provide m)
  (if (not (module-available? m)) (reload-module m)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Module switching
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public-macro (with-module module . body)
  `(begin
     (set! temp-module (current-module))
     (set-current-module ,module)
     ,@body
     (set-current-module temp-module)))
