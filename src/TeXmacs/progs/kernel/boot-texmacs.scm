;;; -*- coding: utf-8 -*-
;;; ☮ ☯ ☭ ☺

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : boot-texmacs.scm
;; DESCRIPTION : some global variables, public macros, on-entry, on-exit and
;;               initialization of the TeXmacs module system
;; COPYRIGHT   : (C) 1999,2017  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when (expand load eval compile)
  (set-current-module (resolve-module '(guile)))
  ;;
  (define texmacs-user (resolve-module '(guile-user))))


(eval-when (expand)
  (newline)
  (display "DEBUG (expand): In module: ")
  (display (module-name (current-module)))
  (newline)
  (newline))

(eval-when (load)
  (newline)
  (display "DEBUG (load): In module: ")
  (display (module-name (current-module)))
  (newline)
  (newline))

(eval-when (eval)
  (newline)
  (display "DEBUG (eval): In module: ")
  (display (module-name (current-module)))
  (newline)
  (newline))

(eval-when (compile)
  (newline)
  (display "DEBUG (compile): In module: ")
  (display (module-name (current-module)))
  (newline)
  (newline))


;;; Note:  In guile_tm.cpp is (set-current-module (resolve-module '(guile)))
;;;        That is necessary to make the cond-expand-provide work properly.

;; TODO: I need to figure out how to make the build system compile all of
;;       the texmacs .scm files into .go objects that can be shipped with
;;       binary packages, for .deb and MacPorts, etc.


(define has-look-and-feel? (lambda (x) (string=? x "emacs")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Redirect standard output
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define original-display display)
(define original-write write)

(define (display . l)
  "display one object on the standard output or a specified port."
  (if (or (null? l) (not (null? (cdr l))))
      (apply original-display l)
      (tm-output (display-to-string (car l)))))

(define (write . l)
  "write an object to the standard output or a specified port."
  (if (or (null? l) (not (null? (cdr l))))
      (apply original-write l)
      (tm-output (object->string (car l)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Provide functions if not defined and public macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;
;;;
;;; Because a symbol lookup from inside any module falls back on a lookup
;;; inside the (guile) module, when use-and-re-export-modules is called from
;;; inside the (guile) module, the public symbols from those modules become
;;; exported from the (guile) module and thus available globally as though
;;; defined inside of the (guile) module.
;;;
(define-syntax use-and-re-export-modules
  (syntax-rules ()
    ((_ (mod ...) ...)
     (begin
       (begin
         (use-modules (mod ...))
         (module-use! (module-public-interface (current-module))
                      (resolve-interface '(mod ...))))
       ...))))

(export-syntax use-and-re-export-modules)

(define-syntax-rule (define-public-macro (name . args) . body)
  (begin
    (define-macro (name . args) . body)
    (export-syntax name)))

(export-syntax define-public-macro)


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

(export-syntax provide-public)


;;; cond-expand already has guile, guile-2, and guile-2.2 available to it.
;;; This gives it also our classification, guile-d, etc.
(eval-when (compile)
  (cond-expand-provide (resolve-module '(guile)) (list (string->symbol (scheme-dialect))))
  ;;
  ;; These must only be things that don't change dynamically at run-time,
  ;; as for installing a program or loading a dynamic module, or whatever.
  ;; They should only be things that properly affect expansions that become
  ;; part of the sexps that are actually compiled by Guile into .go objects,
  ;; which will happen when the package is built, as those will be shipped
  ;; with TeXmacs deb or rpm or whatever packages, pre-built.
  ;;
  ;; Remember to keep that in mind, vs run-time conditionals.
  ;;
  ;; e.g., if the qt-gui became a shared object loadable option, perhaps
  ;; distributable as a separate, optional, deb package, then it would not as
  ;; often, throughout the code, be appropriate to use the qt-gui in
  ;; cond-expand. But as long as it's only an option that's either compiled
  ;; in or not, it does make sense for it to be a cond-expand feature symbol.
  ;;
  (when (os-win32?)
    (cond-expand-provide (resolve-module '(guile)) '(os-win32)))
  (when (os-mingw?)
    (cond-expand-provide (resolve-module '(guile)) '(os-mingw)))
  (when (os-macos?)
    (cond-expand-provide (resolve-module '(guile)) '(os-macos)))
  (when (qt-gui?)
    (cond-expand-provide (resolve-module '(guile)) '(qt-gui)))
  (when (x-gui?)
    (cond-expand-provide (resolve-module '(guile)) '(x-gui))))

;; ? (use-modules (ice-9 rdelim) (ice-9 pretty-print))

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

(define temp-module (current-module))
(define temp-value #f)

(define-public-macro (with-module module . body)
  `(begin
     (set! temp-module (current-module))
     (save-module-excursion
      (lambda ()
        (set-current-module ,module)
        ,@body))))


(define-public (module-available? module-name)
  (catch #t
    (lambda () (resolve-interface module-name) #t)
    (lambda (key . args) #f)))

(define-public (module-provide m)
  (if (not (module-available? m)) (reload-module m)))


(include-from-path "kernel/boot/compat")
;;
(eval-when (expand load eval compile)
  (use-and-re-export-modules (kernel boot abbrevs))
  (use-and-re-export-modules (kernel boot debug))
  (use-and-re-export-modules (kernel boot srfi))
  (use-and-re-export-modules (kernel boot ahash-table))
  (use-and-re-export-modules (kernel boot prologue))
  ;;
  (use-and-re-export-modules (kernel library base))
  (use-and-re-export-modules (kernel library list))
  (use-and-re-export-modules (kernel library tree))
  (use-and-re-export-modules (kernel library content))
  (use-and-re-export-modules (kernel library patch))
  ;;
  (use-and-re-export-modules (kernel regexp regexp-match))
  (use-and-re-export-modules (kernel regexp regexp-select))
  ;;
  (use-and-re-export-modules (kernel logic logic-rules))
  (use-and-re-export-modules (kernel logic logic-query))
  (use-and-re-export-modules (kernel logic logic-data))
  ;;
  (use-and-re-export-modules (kernel texmacs tm-define))
  (use-and-re-export-modules (kernel texmacs tm-preferences))
  (use-and-re-export-modules (kernel texmacs tm-modes))
  (use-and-re-export-modules (kernel texmacs tm-plugins))
  (use-and-re-export-modules (kernel texmacs tm-secure))
  (use-and-re-export-modules (kernel texmacs tm-convert))
  (use-and-re-export-modules (kernel texmacs tm-dialogue))
  (use-and-re-export-modules (kernel texmacs tm-language))
  (use-and-re-export-modules (kernel texmacs tm-file-system))
  (use-and-re-export-modules (kernel texmacs tm-states))
  ;;
  (use-and-re-export-modules (kernel gui gui-markup))
  (use-and-re-export-modules (kernel gui menu-define))
  (use-and-re-export-modules (kernel gui menu-widget))
  (use-and-re-export-modules (kernel gui kbd-define))
  (use-and-re-export-modules (kernel gui kbd-handlers))
  (use-and-re-export-modules (kernel gui menu-test))
  ;;
  (use-and-re-export-modules (kernel old-gui old-gui-widget))
  (use-and-re-export-modules (kernel old-gui old-gui-factory))
  (use-and-re-export-modules (kernel old-gui old-gui-form))
  (use-and-re-export-modules (kernel old-gui old-gui-test)))

;; (define boot-texmacs-loaded? #t)
(display "kernel/boot-texmacs loaded.\n")

(eval-when (expand load eval compile)
  (set-current-module (resolve-module '(guile-user))))
