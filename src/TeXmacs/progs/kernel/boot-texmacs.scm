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
  (module-use! (module-public-interface (current-module))
               (resolve-interface '(guile-user))))

(use-and-re-export-modules (texmacs-glue))

;; TODO: I need to figure out how to make the build system compile all of
;;       the texmacs .scm files into .go objects that can be shipped with
;;       binary packages, for .deb and MacPorts, etc.


(define-public has-look-and-feel? (lambda (x) (string=? x "emacs")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Redirect standard output
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This breaks the repl server, causing its' output to appear in the
;;; input/output buffer, rather than in the geiser-mode repl interface
;;; buffer, when running "texmacs --gdb" in gdb mode.

;; (define original-display display)
;; (define original-write write)

;; (define (display . l)
;;   "display one object on the standard output or a specified port."
;;   (if (or (null? l) (not (null? (cdr l))))
;;       (apply original-display l)
;;       (tm-output (display-to-string (car l)))))

;; (define (write . l)
;;   "write an object to the standard output or a specified port."
;;   (if (or (null? l) (not (null? (cdr l))))
;;       (apply original-write l)
;;       (tm-output (object->string (car l)))))


;;; cond-expand already has guile, guile-2, and guile-2.2 available to it.
;;; This gives it also our classification, guile-d, etc.
(eval-when (expand load eval compile)

  ;; (cond-expand-provide (resolve-module '(guile))
  ;;                      (list (string->symbol (scheme-dialect))))
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



(define-public (module-available? module-name)
  (catch #t
    (lambda () (resolve-interface module-name) #t)
    (lambda (key . args) #f)))

(define-public (module-provide m)
  (if (not (module-available? m)) (reload-module m)))


(include-from-path "kernel/boot/compat")
;;
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
(use-and-re-export-modules (kernel old-gui old-gui-test))

;;
;; End with setting the current module to (guile-user) just
;; in case it doesn't already do that by default here.
;;
(eval-when (expand load eval compile)
  (set-current-module (resolve-module '(guile-user))))
