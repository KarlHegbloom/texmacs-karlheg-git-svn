;;; -*- coding: utf-8 -*-
;;; ☮ ☯ ☭ ☺

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : boot-texmacs.scm
;; DESCRIPTION : some global variables, public macros, on-entry, on-exit and
;;               initialization of the TeXmacs module system
;; COPYRIGHT   : (C) 1999  Joris van der Hoeven
;;             : (C) 2016  Darcy Shen
;;             : (C) 2017  Karl Martin Hegbloom
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; This file is loaded by src/Scheme/Guile/guile_tm.cpp:initialize_scheme()
;;; via eval_scheme_file_in_load_path("kernel/boot-texmacs"). It does not
;;; define it's own module. It is meant to be loaded into the base (guile)
;;; module, just as Guile's own boot-9.scm is loaded there during its'
;;; startup.
;;;
(eval-when (expand load eval compile)
  (set-current-module (resolve-module '(guile)))
  (use-modules (guile-user))
  (module-use! (module-public-interface (current-module))
               (resolve-interface '(guile-user))))
;;;
;;; The above will make all exports from (guile-user) also appear in the
;;; (guile) module... without causing a non-terminating recursion loop!
;;;
;;; [I think that the case of a module using another one that also uses it,
;;;  creating a sort of dependency loop... is a common occurance, and so the
;;;  lookup mechanism very likely is designed with that in mind, and so no
;;;  non-terminating recursion loop will occur.]
;;;
;;;;;;



;;;;;;
;;;
;;; TODO: I need to figure out how to make the build system compile all of
;;;       the texmacs .scm files into .go objects that can be shipped with
;;;       binary packages, for .deb and MacPorts, etc.
;;;
;;; For now, during development, I'm running TeXmacs under M=x gdb as:
;;;
;;;   ./texmacs --gdb
;;;
;;; ... and am then using M-x connect-to-guile to get a Geiser mode Guile REPL
;;; connected to the repl server thread of the now running TeXmacs instance.
;;;
;;; In the gud-mode input/output buffer, which is what you see when you run
;;; TeXmacs from the shell command line, messages are printed regarding
;;; compilation of the various .scm files brought in during bootstrapping.
;;;
;;;;;;



;;;;;;
;;;
;;; Because a symbol lookup from inside any module falls back on a lookup
;;; inside the (guile) module, when use-and-re-export-modules is called from
;;; inside thise (guile) module, the public symbols from those modules become
;;; exported from the (guile) module and thus available globally as though
;;; defined inside of the (guile) module.
;;;
(define-syntax use-and-re-export-modules
  (syntax-rules ()
    ((_ (mod ...) ...)
     (eval-when (expand load eval compile)
       (begin
         (use-modules (mod ...))
         (module-use! (module-public-interface (current-module))
                      (resolve-interface '(mod ...))))
       ...))))

(export-syntax use-and-re-export-modules)

;;;;;;
;;;
;;; Demonstration:
;;;
;;; GNU Guile 2.2.2
;;; Copyright (C) 1995-2017 Free Software Foundation, Inc.
;;;
;;; Guile comes with ABSOLUTELY NO WARRANTY; for details type `,show w'.
;;; This program is free software, and you are welcome to redistribute it
;;; under certain conditions; type `,show c' for details.
;;;
;;; Enter `,help' for help.
;;; scheme@(guile-user)> ,m guile
;;;
;;;   At this point, I used Geiser mode's C-x C-e to send the definition of
;;;   the above syntax to the * Guile REPL * buffer... Then:
;;;
;;; scheme@(guile)> (define-module (kernel blah something))
;;; $2 = #<directory (kernel blah something) 55c428bb78c0>
;;;
;;;   Note that this kind of module is not defined "inside of" another
;;;   module. It is defined inside of Guile's global state. (There are nested
;;;   namespaces, but this isn't that.)
;;;
;;; scheme@(kernel blah something)> (define-public (blah blah) blah)
;;; scheme@(kernel blah something)> ,m guile
;;; scheme@(guile)> (use-and-re-export-modules (kernel blah something))
;;; scheme@(guile)> ,use
;;; (ice-9 ports)
;;; (value-history)
;;; (kernel blah something)
;;; scheme@(guile)> ,m guile-user
;;; scheme@(guile-user)> ,use
;;; (guile)
;;; (system base compile)
;;; (ice-9 r5rs)
;;; (ice-9 session)
;;; (ice-9 regex)
;;; (ice-9 threads)
;;; (geiser emacs)
;;; (value-history)
;;; scheme@(guile-user)> (blah 'blah)
;;; $3 = blah
;;; scheme@(guile-user)> ,b
;;; scheme@(guile-user)> ,m guile
;;; scheme@(guile)> (use-and-re-export-modules (guile-user))
;;; scheme@(guile)> ,use
;;; (ice-9 ports)
;;; (value-history)
;;; (kernel blah something)
;;; (guile-user)
;;; scheme@(guile)> ,m guile-user
;;; scheme@(guile-user)> ,b
;;; scheme@(guile-user)> (define-public (foo fee) fee)
;;; scheme@(guile-user)> ,b
;;; foo                     #<variable 55c428ba91c0 value: #<procedure foo (fee)>>
;;; scheme@(guile-user)> ,m guile
;;; scheme@(guile)> (foo 'fee)
;;; $4 = fee
;;; scheme@(guile)> ,m kernel blah something
;;; scheme@(kernel blah something)> ,b
;;; blah                    #<variable 55c429092890 value: #<procedure blah (blah)>>
;;; scheme@(kernel blah something)> (foo 'fee)
;;; $5 = fee
;;; scheme@(kernel blah something)> ,use
;;; (guile)
;;; (value-history)
;;;
;;;;;;



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



(define-syntax-rule (with-module (mod ...) body ...)
  (save-module-excursion
   (lambda ()
     (set-current-module (resolve-module '(mod ...)))
     body ...)))

(export-syntax with-module)



;;;;;;
;;;
;;; These are used by kernel/gui/menu-define, kernel/gui/kbd-define,
;;; kernel/texmacs/tm-define, version/version-tmfs, and database/bib-manage.
;;;
(define-public (module-available? module-name)
  (false-if-exception
    (resolve-interface module-name)))

(define-public (module-provide module-name)
  (unless (module-available? module-name)
    (reload-module module-name)))



;;;;;;
;;;
;;; From: http://okmij.org/ftp/Scheme/macro-trace.txt
;;;
;;;
(define-syntax mtrace
  (syntax-rules ()
    ((mtrace x)
     (begin
       (display "Trace: ") (write 'x) (newline)
       x))))

(export-syntax mtrace)

;;; Example:  Given:
;;;
;;; (define-syntax test
;;;   (syntax-rules ()
;;;     ((test name a1 ...)
;;;      (define (name a1 ...) (apply * (list a1 ...))))))
;;;
;;; Rewrite as follows:
;;;
;;; (define-syntax test
;;;   (syntax-rules ()
;;;     ((test name a1 ...)
;;;      (mtrace
;;;       (define (name a1 ...) (apply * (list a1 ...)))))))
;;;
;;; scheme@(guile-user)> (test h a b c)
;;; Trace: (define (h a b c) (apply * (list a b c)))
;;; scheme@(guile-user)> (h 1 3 5)
;;; $11 = 15
;;;


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


(use-and-re-export-modules (ice-9 eval-string))

;;;;;;
;;;
;;; tm-define returns a goops object instance... It's class is a subclass of
;;; <applicable>...
;;;
;;;   See: (kernel texmacs tm-define)
;;;
(use-and-re-export-modules (oop goops))

;;;;;;
;;;
;;; Now add 'merge-generics to the default-duplicate-binding-handler list.
;;;
(eval-when (expand load eval compile)
  (default-duplicate-binding-handler
    (cons (cons 'merge-accessors
                (cons 'merge-generics
                      (default-duplicate-binding-handler))))))


;;;;;;
;;;
;;; The (texmacs-glue) module is where the scheme functions exported from the
;;; C++ code are placed by src/Scheme/Glue/*.cpp
;;;
(use-and-re-export-modules (texmacs-glue))

;;;;;;
;;;
;;; cond-expand already has guile, guile-2, and guile-2.2 available to it.
;;;
(eval-when (expand load eval compile)
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



(define-public (has-look-and-feel? x)
  (string=? x "emacs"))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; On-entry and on-exit macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (quit-TeXmacs-scheme) (noop))

(define-public-macro (on-entry . cmd)
  `(begin ,@cmd))

(define-public-macro (on-exit . cmd)
  `(set! quit-TeXmacs-scheme (lambda () ,@cmd (,quit-TeXmacs-scheme))))



;;;;;;
;;;
;;; Pull in the rest of the TeXmacs "kernel".
;;;
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



;;;;;;
;;;
;;; End with setting the current module to (guile-user), which is the module
;;; held by @var{texmacs-user}.
;;;
(set-current-module (resolve-module '(guile-user)))

;; Remain in the `(guile)' module at compilation-time so that the
;; `-Wunused-toplevel' warning works as expected.
(eval-when (compile) (set-current-module the-root-module))
