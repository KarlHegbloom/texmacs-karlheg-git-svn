;;; -*- coding: utf-8 -*-
;;; ☮ ☯ ☭ ☺

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; MODULE      : tm-define.scm
;;; DESCRIPTION : Macros for defining TeXmacs functions
;;; COPYRIGHT   : (C) 1999  Joris van der Hoeven
;;;             : (C) 2017  Karl Martin Hegbloom
;;;
;;; This software falls under the GNU general public license version 3 or later.
;;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-module (kernel texmacs tm-define)
  #:use-module (kernel boot abbrevs)
  #:use-module (kernel boot ahash-table)
  #:use-module (kernel boot srfi)
  #:use-module (kernel boot debug)
  #:use-module (kernel library base)
  #:use-module (kernel library list)
  ;;
  #:use-module (oop goops)
  #:use-module (system base compile))



;;; This will all be converted to using syntax-case...

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; <tm-def>
;;;
;;;   A contextually overloadable function defined with the tm-define form is
;;;   an instance of a subclass of (<object> <applicable>) [making it similar
;;;   to <applicable-struct>, but not sharing the specialized initialize
;;;   method chain (UTSL, oop/goops.scm). The <tm-def> is sort of like a
;;;   <method>, but at least for now, it's not hooked into the MOP
;;;   generic-function dispatch mechanism... the tm-define dispatch mechanism
;;;   is defined within this module.
;;;
;;;   In the <tm-def>'s @slot{procedure} slot is a syntax expander generated
;;;   function that holds, lexically bound within itself, a sort of "this"
;;;   reference so that it can ????? XXXXX ????    
;;;
;;;   ... well if the function is defined after the (make
;;;   <tm-defined-applicable> ...) and then the slot is set to that function,
;;;   it will require a reference to the instance it is assigned to... but if
;;;   that function is generated within the context of the ... blah blah
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;
;;;
;;; All tm-define'd overloadable functions will be instantiated in and
;;; exported from the (kernel texmacs tm-define definitions) module.
;;;
(eval-when (expand load eval compile)

  (save-module-excursion
   (define-module (kernel texmacs tm-define definitions)
     #:use-modules (kernel texmacs tm-define)))

  (with-module (guile)
    (use-and-re-export-modules (kernel texmacs tm-define definitions)))

  (define the-tm-def-module
    (resolve-module '(kernel texmacs tm-define definitions)))

  ) ; eval-when


;;; tmp notes:
;;;
;;;  tm-defined-table is no longer required as long as every tm-defined
;;;  function is instantiated inside of the same module... it can be this one,
;;;  (kernel texmacs tm-define), or maybe texmacs-user, as long as the (guile)
;;;  modules has it pulled in via use-and-re-export-modules so that every
;;;  tm-defined function is available globally.

;;;

(eval-when (expand load eval compile)

  (define-class-with-accessors-keywords <tm-def> (<object> <applicable>)
    #:metaclass <applicable-struct-class>
    procedure
    ;;
    (tm-defined #:init-value ()
                #:init-keyword #:tm-def-tm-defined
                )
    ;;
    (tm-def-mode #:init-value #f
                 #:init-keyword #:tm-def-mode
                 #:slot-set! (lambda (tm-def opt decl)
                               ???
                               )
                 )
    (tm-def-require #:init-value #f
                    #:init-keyword #:tm-def-require
                    )
    (tm-def-type #:init-value #f
                 #:init-keyword #:tm-def-type
                 )
    (tm-def-synopsis #:init-value #f
                     #:init-keyword #:tm-def-synopsis
                     )
    (tm-def-returns #:init-value #f
                    #:init-keyword #:tm-def-returns
                    )
    (tm-def-note #:init-value #f
                 #:init-keyword #:tm-def-note
                 )
    (tm-def-argument #:init-value #f
                     #:init-keyword #:tm-def-argument
                     )
    (tm-def-default #:init-value #f
                    #:init-keyword #:tm-def-default
                    )
    (tm-def-proposals #:init-value #f
                      #:init-keyword #:tm-def-proposals
                      )
    (tm-def-secure #:init-value #f
                   #:init-keyword #:tm-def-secure
                   )
    (tm-def-check-mark #:init-value #f
                       #:init-keyword #:tm-def-check-mark
                       )
    (tm-def-interactive #:init-value #f
                        #:init-keyword #:tm-def-interactive
                        )
    (tm-def-balloon #:init-value #f
                    #:init-keyword #:tm-def-balloon
                    )
    )

  ) ; eval-when



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; !!!! OUT OF BAND REMINDER !!!
;;
;; See ~/org/notes.org !
;;
;;!!! [OT:Todo: org files and github online repo for management of the
;; domestic violence is the new Jim Crow blah blah blah alleged violations of
;; protective order, the research project for discovery, e.g., evidence,
;; depositions, affidavits.
;;
;; Todo: fund it with money from what once was the budget of the infamous
;;       departments of champtery and maintence, bills of attainder and blank
;;       checks.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(define (strip-tm-def-kw-prefix kw)
  (let ((kwstr (symbol->string (keyword->symbol kw))))
    (symbol->keyword
     (string->symbol
      (if (string-prefix? "tm-def-" kwstr)
          (substring kwstr 7)
          kwstr)))))


;;;
;;; Before I write more of this, I will be reviewing boot-9 and psyntax
;;; sources because I know there are a lot of nifty syntax tricks that will be
;;; similar to what tm-define will require internally. i.e., define-inlinable,
;;; and the syntax transformer also must define variables in other
;;; modules... also see the use of syntax-parameterize there.
;;;

;;;

;; this is something like what will be inside of tm-define I think... it has
;; not been run yet. I am pushing to git to show what I've done so far.
;;
;; The tm-define below will be rolled into this one...
;;
(define-syntax tm-define
  (lambda (x)

    (define (keyword-like? stx)
      (let ((dat (syntax->datum stx)))
        (and (symbol? dat)
             (eqv? (string-ref (symbol->string dat) 0) #\:))))

    (define (->keyword sym)
      (symbol->keyword (string->symbol (substring (symbol->string sym) 1))))

    (syntax-case x ()
      ((_ (name arg ...) body0 body1 ...)
       (and (and-map symbol? (syntax->datum #'(name arg ...)))
            (not (module-defined? the-tm-def-module (syntax->datum #'name))))          )
       (let* ((this-new-tm-def (make <tm-def>))
              (compiled-proc (compile
                              (syntax->datum
                               (let-syntax ((parsed-body
                                             (lambda (x)
                                               (syntax-case x ()
                                                 ((_ (kw kwarg) body1 ...)
                                                  (keyword? kw)
                                                  (ahash-set! (slot-ref this-new-tm-def
                                                                        'options-table)
                                                              kw kwarg)
                                                  #'(parsed-body body1 ...))
                                                 ((_ body1 ...)
                                                  #'(body1 ...))))))
                                 (lambda (arg ...) (parsed-body body0 body1 ...)))))))
         (with-syntax ((this-new-tm-def this-new-tm-def)
                       )
           #'(let ((this-tm-def this-new-tm-def))
               (module-define! the-tm-def-module name this-tm-def)
               (slot-set! this-tm-def 'procedure
                          (lambda (arg ...)
                            (let ((defs (slot-ref this-tm-def 'tm-defined '())))
                              (unless (null? defs)
                                ((car defs) arg ...)))))
               (slot-set! this-tm-def 'tm-defined
                          (cons compiled-proc (slot-ref this-tm-def 'tm-defined)))
               ))))
      ((_ (name arg ...) body0 body1)
       (and (and-map symbol? (syntax->datum #'(name arg ...)))
            (module-defined? the-tm-def-module (syntax->datum #'name)))
       (let ((this-tm-def-to-overload (module-ref the-tm-def-module name))
             (compiled-proc (compile (syntax->datum #'(lambda (arg ...) body0 body1 ...)))))
         (with-syntax ((this-tm-def-to-overload this-tm-def-to-overload)
                       )
           #'(let ((this-tm-def this-tm-def-to-overload))
               (slot-set! this-tm-def 'tm-defined
                      (cons compiled-proc (slot-ref this-tm-def 'tm-defined)))))))
      ))

(export-syntax tm-define)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Contextual overloading
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when (expand load eval compile)

  (define-public (ctx-add-condition l kind opt)
    (display* "tm-define:ctx-add-condition " l ", " kind ", " opt "\n")
    (append l (list opt)))

  (define-public (ctx-insert ctx data conds)
    (display* "tm-define:ctx-insert " ctx ", " data ", " conds "\n")
    (cons (cons conds data) (or ctx '())))

  (define-public (ctx-find ctx conds)
    (display* "tm-define:ctx-find " ctx ", " conds "\n")
    (cond ((or (not ctx) (null? ctx)) #f)
          ((== (caar ctx) conds) (cdar ctx))
          (else (ctx-find (cdr ctx) conds))))

  (define-public (ctx-remove ctx conds)
    (display* "tm-define:ctx-remove " ctx ", " conds "\n")
    (cond ((or (not ctx) (null? ctx)) '())
          ((== (caar ctx) conds) (ctx-remove (cdr ctx) conds))
          (else (cons (car ctx) (ctx-remove (cdr ctx) conds)))))

  (define (and-apply l args)
    (display* "tm-define:and-apply " l " " args "\n")
    (or (null? l)
        (and (apply (car l) (or args '()))
             (and-apply (cdr l) args))))

  (define-public (ctx-resolve ctx args)
    (display* "tm-define:ctx-resolve " ctx ", " args "\n")
    (cond ((or (not ctx) (null? ctx)) #f)
          ((and-apply (caar ctx) args) (cdar ctx))
          (else (ctx-resolve (cdr ctx) args))))

  ) ;; eval-when (expand load eval)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Global variables and subroutines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when (expand load eval compile)

  (define-public tm-defined-table (make-ahash-table))
  (define-public tm-defined-name (make-ahash-table))
  (define-public tm-defined-module (make-ahash-table))
  (define-public define-option-table (make-hash-table 100))

  (define-public cur-conds '())

  (define cur-props-table (make-ahash-table))
  (define cur-props '())

  (define-public (ca*r x) (if (pair? x) (ca*r (car x)) x))
  ;;
  ;; scheme@(guile-user)> (ca*r '((a b) c d))
  ;; $9 = a
  ;; scheme@(guile-user)> (ca*r '(((a b) c) d))
  ;; $10 = a

  (define-public (ca*adr x) (ca*r (cadr x)))
  ;;
  ;; scheme@(guile-user)> (ca*adr '((a b) c d))
  ;; $11 = c
  ;; scheme@(guile-user)> (ca*adr '(((a b) c) d))
  ;; $12 = d

  (define (lambda* head body)
    "Confuses novice programmers to cause many minutes of delay."
    (if (pair? head)
        (lambda* (car head) `((lambda ,(cdr head) ,@body)))
        (car body)))

  (define (listify args)
    "Turns and improper list into a proper list,
i.e., (listify '(a b . c)) => (a b c)"
    (if (pair? args)
        (cons (car args) (listify (cdr args)))
        (list args)))

  (define (apply* fun head)
    (cond ((list? head)
           `(,(apply* fun (car head)) ,@(cdr head)))
          ((pair? head)
           `(apply ,(apply* fun (car head)) (cons* ,@(listify (cdr head)))))
          (else fun)))

  (define (and* conds)
    (if (list-1? conds) (car conds) `(and ,@conds)))

  (define (begin* conds)
    (if (list-1? conds) (car conds) `(begin ,@conds)))

  (let ((old-procedure-name procedure-name))
    (set! procedure-name
          (lambda (fun)
            (or (old-procedure-name fun)
                (ahash-ref tm-defined-name fun)))))

  ) ;; eval-when (expand load eval)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Overloading
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;
;;;
;;; These hand around and save scheme lists produced by quasiquote. Those
;;; forms are not evaluated now, but instead are stored for later evaluation,
;;; i.e., (#:require (and in-math? ...))
;;;
;;; Ok, this is Karl trying to grok the program... The value of "former" is
;;; lexically bound... my program that printed key and value via a
;;; hash-table-for-each lost that information. There's more going on than mere
;;; scheme lists being built and stored. Those are closures; they have a
;;; scheme form and an evaluation environment in which to resolve the values
;;; of those symbols; but in the first one, "former" is not bound, in the
;;; second one it's bound to the list holding only the first one, in the third
;;; one it's bound to the list holding the second and first, etc., so
;;; "former"'s value is different inside each one, or this wouldn't work
;;; right.
;;;
;;; This comment will sort of like "self"-destruct after a few WIP's, right?
;;; MI.
;;;
;;;;;;

(eval-when (expand load eval)

  (define (ctx-add-condition! kind opt)
    (set! cur-conds (ctx-add-condition cur-conds kind opt)))

  (define (define-option-mode opt decl)
    (ctx-add-condition! 0 (car opt))
    decl)

  (define-public (predicate-option? x)
    (or (and (symbol? x) (string-ends? (symbol->string x) "?"))
        (and (pair? x) (== (car x) 'lambda))))

  (define (define-option-match opt decl)
    (cond ((predicate-option? opt) (ctx-add-condition! 3 opt))
	  ((and (pair? opt) (null? (cdr opt))
	        (predicate-option? (car opt))
	        (list? (cadr decl)) (= (length (cadr decl)) 3))
	   (ctx-add-condition! 3 (car opt)))
	  (else (ctx-add-condition! 3 `(lambda args (match? args ',opt)))))
    decl)


  (define (define-option-require opt decl)
    (define-option-match
      `(lambda ,(cdadr decl) ,(car opt))
      decl))

  (hash-set! define-option-table :mode define-option-mode)
  (hash-set! define-option-table :require define-option-require)

  ) ;; eval-when (expand load eval)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Properties of overloaded functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when (expand load eval)

  (define (filter-conds l)
    "Remove conditions which depend on arguments from list"
    (cond ((null? l) l)
	  ((>= (car l) 2) (filter-conds (cddr l)))
	  (else (cons (car l) (cons (cadr l) (filter-conds (cddr l)))))))

  (define-public (property-set! var prop what conds*)
    "Associate a property to a function symbol under conditions"
    (let* ((key (cons var prop))
	   (conds (filter-conds conds*)))
      (ahash-set! cur-props-table key
		  (ctx-insert (ahash-ref cur-props-table key) what conds))))

  (define-public (property var prop)
    "Retrieve a property of a function symbol"
    (if (procedure? var) (set! var (procedure-name var)))
    (let* ((key (cons var prop)))
      (ctx-resolve (ahash-ref cur-props-table key) #f)))

  (define (property-rewrite l)
    `(property-set! ,@l (list ,@cur-conds)))

  (define (define-property which)
    (lambda (opt decl)
      (set! cur-props (cons `(',(ca*adr decl) ,which ',opt) cur-props))
      decl))

  (define (define-property* which)
    (lambda (opt decl)
      (set! cur-props (cons `(',(ca*adr decl) ,which (list ,@opt)) cur-props))
      decl))

  (define (compute-arguments decl)
    (cond ((pair? (cadr decl)) (cdadr decl))
	  ((and (pair? (caddr decl)) (== (caaddr decl) 'lambda))
	   (cadr (caddr decl)))
	  (else
	    (texmacs-error "compute-arguments" "Bad argument documentation"))))

  (define (define-option-argument opt decl)
    (let* ((var (ca*adr decl))
	   (args (compute-arguments decl))
	   (arg (list :argument (car opt))))
      (set! cur-props (cons `(',var :arguments ',args) cur-props))
      (set! cur-props (cons `(',var ',arg ',(cdr opt)) cur-props))
      decl))

  (define (define-option-default opt decl)
    (let* ((var (ca*adr decl))
	   (arg (list :default (car opt))))
      (set! cur-props (cons `(',var ',arg (lambda () ,@(cdr opt))) cur-props))
      decl))

  (define (define-option-proposals opt decl)
    (let* ((var (ca*adr decl))
	   (arg (list :proposals (car opt))))
      (set! cur-props (cons `(',var ',arg (lambda () ,@(cdr opt))) cur-props))
      decl))

  (hash-set! define-option-table :type (define-property :type))
  (hash-set! define-option-table :synopsis (define-property :synopsis))
  (hash-set! define-option-table :returns (define-property :returns))
  (hash-set! define-option-table :note (define-property :note))
  (hash-set! define-option-table :argument define-option-argument)
  (hash-set! define-option-table :default define-option-default)
  (hash-set! define-option-table :proposals define-option-proposals)
  (hash-set! define-option-table :secure (define-property* :secure))
  (hash-set! define-option-table :check-mark (define-property* :check-mark))
  (hash-set! define-option-table :interactive (define-property* :interactive))
  (hash-set! define-option-table :balloon (define-property* :balloon))

  (define-public (procedure-sources about)
    (or (and (procedure? about)
             (ahash-ref tm-defined-table (procedure-name about)))
        (and (procedure-source about)
             (list (procedure-source about)))))

  ) ;; eval-when (expand load eval)

(define-public (help about)
  ;; very provisional
  (cond ((property about :synopsis)
	 (property about :synopsis))
	((procedure-documentation about)
	 (procedure-documentation about))
	(else #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Overloaded functions with properties
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Here's a hint regarding the "unhandled constant" error that is printed
;;; during TeXmacs booting while I still haven't figured it out quite right...
;;;
;;; scheme@(guile-user)> ca*r
;;; $13 = #<procedure ca*r (x)>
;;; scheme@(guile-user)> (compile ca*r)
;;; ;;; loading /usr/share/guile/2.2/language/value/spec.scm
;;; ERROR: In procedure scm-error:
;;; ERROR: unhandled constant #<procedure ca*r (x)>
;;;
(d;;; scheme@(guile-user)> (set! ca*r (compile ca*r))
;;; ERROR: In procedure scm-error:
;;; ERROR: unhandled constant #<procedure ca*r (x)>


(eval-when (expand load eval)

  (define (unlambda pred?)
    (if (func? pred? 'lambda)
        (caddr pred?)
        (list pred?)))

  (define-public (tm-add-condition var head body)
    (if (null? cur-conds) body
        `((if ,(and* (map unlambda cur-conds))
              ,(begin* body)
              ,(apply* 'former head)))))


  (define-public-macro (tm-define-overloaded head . body)
    (let* ((var (ca*r head))
           (nbody (tm-add-condition var head body))
           (nval (lambda* head nbody))
           (temp-module-name (module-name (current-module))))
      (if (ahash-ref tm-defined-table var)
          `(let ((former ,var))
             ;;(if (== (length (ahash-ref tm-defined-table ',var)) 1)
             ;;    (display* "Overloaded " ',var "\n"))
             ;;(display* "Overloaded " ',var "\n")
             ;;(display* "   " ',nval "\n")
             (module-define! texmacs-user ,var ,nval)
             (module-export! texmacs-user '(,var))
             (ahash-set! tm-defined-table ',var
                         (cons ',nval (ahash-ref tm-defined-table ',var)))
             (ahash-set! tm-defined-name ,var ',var)
	     (ahash-set! tm-defined-module ',var
		         (cons temp-module-name
			       (ahash-ref tm-defined-module ',var)))
             ,@(map property-rewrite cur-props))
          `(begin
             (when (nnull? cur-conds)
               (display* "warning: conditional master routine " ',var "\n")
               ;;(display* "   " ',nval "\n")
               )
             ;;(display* "Defined " ',var "\n")
             ;;(if (nnull? cur-conds) (display* "   " ',nval "\n"))
             ;;(display* "    " ',nval "\n")
             (module-define! texmacs-user ,var ,nval)
             (module-export! texmacs-user '(,var))
             (ahash-set! tm-defined-table ',var (list ',nval))
             (ahash-set! tm-defined-name ,var ',var)
	     (ahash-set! tm-defined-module ',var (list temp-module-name))
             ,@(map property-rewrite cur-props)))))

  (define-public (tm-define-sub head body)
    (if (and (pair? (car body)) (keyword? (caar body)))
        (let ((decl (tm-define-sub head (cdr body))))
	  (if (not (hash-ref define-option-table (caar body)))
	      (texmacs-error "tm-define-sub" "unknown option ~S" (caar body)))
	  ((hash-ref define-option-table (caar body)) (cdar body) decl))
        (cons 'tm-define-overloaded (cons head body))))


  (define-public-macro (tm-define head . body)
    (set! cur-conds '())
    (set! cur-props '())
    (tm-define-sub head body))

  ) ;; eval-when (expand load eval)

;;; My try:

;;; Notes: I am reading oop/goops.scm right now... I wonder if it's possible
;;;        and worth it to use GOOPS for this?
;;;
;;; <applicable>, <applicable-struct>, <generic>, <generic-with-setter>, ...
;;;
;;; A metaclass can be created that... ? Does it require a metaclass?
;;;
;;; Need to find out: What do the evaluator and virtual machine do when they
;;; encounter a <generic>, an <applicable>, etc...
;;;
;;; "tm-define" is sort of like a "define-method" method... for not a standard
;;; goops generic function method, but for one where dispatch happens via:
;;;
;;;   1. Arity and type signature are used first, then
;;;
;;;   2. The method that is returned will apply each "editor context
;;;      overloading" predication in turn until one returns #t, at which point
;;;      that is the method chosen.
;;;
;;;      So this means that that returned method must be either a (curried)
;;;      closure that carries with it the list of (predication . function)?
;;;      Or is that something that will be hidden inside of the method
;;;      dispatch set up by this thing's metaclass???
;;;
;;;  In either case... what the original code produces, in tm-defined-table,
;;;  is like... (see below, where I am choosing one I'm familiar with because
;;;  it's from my own program) And so it is a list of lambda forms, which
;;;  begin with an "if" statement, where if the test expression is true, the
;;;  body from within the originating "tm-define" is run, otherwise "former"
;;;  is invoked. Here I'm not sure how "former" gets defined. Are these
;;;  closures that "pretty-print" couldn't dump full information about for me?
;;;
;;;  Since the example is from TeXmacs with Guile 1.8, the lambda forms are
;;;  not compiled. I will compile them, as well as the predication forms, so
;;;  they will be pre-parsed and run quickly on the virtual machine.
;;;
;;;  "former" means to call on the next one in the list. Where is the state
;;;  information about where in the list it is being kept? There must be
;;;  closures, not mere lambda forms created via quasiquote.
;;;
;;;  "former" ends up being sort of a pointer to the next lambda form in the
;;;  list, and the predications are built-in, rather than being an external
;;;  thing... but at the user level, that kind of implementation detail
;;;  doesn't matter as much; the point is that when "former" is called, what
;;;  will happen? Where will it find out where in the list it is? [What if the
;;;  list with this lambda form as it's car is passed as the first argument,
;;;  an argument hidden from the user and used only internally?] But the
;;;  closure thing must still be true; each is constructed inside of it's own
;;;  module and within it's particular lexical environment where it was
;;;  instantiated, and so it can refer to module-local variables and functions
;;;  at will as well as globals and ones imported via use-modules, etc; and so
;;;  the way it knows the value of "former" ought to still work pretty much
;;;  the same way; the compiler will carry that environment information
;;;  through to the byte-code.
;;;
;;;  What if "former" was more like a tail-call optimized jump to the next one
;;;  in the list? Sort of a programatically built cond block, sort of like the
;;;  regexp-exec machine in tm-zotero.scm? Running a list, car is predication,
;;;  cdr is compiled function... or just compile the predication right in?
;;;
;;;  If they become lists of that nature, does the method resolution process
;;;  need to do anything put those into any kind of special order, or should
;;;  they be only in lexical order, as written in the source? The arity and
;;;  type dispatch won't matter? Should that even be possible? Well, if
;;;  there's more than one method signature for the same tm-define'd name,
;;;  maybe that's unintentional or a programmer error?  But once it's past the
;;;  arity and type signature based method dispatch, each separate
;;;  arity+type-signature one will have it's own (predicate . function) list?
;;;  Does it need this? What about keyword arguments with defaults, optional,
;;;  and rest arguments?
;;;
;;;  If the underlying implementation is the same as or very similar to the
;;;  present one, then I think that an explicit compilation step may be
;;;  required...
;;;
;;;  The way a tm-define-macro works, with it's $impl suffix...
;;;
;;{{{ Long example, "clipboard-cut" from tm-zotero.scm
#!
(clipboard-cut .
 ((lambda (which)
    (if (and (in-tm-zotero-style?)
             (is-during-tm-zotero-clipboard-cut?))
      (noop)
      (former which)))
  (lambda (which)
    (if (let ((st (selection-tree)))
          (and (in-tm-zotero-style?)
               (not (is-during-tm-zotero-clipboard-cut?))
               (inside-zcite? (cursor-tree))
               (inside-inactive? (cursor-tree))
               (not (has-zfield? st))
               (has-zsubCite? st)))
      (with-fluids
        ((fluid/is-during-tm-zotero-clipboard-cut? #t))
        (let* ((selection-t (selection-tree))
               (sel-zsubCite-ls
                 (tm-search selection-t is-zsubCite?))
               (zfield
                 (tree-search-upwards
                   (path->tree (cDr (selection-get-start)))
                   '(zcite)))
               (zfieldID (zfield-zfieldID zfield))
               (zfield-new-ID (generate-unique-zfieldID))
               (zfield-copy (tree-copy zfield))
               (zfield-copy-ID (generate-unique-zfieldID))
               (zsubCite-t-ls
                 (tm-search
                   (zfield-Text-t zfield-copy)
                   is-zsubCite?))
               (documentID (get-documentID))
               (dd (get-<document-data> documentID))
               (zfd-ls (document-zfield-zfd-ls dd))
               (zfd-key (string-append documentID zfieldID))
               (zfd (hash-ref
                      documentID+zfieldID-><zfield-data>-ht
                      zfd-key
                      #f))
               (layout-prefix
                 (or (zotero-style-citation-layout-prefix) ""))
               (layout-delimiter
                 (or (zotero-style-citation-layout-delimiter) ""))
               (layout-suffix
                 (or (zotero-style-citation-layout-suffix) ""))
               (suppress?
                 (and zfd
                      (zfd-Code-code-properties-suppress-trailing-punctuation
                        zfd)))
               (is-note? (zfield-IsNote? zfield))
               (formattedCitation
                 (and zfd
                      (zfd-Code-code-properties-formattedCitation zfd)))
               (formattedCitation
                 (if (string? formattedCitation)
                   formattedCitation
                   ""))
               (formattedCitation
                 (if (string-prefix? "{\\rtf " formattedCitation)
                   (substring
                     formattedCitation
                     6
                     (1- (string-length formattedCitation)))))
               (formattedCitation
                 (if (string-prefix? layout-prefix formattedCitation)
                   (substring
                     formattedCitation
                     (string-length layout-prefix)
                     (string-length formattedCitation))
                   formattedCitation))
               (formattedCitation
                 (if (string-suffix? layout-suffix formattedCitation)
                   (substring
                     formattedCitation
                     0
                     (- (string-length formattedCitation)
                        (string-length layout-suffix)))
                   formattedCitation))
               (formattedSubCite-ls
                 (split-string-by-substr
                   formattedCitation
                   layout-delimiter))
               (code-citationItems-ls
                 (or (and zfd (zfd-Code-code-citationItems-ls zfd))
                     '()))
               (zsubCite->*-alist
                 (if (and (== (length zsubCite-t-ls)
                              (length formattedSubCite-ls))
                          (== (length zsubCite-t-ls)
                              (length code-citationItems-ls)))
                   (map (lambda (a b c) (list a b c))
                        zsubCite-t-ls
                        formattedSubCite-ls
                        code-citationItems-ls)
                   '()))
               (keep-alist
                 (list-filter
                   zsubCite->*-alist
                   (lambda (elt)
                     (not (member (car elt) sel-zsubCite-ls)))))
               (cut-alist
                 (list-filter
                   zsubCite->*-alist
                   (lambda (elt) (member (car elt) sel-zsubCite-ls))))
               (b (buffer-new))
               (copy-zfd #f))
          (selection-cancel)
          (when zfd
                (buffer-set-body b zfield-copy)
                (buffer-pretend-autosaved b)
                (buffer-pretend-saved b)
                (unintern-ztHrefFromCiteToBib-for-cut
                  documentID
                  zfield)
                (set! (zfield-zfieldID zfield-copy)
                  zfield-copy-ID)
                (set! copy-zfd
                  (make-instance
                    <zfield-data>
                    #:zfd-tree
                    zfield-copy))
                (set! (zfd-Code-code-citationID zfd)
                  (tm-zotero-random-string))
                (set! (zfd-Code-code-citationID copy-zfd)
                  (tm-zotero-random-string))
                (set! (zfd-Code-code-properties-formattedCitation zfd)
                  (string-append
                    "{\\rtf "
                    (string-join
                      (map cadr keep-alist)
                      layout-delimiter)
                    (if suppress? "" layout-suffix)
                    "}"))
                (set! (zfd-Code-code-properties-formattedCitation
                        copy-zfd)
                  (string-append
                    "{\\rtf "
                    (string-join
                      (map cadr cut-alist)
                      layout-delimiter)
                    (if suppress? "" layout-suffix)
                    "}"))
                (set! (zfield-Text-t zfield)
                  (tm-zotero-UTF-8-str_text->texmacs
                    (zfd-Code-code-properties-formattedCitation zfd)
                    is-note?
                    #f))
                (set! (zfield-Text-t zfield-copy)
                  (tm-zotero-UTF-8-str_text->texmacs
                    (zfd-Code-code-properties-formattedCitation
                      copy-zfd)
                    is-note?
                    #f))
                (set! (zfd-Code-code-citationItems-ls zfd)
                  (map caddr keep-alist))
                (set! (zfd-Code-code-citationItems-ls copy-zfd)
                  (map caddr cut-alist))
                (set! (zfd-Code-code-properties-plainCitation zfd)
                  (zfield-Text zfield))
                (set! (zfd-Code-code-properties-plainCitation copy-zfd)
                  (zfield-Text zfield-copy))
                (set! (zfield-Code-is-modified?-flag zfield)
                  "false")
                (set! (zfield-Code-is-modified?-flag zfield-copy)
                  "false")
                (clipboard-set which zfield-copy)
                (clear-tree-pointer copy-zfd)
                (if (null? keep-alist)
                  (begin
                    (hash-remove!
                      documentID+zfieldID-><zfield-data>-ht
                      zfd-key)
                    (set! (document-zfield-zfd-ls dd)
                      (list-filter
                        zfd-ls
                        (lambda (elt) (not (eq? elt zfd)))))
                    (tree-assign! zfield "")
                    (and-with
                      inactive
                      (tree-search-upwards zfield (quote inactive))
                      (tree-assign! inactive "")))
                  (begin
                    (enqueue-delayed-integration-commands
                      documentID
                      (list zfield)
                      tm-zotero-affirmCitation)))
                (buffer-pretend-autosaved b)
                (buffer-pretend-saved b)
                (buffer-close b))))
      (former which)))
  (lambda (which)
    (if (and (in-tm-zotero-style?)
             (not (is-during-tm-zotero-clipboard-cut?))
             (has-zfield? (selection-tree)))
      (with-fluids
        ((fluid/is-during-tm-zotero-clipboard-cut? #t))
        (let* ((selection-t (selection-tree))
               (zfields (tm-search selection-t is-zfield?))
               (documentID (get-documentID))
               (dd (get-<document-data> documentID))
               (zfd-ls (document-zfield-zfd-ls dd))
               (zb-zfd-ls (document-zbibliography-zfd-ls dd))
               (new-zfield-zfd (document-new-zfield-zfd dd)))
          (map (lambda (zfield)
                 (let* ((zfieldID (zfield-zfieldID zfield))
                        (zfd-key (string-append documentID zfieldID))
                        (zfd (hash-ref
                               documentID+zfieldID-><zfield-data>-ht
                               zfd-key)))
                   (cond ((and zfd (not (eq? zfd new-zfield-zfd)))
                          (hash-remove!
                            documentID+zfieldID-><zfield-data>-ht
                            zfd-key)
                          (set! (document-zfield-zfd-ls dd)
                            (list-filter
                              zfd-ls
                              (lambda (elt) (not (eq? elt zfd)))))
                          (when (is-zbibliography? zfield)
                                (set! (document-zbibliography-zfd-ls dd)
                                  (list-filter
                                    zb-zfd-ls
                                    (lambda (elt) (not (eq? elt zfd))))))
                          (clear-tree-pointer zfd)
                          (unintern-ztHrefFromCiteToBib-for-cut
                            documentID
                            zfield))
                         (zfd
                          (let ((tp (tree-pointer zfd)))
                            (tm-zotero-format-error
                              "_BOLD__RED_clipboard-cut_RESET_: _RED_Cutting new zfield!_RESET_ _BOLD__RED_Fixme:_RESET_ Probably protocol breakdown; Restart Firefox and TeXmacs.")
                            (tree-assign!
                              zfield
                              (stree->tree
                                '(strong "{?? New Citation ??}")))
                            (clear-tree-pointer zfd)
                            (set! (document-new-zfield-zfd dd) #f))))))
               zfields)
          (cpp-clipboard-cut "none")
          (when (not (== which "none"))
                (clipboard-set which selection-t))))
      (former which)))
  (lambda l
    (if (in-sem-math?)
      (with cmd
            (lambda () (apply former l))
            (wrap-remove cmd #f))
      (apply former (cons* l))))
  (lambda (which)
    (if (and (inside? (quote slideshow)) (slide-range))
      (with (i1 i2)
            (slide-range)
            (with t
                  (tree-innermost slideshow-context?)
                  (clipboard-set which (selection-tree))
                  (tree-remove (tree-ref t 0) i1 (+ (- i2 i1) 1))
                  (if (>= i1 (tree-arity (tree-ref t 0)))
                    (tree-go-to t 0 (- i1 1) #:end)
                    (tree-go-to t 0 i1 #:start))))
      (former which)))
  (lambda (cb) (cpp-clipboard-cut cb)))
 )
!#
;;}}}
;;;
;;; Another good source for ideas and knowledge is in (ice-9 eval); in
;;; particular, look at how it calls the compile function in let bindings!
;;;


(define-syntax tm-define
  (lambda (whole-expr)
    (letrec* ((cur-conds '())
              
    (define (keyword-like? stx)
      (let ((dat (syntax->datum stx)))
        (and (symbol? dat)
             (eqv? (string-ref (symbol->string dat) 0) #\:))))

    (define (->keyword sym)
      (symbol->keyword (string->symbol (substring (symbol->string sym) 1))))

    (define (parse args mode require type synopsis returns note argument default proposals secure check-mark interactive balloon)
      (syntax-case args ()
        (()
         (let ((mode        (if (null? mode)        '() #`(#:mode        `#,mode)))
               (require     (if (null? require)     '() #`(#:require     `#,require)))
               (type        (if (null? type)        '() #`(#:type        `#,type)))
               (synopsis    (if (null? synopsis)    '() #`(#:synopsis    `#,synopsis)))
               (returns     (if (null? returns)     '() #`(#:returns     `#,returns)))
               (note        (if (null? note)        '() #`(#:note        `#,note)))
               (argument    (if (null? argument)    '() #`(#:argument    `#,argument)))
               (default     (if (null? default)     '() #`(#:default     `#,default)))
               (proposals   (if (null? proposals)   '() #`(#:proposals   `#,proposals)))
               (secure      (if (null? secure)      '() #`(#:secure      `#,secure)))
               (check-mark  (if (null? check-mark)  '() #`(#:check-mark  `#,check-mark)))
               (interactive (if (null? interactive) '() #`(#:interactive `#,interactive)))
               (balloon     (if (null? balloon)     '() #`(#:balloon     `#,balloon))))
           #`(#,@mode       #,@require     #,@type    #,@synopsis  #,@returns
              #,@note       #,@argument    #,@default #,@proposals #,@secure
              #,@check-mark #,@interactive #,@balloon)))
        ;; The user wanted #:foo, but wrote :foo. Fix it.
        (((kwsym . kwargs) . args)
         (keyword-like? #'kwsym)
         (parse #`((#,(->keyword (syntax->datum #'kwsym)) . kwargs) . args)
                mode require type synopsis returns note argument
                default proposals secure check-mark interactive
                balloon))
        (((kw . kwargs) . args)
         (not (keyword? (syntax->datum #'kw)))
         (syntax-violation 'define-module "expected keyword arg" x #'kw))

        ((#:mode . args)
         )
        ((kw val . args)
         (syntax-violation 'define-module "unknown keyword or bad argument"
                           #'kw #'val))))

    (define (define-overloaded head . body)
      
      )


    (syntax-case whole-expr ()

      ((_ (name bvar ...) (kw . kwargs) ... body ...)
       (and
        (and-map symbol? (syntax->datum #'(name bvar ...)))
        (or (keyword? (syntax->datum #'kw))
            (keyword-like? #'kw)))
       (with-syntax (((parsed-kwargs ...)
                      (parse #'((kw . kwargs) ...)
                             '() '() '() '() '()
                             '() '() '() '() '()
                             '() '() '())))
         (let loop ((l (parsed-kwargs ...)))
           (case (caar l)
             ((#:mode)
              )
             ((#:require)
              )
             ((#:type)
              )
             ((#:synopsis)
              )
             ((#:
         #'(eval-when (expand load eval)
             )))

      ((_ (name bvar ...) body ...)
       (and-map symbol? (syntax->datum #'(name bvar ...)))
       #'(eval-when (expand load eval)
           (
            )
           )))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Overloaded macros with properties
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when (expand load eval compile)

  (define-public (tm-macroify head)
    (if (pair? head)
        (cons (tm-macroify (car head)) (cdr head))
        (string->symbol (string-append (symbol->string head) "$impl"))))

  (define-public-macro (tm-define-macro head . body)
    (with macro-head (tm-macroify head)
      ;; (display* (ca*r head) "\n")
      ;; (display* "   " `(tm-define ,macro-head ,@body) "\n")
      ;; (display* "   " `(define-public-macro ,head
      ;;                    ,(apply* (ca*r macro-head) head)) "\n")
      `(begin
         (tm-define ,macro-head ,@body)
         (with-module texmacs-user
           (define-macro ,head
             ,(apply* (ca*r macro-head) head))
           (export-syntax ,head)))))

  ) ;; eval-when (expand load eval)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Associating extra properties to existing function symbols
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when (expand load eval compile)

  (define-public (tm-property-sub head body)
    (if (null? body)
        (cons 'tm-property-overloaded (cons head body))
        (let ((decl (tm-property-sub head (cdr body))))
	  ((hash-ref define-option-table (caar body)) (cdar body) decl))))

  (define-public-macro (tm-property head . body)
    (set! cur-conds '())
    (set! cur-props '())
    (tm-property-sub head body))

  (define-public-macro (tm-property-overloaded head . body)
    `(begin
       ,@(map property-rewrite cur-props)))

  ) ;; eval-when (expand load eval)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Lazy function declations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when (expand load eval)

  (define lazy-define-table (make-ahash-table))

  (define-public (not-define-option? item)
    (not (and (pair? item) (keyword? (car item)))))

  (define-public (lazy-define-one module opts name)
    (let* ((old (ahash-ref lazy-define-table name))
	   (new (if old (cons module old) (list module))))
      (ahash-set! lazy-define-table name new))
    (with name-star (string->symbol (string-append (symbol->string name) "*"))
      `(when (not (defined? ',name))
         (tm-define (,name . args)
           ,@opts
           (let* ((m (resolve-module ',module))
                  (p (resolve-interface texmacs-user))
                (r (module-ref p ',name #f)))
           (if (not r)
               (texmacs-error "lazy-define"
                              ,(string-append "Could not retrieve "
                                              (symbol->string name))))
           (apply r args))))))

  (define-public-macro (lazy-define module . names)
    (receive (opts real-names) (list-break names not-define-option?)
      `(begin
         ,@(map (lambda (name) (lazy-define-one module opts name)) names))))

  (define-public (lazy-define-force name)
    (if (procedure? name) (set! name (procedure-name name)))
    (let* ((im (ahash-ref lazy-define-table name))
	   (modules (if im im '())))
      (ahash-remove! lazy-define-table name)
      (for-each module-provide modules)))

  ) ;; eval-when (expand load eval)
