;;;;;;
;;;
;;; Development exploration for new tm-define implementation for Guile 2.2
;;; based TeXmacs.
;;;
;;;  (Same licence as TeXmacs)
;;;
;;;
;;; Todo: Just as in GOOPS there's a way to turn a normal function into a
;;;       generic method, perhaps a tm-define of an already defined function
;;;       ought to wrap that function inside of a <tm-defined> / <%tm-def>,
;;;       thus making it extensible?
;;;

(define-module (kernel texmacs tm-define)
  ;;
  #:use-module (kernel texmacs tm-define definitions)
  #:use-module (oop goops)
  ;;
  #:use-module (ice-9 pretty-print)
  #:use-module (ice-9 format)
  ;;
  #:duplicates (merge-generics merge-accessors replace warn-override-core warn last))


;;
;; module-defined-hook ought to take care of this:
;;
;; (default-duplicate-binding-handler
;;     '(merge-generics merge-accessors replace warn-override-core warn last))


(define-syntax the-tm-defs-module
  (identifier-syntax
   (resolve-module '(kernel texmacs tm-define definitions))))

(define-syntax the-tm-defs-interface
  (identifier-syntax
   (resolve-interface '(kernel texmacs tm-define definitions))))


;;;
;;; Normally defined inside of TeXmacs... But I like (ice-9 format) better
;;; anyway.
;;;
(define-public (display* . l)
  (for-each display l))

(define-public (write* . l)
  (for-each write l))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Because of the way that Guile 2.2 looks up the value of a symbol, the
;;; TeXmacs guile module namespaces shall be arranged such that the (kernel
;;; texmacs tm-define definitions) interface is first in the (module-uses
;;; (resolve-module '(guile))) list, so that when lookup fails in the
;;; (current-module)'s obarray, and falls back to each module in the
;;; (module-uses (current-module)), which always contains the (guile) module,
;;; and then doesn't find it in that obarray, it will look in the (module-uses
;;; (resolve-module '(guile))) and then into each of those obarrays, it will
;;; reduce the number of steps until the lookup is complete;
;;;
;;; When tm-define is called on to define a new function, for which there is
;;; not already one of the same name being overloaded, it makes an instance of
;;; a <tm-defined> object, as well as an instance of a <%tm-def> object, which
;;; is imediately stored inside of the <tm-defined> as the initial, or base
;;; item in the <%tm-def>s slot. Because both <%tm-def> and <tm-defined>
;;; inherit from <applicable>, when a scheme form is evaluated and the symbol
;;; in the function position is an <applicable>, the function in its' procedure
;;; slot is invoked. (procedure? an-instance-of-a-<tm-defined>) => #t
;;;
;;; The name of the new tm-defined function is used to construct the name for
;;; the <tm-defined> instance that will be put into the (kernel texmacs
;;; tm-define definitions) module namespace. It's name is simply the given name
;;; with -<tm-defined> symbol-appended to it. So evaluating that symbol returns
;;; the <tm-defined> instance object.
;;;
;;; In the same namespace, right next to it, will be defined a symbol named
;;; only the name given for this function in the tm-define invokation. It will
;;; be symbol-syntax that expands to (@@ (kernel texmacs tm-define definitions)
;;; name-<tm-defined>), so that after syntax expansion has taken place, the
;;; compiler or evaluator can look straight in to the <tm-defined> instance,
;;; with no clicking through obarrays to locate it at run-time.
;;;
;;; For a <tm-defined>, the procedure will always pass control to the procedure
;;; of the <%tm-def> instance that is the car of the <%tm-def>s list.
;;;
;;; The procedure of a <%tm-def> is formed using the body of the call to the
;;; tm-defined, as well as the value of any (#:mode ...) or (#:require ...)
;;; statements appearing before the body of the tm-define. #:mode is folded
;;; into #:require, and when there's more than one, they are automatically
;;; placed inside of an (and ...) in the same lexical order they appear in. The
;;; resulting function is #'(lambda (formals ...) (if req (begin body ...)
;;; (former formals ...))) form. The function called former is magically bound
;;; to the next item from the top in the <%tm-def>s list.
;;;
;;; So when the top <%tm-def> is called, if its' require form returns #t, its'
;;; body is run, and it returns, unless the body explicitly calls on (former
;;; ...), in which case it calls that at the point it's called, returning its'
;;; value, perhaps for you to munge or test or whatever before returning. If
;;; its' require form returns #f, then (former ...) is automatically
;;; called. Thus, the #:require forms are used to determine which of the
;;; <%tm-def>s is to be executed at that point in time and within the editor
;;; context tested by the forms in the #:require block.
;;;
;;;;;;
;;;
;;; It has been suggested that instead of always instantiating the <tm-defined>
;;; objects in the (kernel texmacs tm-define definitions) module, each newly
;;; created one can be instantiated in the module where it is being
;;; defined. The thing is that these tm-defined functions must be effectively
;;; globally visible anyway. The syntax expansion for the body of the <%tm-def>
;;; procedure will happen in the lexical context of the module the definition
;;; is created in, despite that the <tm-defined> instance itself will be
;;; primarily referenced via a variable instantiated in the (kernel texmacs
;;; tm-define definitions) module.
;;;
;;; Also, the lookup to resolve the value of the "name" for the tm-define'd
;;; function will, by design, go through the (guile) module's module-uses list,
;;; so that they will be globally visible, from every module, without needing
;;; to explicitly name (kernel texmacs tm-define definitions) in a use-modules
;;; statement in normal texmacs guile modules. I think that not every texmacs
;;; guile module's symbols should be made globally visible that way, and thus,
;;; they are not all made globally visible; but when there's a tm-define, that
;;; symbol must be globally visible for TeXmacs to function properly.
;;;
;;; Note that by "globally visible" I do not mean merely exported from the
;;; module it's defined in, though that is one of the requisites; I mean that
;;; also the module's interface has been added to the module-uses of the base
;;; (guile) module, making any exported symbol fully globally visible.
;;;
;;;;;;

;;;;;;
;;;
;;; Saving mode and require here for introspection, I guess; wasteful?
;;; They will already be incorporated into the procedure...
;;;
(define-class <%tm-def> (<object> <applicable>)
  ;;
  (procedure #:init-keyword #:<%tm-def>-procedure)
  ;;
  (formals #:init-keyword #:<%tm-def>-formals #:init-value '())
  (require #:init-keyword #:<%tm-def>-require #:init-value '())
  ;;
  ;; (<%tm-def>-module #:init-keyword #:<%tm-def>-module)
  ;;
  #:metaclass <applicable-struct-class>)

(export <%tm-def>)


(define-method (write (o <%tm-def>) file)
  (format file
    "#<<%tm-def>~@{~@?~}>"
    " ~x" (object-address o)
    " procedure ~s" (slot-ref o 'procedure)
    " formals ~s"   (slot-ref o 'formals)
    " require ~s"   (slot-ref o 'require)))


(define-class <tm-defined> (<object> <applicable>)
  ;;
  (procedure   #:init-keyword #:<tm-defined>-procedure)
  ;; saving formals for introspection?
  (formals     #:init-keyword #:<tm-defined>-formals     #:init-value '())
  ;;
  (<%tm-def>s  #:init-keyword #:<tm-defined>-<%tm-def>s  #:init-value '())
  ;;
  (type        #:init-keyword #:<tm-defined>-type        #:init-value '())
  (synopsis    #:init-keyword #:<tm-defined>-synopsis    #:init-value '())
  (returns     #:init-keyword #:<tm-defined>-returns     #:init-value '())
  (note        #:init-keyword #:<tm-defined>-note        #:init-value '())
  (argument    #:init-keyword #:<tm-defined>-argument    #:init-value '())
  (default     #:init-keyword #:<tm-defined>-default     #:init-value '())
  (proposals   #:init-keyword #:<tm-defined>-proposals   #:init-value '())
  (secure      #:init-keyword #:<tm-defined>-secure      #:init-value '())
  (check-mark  #:init-keyword #:<tm-defined>-check-mark  #:init-value '())
  (interactive #:init-keyword #:<tm-defined>-interactive #:init-value '())
  (balloon     #:init-keyword #:<tm-defined>-balloon     #:init-value '())
  ;;
  ;; (<tm-defined>-module #:init-keyword #:<tm-defined>-module)
  ;;
  #:metaclass <applicable-struct-class>)

(export <tm-defined>)


(define-method (write (o <tm-defined>) file)
  (format file
    "#<<tm-defined>~@{~@?~}>"
    " ~x" (object-address o)
    " procedure ~s"   (slot-ref o 'procedure)
    " formals ~s  "   (slot-ref o 'formals)
    " <%tm-def>s ~s"  (slot-ref o '<%tm-def>s)
    " type ~s"        (slot-ref o 'type)
    " synopsis ~s"    (slot-ref o 'synopsis)
    " returns ~s"     (slot-ref o 'returns)
    " note ~s"        (slot-ref o 'note)
    " argument ~s"    (slot-ref o 'argument)
    " default ~s"     (slot-ref o 'default)
    " proposals ~s"   (slot-ref o 'proposals)
    " secure ~s"      (slot-ref o 'secure)
    " check-mark ~s"  (slot-ref o 'check-mark)
    " interactive ~s" (slot-ref o 'interactive)
    " balloon ~s"     (slot-ref o 'balloon)))


;;;;;;
;;;
;;; Guile 1.8 -> 2.2 Devel notes:
;;;
;;; In TeXmacs 1.99 guile 1.8, I see no calls to property-set! where conds* is
;;; anything but the empty list. In the property function there, the position
;;; for conds in the call to ctx-resolve is always #f. I think that the conds
;;; thing there is only partially implemented.
;;;
;;; Tentatively, it looks like it may be meant for contextual overloading of
;;; the return value of lookups??? But I think so far none are overloaded
;;; because the code to make it possible is not present... thus the conds
;;; variable in much of that is not used by anything... ?
;;;
;;; Todo Must double check that with grep-find of entire code looking for uses
;;;      of ctx-insert and ctx-resolve.
;;;
(define-method (property-set! (var <tm-defined>) prop what . conds*)
  (slot-set! var (keyword->symbol prop) what))

(define-method (property-set! (var <%tm-def>) prop what . conds*)
  (slot-set! var (keyword->symbol prop) what))

(export property-set!)

;;;;;;
;;;
;;; If the and-apply in ctx-resolve turns out to matter... Will need to make
;;; #:getter and #:setter or #:accessor functions for the various slots, or a
;;; post-processing, or an #:allocation #:virtual slot that does what
;;; ctx-resolve does using and-apply...
;;;
(define-method (property (var <tm-defined>) prop)
  (slot-ref var (keyword->symbol prop)))

(define-method (property (var <%tm-def>) prop)
  (slot-ref var (keyword->symbol prop)))

(export property)



(define-syntax-parameter former
  (lambda (x)
    (syntax-violation 'former "former used outside of an overloading tm-define" x)))

(export former)



(define (syntax-symbol-append ctx sym-prefix syntax-name sym-suffix)
  (datum->syntax
   ctx
   (cond
     ((and sym-prefix sym-suffix)
      (symbol-append sym-prefix (syntax->datum syntax-name) sym-suffix))
     (sym-prefix
      (symbol-append sym-prefix (syntax->datum syntax-name)))
     (sym-suffix
      (symbol-append (syntax->datum syntax-name) sym-suffix)))))



(define-syntax build-<%tm-def>-procedure
  (lambda (syntax-x)
    (with-ellipsis :::
      (syntax-case syntax-x ()
        ((_ (ign) (fmls :::) (#:no-require) (bod :::))
         #'(lambda (fmls :::) (begin bod :::)))
        ((_ (#:has-former) (fmls :::) (req) (bod :::))
         #'(lambda (fmls :::)
             (if req (begin bod :::) (former fmls :::))))
        ((_ (#:no-former) (fmls :::) (req) (bod :::))
         ;; base <%tm-def> has no former:
         #`(lambda (fmls :::)
             (if req (begin bod :::))))))))




(define-syntax _%_make-new-<tm-defined>-with-base-<%tm-def>
  (lambda (x)
    (with-ellipsis !!!
      (syntax-case x ()
        ((_ name #:<tm-defined>-formals (formals !!!) #:<%tm-def>-require (require) #:<%tm-def>-body (body !!!) rest !!!)
         (with-syntax ((name-<tm-defined>           (syntax-symbol-append
                                                     x #f #'name '-<tm-defined>))
                       (name-<tm-defined>-procedure (syntax-symbol-append
                                                     x #f #'name '-<tm-defined>-procedure))
                       (name-<%tm-def>              (syntax-symbol-append
                                                     x #f #'name '-<%tm-def>))
                       (name-<%tm-def>-procedure    (syntax-symbol-append
                                                     x #f #'name '-<%tm-def>-procedure))
                       )
           ;; former is not defined for the first <%tm-def> item:
           #'(let* ((name-<%tm-def>-procedure (build-<%tm-def>-procedure (#:no-former)
                                                                         (formals !!!)
                                                                         (require)
                                                                         (body !!!)))
                    (name-<%tm-def> (make-instance <%tm-def>
                                                   #:<%tm-def>-formals   '(formals !!!)
                                                   #:<%tm-def>-require   'require
                                                   #:<%tm-def>-procedure name-<%tm-def>-procedure
                                                   rest !!!))
                    (name-<tm-defined> (make-instance <tm-defined>
                                                      #:<tm-defined>-formals    '(formals !!!)
                                                      #:<tm-defined>-<%tm-def>s (list name-<%tm-def>)
                                                      rest !!!))
                    (name-<tm-defined>-procedure
                     ;;
                     ;; The procedure slot of an <applicable> is invoked when
                     ;; the instance object is found in function position
                     ;; during evaluation. It is not quite the same thing as a
                     ;; slot accessor access, via either slot-ref or a class
                     ;; defined getter, setter, or accessor method, since those
                     ;; must be handed an instance object as their first
                     ;; argument, where as the procedure in the procedure slot
                     ;; of an <applicable> does not automatically recieve a
                     ;; "this" instance as any argument. [This was determined
                     ;; by creating an instance and setting its' procedure slot
                     ;; to (lambda x (write x) (newline)), and then calling it
                     ;; with various arguments.]
                     ;;
                     ;; The procedure of a <tm-defined> must know which
                     ;; instance it lives in, in order to look up the car of
                     ;; the <%tm-def>s slot at run-time in order to invoke
                     ;; it... The simplest and most obvious means of achieving
                     ;; this is to lexically bind the location of this
                     ;; <tm-defined> instance for use inside of its' procedure.
                     ;;
                     ;; It is also the case that each instance of a
                     ;; <tm-defined> has its' own separate formals list, and so
                     ;; instead of trying to make there be one class-wide
                     ;; function that deals with that, this program will simply
                     ;; define one for each instance.
                     ;;
                     (let ((this-<tm-defined> name-<tm-defined>))
                       ;; closure over this-<tm-defined>
                       (lambda (formals !!!)
                         ;; car of that list at run-time determines top
                         ;; tm-def.
                         ((car (slot-ref this-<tm-defined> '<%tm-def>s))
                          formals !!!))))
                    )
               (slot-set! name-<tm-defined> 'procedure name-<tm-defined>-procedure)
               name-<tm-defined>)))))))



(define-syntax _%_make-new-<%tm-def>
  (lambda (x)
    (with-ellipsis !!!
      (syntax-case x ()
        ((_ name #:<tm-defined>-formals (formals !!!) #:<%tm-def>-require (require) #:<%tm-def>-body (body !!!) rest !!!)
         (with-syntax ((name-<%tm-def>           (syntax-symbol-append
                                                  x #f #'name '-<%tm-def>))
                       (name-<%tm-def>-procedure (syntax-symbol-append
                                                  x #f #'name '-<%tm-def>-procedure))
                       )
           ;; former is defined for the overloading <%tm-def> items:
           #'(let* ((name-<%tm-def>-procedure (build-<%tm-def>-procedure (#:has-former)
                                                                         (formals !!!)
                                                                         (require)
                                                                         (body    !!!)))
                    (name-<%tm-def> (make-instance <%tm-def>
                                                   #:<%tm-def>-formals   '(formals !!!)
                                                   #:<%tm-def>-require   'require
                                                   #:<%tm-def>-procedure name-<%tm-def>-procedure
                                                   rest !!!)))
               name-<%tm-def>)))))))



;;;;;;
;;;
;;; This is a slightly modified copy of the stock identifier-syntax macro.
;;;
(define-syntax tm-defs-identifier-syntax
  (lambda (xx)
    (syntax-case xx ()
      ((_ e)
       #'(make-variable-transformer
          (lambda (x)
            #((macro-type . variable-transformer))
            (syntax-case x (set!)
              ((set! id val)
               #'(syntax-error "set! not supported for this type"))
              ((id x (... ...))
               #'(e x (... ...)))
              (id
               (identifier? #'id)
               #'e))))))))


;;;
;;; Todo: a #:secure flag can only be set at the initial definition point... is
;;; that right? So it's part of the <tm-defined> not part of the <%tm-def>?
;;;

;;;
;;; Some properties belong to the <tm-defined> and others to the <%tm-def>.
;;;
;;;

(define-syntax tm-define
  (lambda (x)

    (define (keyword-like? stx)
      (let ((dat (syntax->datum stx)))
        (and (symbol? dat)
             (eqv? (string-ref (symbol->string dat) 0) #\:))))

    (define (->keyword sym)
      (symbol->keyword (string->symbol (substring (symbol->string sym) 1))))

    (define (parse parse-body
                   formals require body
                   type synopsis returns note argument default
                   proposals secure check-mark interactive balloon)
      (syntax-case parse-body ()
        ((#:parse-finished)
         (let ((formals     #`(#:<tm-defined>-formals (#,@formals)))
               ;;
               (require     (let ((require-datum (syntax->datum require)))
                             (cond
                              ((null? require-datum)
                               #'(#:<%tm-def>-require (#:no-require)))
                              ((> 1 (length require-datum))
                               #`(#:<%tm-def>-require (and #,@require)))
                              (else
                                #`(#:<%tm-def>-require (#,@require))))))
               ;;
               (body        #`(#:<%tm-def>-body (#,@body)))
               ;;
               (type        (if (null? (syntax->datum type))        #'() #`(#:<tm-defined>-type        '#,@type)))
               (synopsis    (if (null? (syntax->datum synopsis))    #'() #`(#:<tm-defined>-synopsis    '#,@synopsis)))
               (returns     (if (null? (syntax->datum returns))     #'() #`(#:<tm-defined>-returns     '#,@returns)))
               (note        (if (null? (syntax->datum note))        #'() #`(#:<tm-defined>-note        '#,@note)))
               (argument    (if (null? (syntax->datum argument))    #'() #`(#:<tm-defined>-argument    '#,@argument)))
               (default     (if (null? (syntax->datum default))     #'() #`(#:<tm-defined>-default     '#,@default)))
               ;;
               (proposals   (if (null? (syntax->datum proposals))   #'() #`(#:<tm-defined>-proposals   '#,@proposals)))
               (secure      (if (null? (syntax->datum secure))      #'() #`(#:<tm-defined>-secure      '#,@secure)))
               (check-mark  (if (null? (syntax->datum check-mark))  #'() #`(#:<tm-defined>-check-mark  '#,@check-mark)))
               (interactive (if (null? (syntax->datum interactive)) #'() #`(#:<tm-defined>-interactive '#,@interactive)))
               (balloon     (if (null? (syntax->datum balloon))     #'() #`(#:<tm-defined>-balloon     '#,@balloon)))
               )
           #`(#,@formals #,@require #,@body
              #,@type #,@synopsis #,@returns #,@note #,@argument #,@default
              #,@proposals #,@secure #,@check-mark #,@interactive #,@balloon))
           )
        ;; The user wanted #:foo but wrote :foo. Fix it.
        (((sym forms ...) rest0 rest1 ...)
         (keyword-like? #'sym) ; keyword-like?=>#t->(symbol?=>#f and keyword?=>#f)
         (parse #`((#,(->keyword (syntax->datum #'sym)) forms ...) rest0 rest1 ...)
                formals require body
                type synopsis returns note argument default
                proposals secure check-mark interactive balloon)
         )
        (((#:mode predicate-fn) rest0 rest1 ...)
         (not (identifier? #'predicate-fn))
         #'(syntax-violation 'tm-define "(#:mode predicate-fn) expected:" parse-body)
         )
        (((#:mode predicate-fn) rest0 rest1 ...)
         (parse #'(rest0 rest1 ...) formals #`(#,@require (predicate-fn)) body
                type synopsis returns note argument default
                proposals secure check-mark interactive balloon)
         )
        (((#:require forms ...) rest0 rest1 ...)
         (parse #'(rest0 rest1 ...) formals #`(#,@require forms ...) body
                type synopsis returns note argument default
                proposals secure check-mark interactive balloon)
         )
        (((#:type forms ...) rest0 rest1 ...)
         (parse #'(rest0 rest1 ...) formals require body
                #`(#,@type (forms ...)) synopsis returns note argument default
                proposals secure check-mark interactive balloon)
         )
        (((#:synopsis forms ...) rest0 rest1 ...)
         (parse #'(rest0 rest1 ...) formals require body
                type #`((forms ...)) returns note argument default
                proposals secure check-mark interactive balloon)
         )
        (((#:returns forms ...) rest0 rest1 ...)
         (parse #'(rest0 rest1 ...) formals require body
                type synopsis #`((forms ...)) note argument default
                proposals secure check-mark interactive balloon)
         )
        (((#:note forms ...) rest0 rest1 ...)
         (parse #'(rest0 rest1 ...) formals require body
                type synopsis returns #`(#,@note (forms ...)) argument default
                proposals secure check-mark interactive balloon)
         )
        (((#:argument forms ...) rest0 rest1 ...)
         (parse #'(rest0 rest1 ...) formals require body
                type synopsis returns note #`(#,@argument (forms ...)) default
                proposals secure check-mark interactive balloon)
         )
        (((#:default forms ...) rest0 rest1 ...)
         (parse #'(rest0 rest1 ...) formals require body
                type synopsis returns note argument #`((forms ...))
                proposals secure check-mark interactive balloon)
         )
        (((#:proposals forms ...) rest0 rest1 ...)
         (parse #'(rest0 rest1 ...) formals require body
                type synopsis returns note argument default
                #`((forms ...)) secure check-mark interactive balloon)
         )
        (((#:secure forms ...) rest0 rest1 ...)
         (parse #'(rest0 rest1 ...) formals require body
                type synopsis returns note argument default
                proposals #'(#t) check-mark interactive balloon)
         )
        (((#:check-mark forms ...) rest0 rest1 ...)
         (parse #'(rest0 rest1 ...) formals require body
                type synopsis returns note argument default
                proposals secure #`((forms ...)) interactive balloon)
         )
        (((#:interactive forms ...) rest0 rest1 ...)
         (parse #'(rest0 rest1 ...) formals require body
                type synopsis returns note argument default
                proposals secure check-mark #`((#t)) balloon)
         )
        (((#:balloon forms ...) rest0 rest1 ...)
         (parse #'(rest0 rest1 ...) formals require body
                type synopsis returns note argument default
                proposals secure check-mark interactive #`((forms ...)))
         )
        (((kw forms ...) rest0 rest1 ...)
         (keyword? (syntax->datum #'kw))
         #'(syntax-violation 'tm-define "Unknown keyword " kw)
         )
        ((forms ...)
         (parse #'(#:parse-finished) formals require #'(forms ...) ; body
                type synopsis returns note argument default
                proposals secure check-mark interactive balloon))))


    (syntax-case x ()
      ((_ (name arg ...) body ...)
       (and (identifier? #'name)
            (not (module-defined? the-tm-defs-module (syntax->datum #'name))))
       (begin
         (with-syntax ((name-<tm-defined>           (syntax-symbol-append
                                                     x #f #'name '-<tm-defined>))
                       (name-<tm-defined>-procedure (syntax-symbol-append
                                                     x #f #'name '-<tm-defined>-procedure))
                       (name-<%tm-def>              (syntax-symbol-append
                                                     x #f #'name '-<%tm-def>))
                       (name-<%tm-def>-procedure    (syntax-symbol-append
                                                     x #f #'name '-<%tm-def>-procedure))
                       (name #'name))
           #`(letrec* ((tm-defs-module the-tm-defs-module)
                       (name-<tm-defined>
                        (_%_make-new-<tm-defined>-with-base-<%tm-def>
                         name #,@(parse #'(body ...) ; args
                                        #'(arg ...) ; formals
                                        #'() ; require
                                        #'() ; body
                                        #'() ; type
                                        #'() ; synopsis
                                        #'() ; returns
                                        #'() ; note
                                        #'() ; argument
                                        #'() ; default
                                        #'() ; proposals
                                        #'() ; secure
                                        #'() ; check-mark
                                        #'() ; interactive
                                        #'() ; balloon
                                        ))))
               (module-define! tm-defs-module
                               'name-<tm-defined>
                               name-<tm-defined>)
               (module-define! tm-defs-module
                               'name
                               (make-syntax-transformer
                                'name 'macro
                                (tm-defs-identifier-syntax
                                 (@@ (kernel texmacs tm-define definitions)
                                     name-<tm-defined>))))
               (module-export! tm-defs-module '(name))
               name-<tm-defined>)))
       )
      ((_ (name arg ...) body ...)
       (and (identifier? #'name)
            (module-defined? the-tm-defs-module (syntax->datum #'name)))
       (begin
         (with-syntax ((name-<tm-defined> (syntax-symbol-append
                                           x #f #'name '-<tm-defined>))
                       (name-<%tm-def>    (syntax-symbol-append
                                           x #f #'name '-<%tm-def>))
                       (name #'name))
           #`(letrec* ((this-tm-defined (@@ (kernel texmacs tm-define definitions) name-<tm-defined>))
                       (_%_former (car (slot-ref this-tm-defined '<%tm-def>s))))
               (syntax-parameterize ((former (syntax-rules ()
                                               ((former vals (... ...))
                                                (_%_former vals (... ...))))))
                 (let ((name-<%tm-def>
                        (_%_make-new-<%tm-def>
                         name #,@(parse #'(body ...) ; args
                                        #'(arg ...) ; formals
                                        #'() ; require
                                        #'() ; body
                                        #'() ; type
                                        #'() ; synopsis
                                        #'() ; returns
                                        #'() ; note
                                        #'() ; argument
                                        #'() ; default
                                        #'() ; proposals
                                        #'() ; secure
                                        #'() ; check-mark
                                        #'() ; interactive
                                        #'() ; balloon
                                        ))))
                   (slot-set! this-tm-defined '<%tm-def>s
                              (cons name-<%tm-def> (slot-ref this-tm-defined '<%tm-def>s)))
                   this-tm-defined)))))))))

(export-syntax tm-define)

;;; (put 'with-ellipsis 'scheme-indent-function 0)
