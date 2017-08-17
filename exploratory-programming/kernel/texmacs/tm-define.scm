;;;
;;;
;;; Development exploration for new tm-define implementation for Guile 2.2
;;; based TeXmacs.
;;;
;;;

(define-module (kernel texmacs tm-define)
  #:use-module (kernel texmacs tm-define definitions)
  #:use-module (oop goops)
  #:use-module (oop goops simple))


(define-public the-tm-defs-module
  (resolve-module '(kernel texmacs tm-define definitions)))


;;;;;;;
;;;
;;; Saving mode and require here for introspection, I guess; wasteful?
;;; They will already be incorporated into the procedure...
;;;
(define-class <%tm-def> (<object> <applicable>)
  (procedure #:init-keyword #:tm-def-procedure)
  ;;
  (formals #:init-keyword #:tm-def-formals #:init-value '())
  (require #:init-keyword #:tm-def-require #:init-value '())
  ;;
  #:metaclass <applicable-struct-class>)

(export <%tm-def>)


(define-class <tm-defined> (<object> <applicable>)
  procedure
  ;;
  (tm-defs     #:init-keyword #:tm-def-tm-defs     #:init-value '())
  ;;
  (type        #:init-keyword #:tm-def-type        #:init-value '())
  (synopsis    #:init-keyword #:tm-def-synopsis    #:init-value '())
  (returns     #:init-keyword #:tm-def-returns     #:init-value '())
  (note        #:init-keyword #:tm-def-note        #:init-value '())
  (argument    #:init-keyword #:tm-def-argument    #:init-value '())
  (default     #:init-keyword #:tm-def-default     #:init-value '())
  (proposals   #:init-keyword #:tm-def-proposals   #:init-value '())
  (secure      #:init-keyword #:tm-def-secure      #:init-value '())
  (check-mark  #:init-keyword #:tm-def-check-mark  #:init-value '())
  (interactive #:init-keyword #:tm-def-interactive #:init-value '())
  (balloon     #:init-keyword #:tm-def-balloon     #:init-value '())
  ;;
  #:metaclass <applicable-struct-class>)

(export <tm-defined>)


(define-syntax-parameter former
  (lambda (x)
    (syntax-violation 'former "former used outside of an overloading tm-define" x)))

(export-syntax former)


(define-syntax inline-make-new-<tm-defined>-with-base-<%tm-def>
  (lambda (x)
    (syntax-case x ()
      ((_ name #:tm-def-formals (formals) #:tm-def-require (require) #:tm-def-body (body) . rest)
       ;; former is not defined for the first <%tm-def> item.
       (with-syntax ((name-<%tm-def> (datum->syntax
                                      x
                                      (symbol-append
                                       (syntax->datum #'name)
                                       '-<%tm-def>)))
                     (maybe-if
                      (lambda (x)
                        (syntax-case x ()
                          ((_ (formals) (require) . body)
                           (not (eqv? '() (syntax->datum #'require)))
                           #'(if require (begin body)))
                          ((_ (formals) (require) . body)
                           (eqv? '() (syntax->datum #'require))
                           #'(begin body))))))
         #'(let* ((name-<%tm-def> (lambda (formals)
                                      (maybe-if (formals) (require) body)))
                  (tm-def (make-instance <%tm-def>
                                         #:tm-def-formals formals
                                         #:tm-def-require require
                                         #:tm-def-procedure tm-def-procedure))
                  (new-tm-defined (make-instance <tm-defined>
                                                 #:tm-def-tm-defs (list tm-def)
                                                 rest))
                  (procedure (let ((this-tm-defined new-tm-defined))
                               (lambda (formals)
                                 ((car (slot-ref this-tm-defined 'tm-defs))
                                  formals)))))
             (slot-set! new-tm-defined 'procedure procedure)
             new-tm-defined))))))


(define-syntax inline-make-new-<%tm-def>
  (lambda (x)
    (syntax-case x ()
      ((_ #:tm-def-formals (formals) #:tm-def-require (require) #:tm-def-body (body) . rest)
       ;; former is defined for the overloading <%tm-def> items.
       (with-syntax ((name-<%tm-def> (datum->syntax
                                      x
                                      (symbol-append
                                       (syntax->datum #'name)
                                       '-<%tm-def>)))
                     (maybe-if
                      (lambda (x)
                        (syntax-case x ()
                          ((_ (formals) (require) . body)
                           (not (eqv? '() (syntax->datum #'require)))
                           #'(if require
                                 (begin body)
                                 (former formals)))
                          ((_ (formals) (require) . body)
                           (eqv? '() (syntax->datum #'require))
                           #'(begin body))))))
         #'(let* ((name-<%tm-def> (lambda (formals)
                                    (maybe-if (formals) (require) body)))
                  (new-tm-def (make-instance <%tm-def>
                                             #:tm-def-formals (formals)
                                             #:tm-def-require (require)
                                             #:tm-def-procedure name-<%tm-def>)))
             ;; Todo: process @arg{rest} and modify this-tm-defined ?
             new-tm-def))))))


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


(define-syntax tm-define
  (lambda (x)

    (define (keyword-like? stx)
      (let ((dat (syntax->datum stx)))
        (and (symbol? dat)
             (eqv? (string-ref (symbol->string dat) 0) #\:))))

    (define (->keyword sym)
      (symbol->keyword (string->symbol (substring (symbol->string sym) 1))))

    (define (parse args
                   formals require body
                   type synopsis returns note argument default
                   proposals secure check-mark interactive balloon)
      ;;
      (syntax-case args ()
        (()
         (let ((formals     #`(#:tm-def-formals '(#,formals)))
               (require     (if (or (null? require)
                                    (< 2 (length (syntax->datum #'require))))
                                #`(#:tm-def-require '(#,require))
                                #`(#:tm-def-require '(#,(and require)))))
               (body        #`(#:tm-def-body '(#,body)))
               ;;
               (type        (if (null? type)        '() #`(#:tm-def-type        '(#,type))))
               (synopsis    (if (null? synopsis)    '() #`(#:tm-def-synopsis    '(#,synopsis))))
               (returns     (if (null? returns)     '() #`(#:tm-def-returns     '(#,returns))))
               (note        (if (null? note)        '() #`(#:tm-def-note        '(#,note))))
               (argument    (if (null? argument)    '() #`(#:tm-def-argument    '(#,argument))))
               (default     (if (null? default)     '() #`(#:tm-def-default     '(#,default))))
               ;;
               (proposals   (if (null? proposals)   '() #`(#:tm-def-proposals   '(#,proposals))))
               (secure      (if (null? secure)      '() #`(#:tm-def-secure      '(#,secure))))
               (check-mark  (if (null? check-mark)  '() #`(#:tm-def-check-mark  '(#,check-mark))))
               (interactive (if (null? interactive) '() #`(#:tm-def-interactive '(#,interactive))))
               (balloon     (if (null? balloon)     '() #`(#:tm-def-balloon     '(#,balloon)))))
           #`(#,@formals #,@require #,@body
              #,@type #,@synopsis #,@returns #,@note #,@argument #,@default
              #,@proposals #,@secure #,@check-mark #,@interactive #,@balloon)))
        ;; The user wanted #:foo but wrote :foo. Fix it.
        (((sym . forms) . args)
         (keyword-like? #'sym) ; keyword-like? => #t implies keyword? => #f
         (parse #`((#,(->keyword (syntax->datum #'sym)) . forms) . args)
                formals require body
                type synopsis returns note argument default
                proposals secure check-mark interactive balloon))
        (((kw . forms) . args)
         (not (keyword? (syntax->datum #'kw))) ; end of (#:kw args)
         (parse #'() formals require #'((kw . forms) . args) ; body
                type synopsis returns note argument default
                proposals secure check-mark interactive balloon))
        (((#:mode . forms) . args)
         (not (identifier? #'forms))
         (syntax-violation 'tm-define "(#:mode predicate-fn?) expected" args))
        (((#:mode predicate-fn) . args)
         (parse #'args formals #`(#,@require (predicate-fn)) body
                type synopsis returns note argument default
                proposals secure check-mark interactive balloon))
        (((#:require . forms) . args)
         (parse #'args formals #`(#,@require forms) body
                type synopsis returns note argument default
                proposals secure check-mark interactive balloon))
        (((#:type . forms) . args)
         (parse #'args formals require body
                #`(#,@type forms) synopsis returns note argument default
                proposals secure check-mark interactive balloon))
        (((#:synopsis . forms) . args)
         (parse #'args formals require body
                type #`(#,@synopsis forms) returns note argument default
                proposals secure check-mark interactive balloon))
        (((#:returns . forms) . args)
         (parse #'args formals require body
                type synopsis #`(#,@returns forms) note argument default
                proposals secure check-mark interactive balloon))
        (((#:note . forms) . args)
         (parse #'args formals require body
                type synopsis forms #`(#,@note forms) argument default
                proposals secure check-mark interactive balloon))
        (((#:argument . forms) . args)
         (parse #'args formals require body
                type synopsis forms forms #`(#,@argument forms) default
                proposals secure check-mark interactive balloon))
        (((#:default . forms) . args)
         (parse #'args formals require body
                type synopsis forms forms argument #`(#,@default forms)
                proposals secure check-mark interactive balloon))
        (((#:proposals . forms) . args)
         (parse #'args formals require body
                type synopsis forms forms argument default
                #`(#,@proposals forms) secure check-mark interactive balloon))
        (((#:secure . forms) . args)
         (parse #'args formals require body
                type synopsis forms forms argument default
                proposals #'(#t) check-mark interactive balloon))
        (((#:check-mark . forms) . args)
         (parse #'args formals require body
                type synopsis forms forms argument default
                proposals secure #`(#,@check-mark (forms)) interactive balloon))
        (((#:interactive . forms) . args)
         (parse #'args formals require body
                type synopsis forms forms argument default
                proposals secure check-mark #`(#,@interactive forms) balloon))
        (((#:balloon . forms) . args)
         (parse #'args formals require body
                type synopsis forms forms argument default
                proposals secure check-mark interactive #`(#,@balloon forms)))))

    (syntax-case x ()
      ((_ (name arg0 arg1 ...) . body)
       (and (identifier? #'name)
            (not (module-defined? the-tm-defs-module (syntax->datum #'name))))
       (begin
         ;; (display "new tm-define, name:|" )
         ;; (display (syntax->datum #'name))
         ;; (display "|\n")
         (with-syntax ((name-<tm-defined> (datum->syntax
                                           x
                                           (symbol-append
                                            (syntax->datum #'name)
                                            '-<tm-defined>)))
                       (name-<tm-defined>-proc (datum->syntax
                                                x
                                                (symbol-append
                                                 (syntax->datum #'name)
                                                 '-<tm-defined>-proc)))
                       (name #'name)
                       ;; (the-tm-defs-module
                       ;;  #'(resolve-module
                       ;;     '(kernel texmacs tm-define definitions)))
                       ((parsed-body ...)
                        (parse #'(body ...) ; args
                               #'(arg0 arg1 ...) ; formals
                               '() ; require
                               '() ; body
                               '() ; type
                               '() ; synopsis
                               '() ; returns
                               '() ; note
                               '() ; argument
                               '() ; default
                               '() ; proposals
                               '() ; secure
                               '() ; check-mark
                               '() ; interactive
                               '() ; balloon
                               )))
           #'(letrec ((the-tm-defs-module the-tm-defs-module)
                      (tm-defined
                       (inline-make-new-<tm-defined>-with-base-<%tm-def>
                        name parsed-body ...)))
               (module-define! the-tm-defs-module
                               'name-<tm-defined>
                               tm-defined)
               (module-define! the-tm-defs-module
                               'name
                               (make-syntax-transformer
                                'name 'macro
                                (tm-defs-identifier-syntax
                                 (@@ (kernel texmacs tm-define definitions)
                                     name-<tm-defined>))))
               (module-export! the-tm-defs-module '(name))
               tm-defined))))
      ((_ (name arg0 arg1 ...) . body)
       (and (identifier? #'name)
            (module-defined? the-tm-defs-module (syntax->datum #'name)))
       (begin
         ;; (display "overloading, name:|" )
         ;; (display (syntax->datum #'name))
         ;; (display "|\n")
         (with-syntax ((name-<tm-defined> (datum->syntax
                                           x
                                           (symbol-append
                                            (syntax->datum #'name)
                                            '-<tm-defined>)))
                       (name #'name)
                       ;; (the-tm-defs-module
                       ;;  #'(resolve-module
                       ;;     '(kernel texmacs tm-define definitions)))
                       ((parsed-body ...)
                        (parse #'(body ...) ; args
                               #'(arg0 arg1 ...) ; formals
                               '() ; require
                               '() ; body
                               '() ; type
                               '() ; synopsis
                               '() ; returns
                               '() ; note
                               '() ; argument
                               '() ; default
                               '() ; proposals
                               '() ; secure
                               '() ; check-mark
                               '() ; interactive
                               '() ; balloon
                               )))
           (with-syntax ((tm-defined
                          #'(@@ (kernel texmacs tm-define definitions)
                                name-<tm-defined>)))
             #'(letrec* ((this-tm-defined tm-defined)
                         (|% former| (car (slot-ref this-tm-defined 'tm-defs))))
                 (syntax-parameterize ((former (syntax-rules ()
                                                 ((former vals (... ...))
                                                  (|% former| vals (... ...))))))
                   (slot-set! this-tm-defined 'tm-defs
                              (cons (inline-make-new-<%tm-def> name parsed-body ...)
                                    (slot-ref this-tm-defined 'tm-defs)))
                   this-tm-defined)))))))))

(export-syntax tm-define)
