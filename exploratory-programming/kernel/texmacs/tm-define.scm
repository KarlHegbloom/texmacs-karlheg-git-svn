;;;
;;;
;;; Development exploration for new tm-define implementation for Guile 2.2
;;; based TeXmacs.
;;;
;;;

(define-module (kernel texmacs tm-define)
  #:use-module (kernel texmacs tm-define definitions)
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
      ((_ name (#:tm-def-formals (formals ...) #:tm-def-require (require) #:tm-def-body (body)))
       ;; former is not defined for the first <%tm-def> item.
       (with-syntax ((name-<%tm-def> (datum->syntax
                                      x
                                      (symbol-append
                                       (syntax->datum #'name)
                                       '-<%tm-def>)))
                     (maybe-if
                      (lambda (x)
                        (syntax-case x ()
                          ((_ (formals ...) (require) body)
                           (not (eqv? '(()) (syntax->datum #'require)))
                           #'(if require body))
                          ((_ (formals ...) (require) body)
                           (eqv? '(()) (syntax->datum #'require))
                           #'body)))))
         #'(let* ((name-<%tm-def> (lambda (formals ...)
                                      (maybe-if (formals ...) (require) body)))
                  (tm-def (make-instance <%tm-def>
                                         #:tm-def-formals (formals ...)
                                         #:tm-def-require (require)
                                         #:tm-def-procedure tm-def-procedure))
                  (new-tm-defined (make-instance <tm-defined>
                                                 #:tm-def-tm-defs (list tm-def)
                                                 rest))
                  (procedure (let ((this-tm-defined new-tm-defined))
                               (lambda (formals ...)
                                 ((car (slot-ref this-tm-defined 'tm-defs))
                                  formals ...)))))
             (slot-set! new-tm-defined 'procedure procedure)
             (display "inline-make-new-<tm-defined>-with-base-<%tm-def>: formals:")
             (write #'(formals ...))
             (newline)
             (display "                                                  require:")
             (write require)
             (newline)
             (display "                                                  ")
             (display (symbol-string 'name-<%tm-def>))
             (display ":")
             (write name-<%tm-def>)
             (newline)
             new-tm-defined))))))


(define-syntax inline-make-new-<%tm-def>
  (lambda (x)
    (syntax-case x ()
      ((_ (#:tm-def-formals (formals ...) #:tm-def-require (require) #:tm-def-body (body)))
       ;; former is defined for the overloading <%tm-def> items.
       (with-syntax ((name-<%tm-def> (datum->syntax
                                      x
                                      (symbol-append
                                       (syntax->datum #'name)
                                       '-<%tm-def>)))
                     (maybe-if
                      (lambda (x)
                        (syntax-case x ()
                          ((_ (formals ...) (require) body)
                           (not (eqv? '(()) (syntax->datum #'require)))
                           #'(if require
                                 body
                                 (former formals ...)))
                          ((_ (formals ...) (require) body)
                           (eqv? '(()) (syntax->datum #'require))
                           #'body)))))
         #'(let* ((name-<%tm-def> (lambda (formals ...)
                                    (maybe-if (formals ...) (require) body)))
                  (new-tm-def (make-instance <%tm-def>
                                             #:tm-def-formals (formals ...)
                                             #:tm-def-require (require)
                                             #:tm-def-procedure name-<%tm-def>)))
             ;; Todo: process @arg{rest} and modify this-tm-defined ?
             (display "inline-make-new-<%tm-def>: formals:")
             (write (slot-ref new-tm-def 'formals))
             (newline)
             (display "                           require:")
             (write (slot-ref newline 'require))
             (newline)
             (display "                           ")
             (display (symbol->string 'name-<%tm-def>))
             (display ":")
             (write name-<%tm-def>)
             (newline)
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

    (define (parse parse-body
                   formals require body
                   type synopsis returns note argument default
                   proposals secure check-mark interactive balloon)
      ;;
      (newline)
      (display "parse:")
      (newline)
      (display "       parse-body is:")
      (write parse-body)
      (newline)
      (display "       formals is:")
      (write formals)
      (newline)
      (display "       require is:")
      (write require)
      (newline)
      (display "       body is:")
      (write body)
      (newline)
      ;;
      (syntax-case parse-body ()
        ((#:PARSE-FINISHED)
         (let ((formals     #`(#:tm-def-formals (#,@formals)))
               (require     (if (null? (syntax->datum require))
                                #'(#:tm-def-require (()))
                                #`(#:tm-def-require ((and #,@require)))))
               (body        #`(#:tm-def-body ((begin #,@body))))
               ;;
               (type        (if (null? (syntax->datum type))        #'() #`(#:tm-def-type        (#,@type))))
               (synopsis    (if (null? (syntax->datum synopsis))    #'() #`(#:tm-def-synopsis    (#,@synopsis))))
               (returns     (if (null? (syntax->datum returns))     #'() #`(#:tm-def-returns     (#,@returns))))
               (note        (if (null? (syntax->datum note))        #'() #`(#:tm-def-note        (#,@note))))
               (argument    (if (null? (syntax->datum argument))    #'() #`(#:tm-def-argument    (#,@argument))))
               (default     (if (null? (syntax->datum default))     #'() #`(#:tm-def-default     (#,@default))))
               ;;
               (proposals   (if (null? (syntax->datum proposals))   #'() #`(#:tm-def-proposals   (#,@proposals))))
               (secure      (if (null? (syntax->datum secure))      #'() #`(#:tm-def-secure      (#,@secure))))
               (check-mark  (if (null? (syntax->datum check-mark))  #'() #`(#:tm-def-check-mark  (#,@check-mark))))
               (interactive (if (null? (syntax->datum interactive)) #'() #`(#:tm-def-interactive (#,@interactive))))
               (balloon     (if (null? (syntax->datum balloon))     #'() #`(#:tm-def-balloon     (#,@balloon)))))
           #`(#,@formals #,@require #,@body
              #,@type #,@synopsis #,@returns #,@note #,@argument #,@default
              #,@proposals #,@secure #,@check-mark #,@interactive #,@balloon)))
        ((((sym forms ...) rest-parse-body ...))
         (parse #'((sym forms ...) rest-parse-body ...)
                formals require body
                type synopsis returns note argument default
                proposals secure check-mark interactive balloon))
        ;; The user wanted #:foo but wrote :foo. Fix it.
        (((sym forms ...) rest-parse-body ...)
         (keyword-like? #'sym) ; keyword-like? => #t implies symbol? => #f and
                               ; keyword? => #f
         (begin
           (display "           keyword-like?  ")
           (write #'sym)
           (newline)
           (parse #`((#,(->keyword (syntax->datum #'sym)) forms ...) rest-parse-body ...)
                  formals require body
                  type synopsis returns note argument default
                  proposals secure check-mark interactive balloon)))
        (((sym) rest-parse-body ...)
         (keyword-like? #'sym) ; keyword-like? => #t implies symbol? => #f and
                               ; keyword? => #f
         (begin
           (display "           keyword-like?  ")
           (write #'sym)
           (newline)
           (parse #`((#,(->keyword (syntax->datum #'sym))) rest-parse-body ...)
                  formals require body
                  type synopsis returns note argument default
                  proposals secure check-mark interactive balloon)))
        (((#:mode predicate-fn) rest-parse-body ...)
         (not (identifier? #'predicate-fn))
         #'(syntax-violation 'tm-define "(#:mode predicate-fn?) expected" parse-body))
        ((((#:mode predicate-fn) rest-parse-body ...))
         (begin
           (display "           #:mode ")
           (write #'predicate-fn)
           (newline)
           (parse #'(rest-parse-body ...) formals #`(#,@require (predicate-fn)) body
                  type synopsis returns note argument default
                  proposals secure check-mark interactive balloon)))
        (((#:require forms ...) rest-parse-body ...)
         (begin
           (display "           #:require ")
           (write #'(forms ...))
           (newline)
           (parse #'(rest-parse-body ...) formals #`(#,@require forms ...) body
                  type synopsis returns note argument default
                  proposals secure check-mark interactive balloon)))
        (((#:type forms ...) rest-parse-body ...)
         (parse #'(rest-parse-body ...) formals require body
                #`(#,@type forms ...) synopsis returns note argument default
                proposals secure check-mark interactive balloon))
        (((#:synopsis forms ...) rest-parse-body ...)
         (parse #'(rest-parse-body ...) formals require body
                type #`(#,@synopsis forms ...) returns note argument default
                proposals secure check-mark interactive balloon))
        (((#:returns forms ...) rest-parse-body ...)
         (parse #'(rest-parse-body ...) formals require body
                type synopsis #`(#,@returns forms ...) note argument default
                proposals secure check-mark interactive balloon))
        (((#:note forms ...) rest-parse-body ...)
         (parse #'(rest-parse-body ...) formals require body
                type synopsis returns #`(#,@note forms ...) argument default
                proposals secure check-mark interactive balloon))
        (((#:argument forms ...) rest-parse-body ...)
         (parse #'(rest-parse-body ...) formals require body
                type synopsis returns note #`(#,@argument forms ...) default
                proposals secure check-mark interactive balloon))
        (((#:default forms ...) rest-parse-body ...)
         (parse #'(rest-parse-body ...) formals require body
                type synopsis returns note argument #`(#,@default forms ...)
                proposals secure check-mark interactive balloon))
        (((#:proposals forms ...) rest-parse-body ...)
         (parse #'(rest-parse-body ...) formals require body
                type synopsis returns note argument default
                #`(#,@proposals forms ...) secure check-mark interactive balloon))
        (((#:secure) rest-parse-body ...)
         (begin
           (display "           #:secure ")
           (newline)
           (parse #'(rest-parse-body ...) formals require body
                  type synopsis returns note argument default
                  proposals #'(#t) check-mark interactive balloon)))
        (((#:secure forms ...) rest-parse-body ...)
         (begin
           (display "           #:secure forms ")
           (write #'(forms ...))
           (newline)
           (parse #'(rest-parse-body ...) formals require body
                  type synopsis returns note argument default
                  proposals #'(#t) check-mark interactive balloon)))
        (((#:check-mark forms ...) rest-parse-body ...)
         (parse #'(rest-parse-body ...) formals require body
                type synopsis returns note argument default
                proposals secure #`(#,@check-mark (forms ...)) interactive balloon))
        (((#:interactive forms ...) rest-parse-body ...)
         (parse #'(rest-parse-body ...) formals require body
                type synopsis returns note argument default
                proposals secure check-mark #`(#,@interactive forms ...) balloon))
        (((#:balloon forms ...) rest-parse-body ...)
         (parse #'(rest-parse-body ...) formals require body
                type synopsis returns note argument default
                proposals secure check-mark interactive #`(#,@balloon forms ...)))
        (((kw forms ...) rest-parse-body ...)
         (not (keyword? (syntax->datum #'kw))) ; end of (#:kw rest-parse-body)
         (begin
           (display "   parse finished!  ")
           (write #'kw)
           (newline)
           (parse #'(#:PARSE-FINISHED) formals require #'((kw forms ...) rest-parse-body ...) ; body
                  type synopsis returns note argument default
                  proposals secure check-mark interactive balloon)))))

    (syntax-case x ()
      ((_ (name arg ...) . body)
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
                       (parsed-body
                        (parse #'(body) ; args
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
                               )))
           #'(letrec ((the-tm-defs-module the-tm-defs-module)
                      (tm-defined
                       (inline-make-new-<tm-defined>-with-base-<%tm-def>
                        name parsed-body)))
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
      ((_ (name arg ...) . body)
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
                       (parsed-body
                        (parse #'(body) ; args
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
                               )))
           (with-syntax ((tm-defined
                          #'(@@ (kernel texmacs tm-define definitions)
                                name-<tm-defined>)))
             #'(letrec* ((this-tm-defined tm-defined)
                         (|% former| (car (slot-ref this-tm-defined 'tm-defs))))
                 (syntax-parameterize ((former (syntax-rules ()
                                                 ((former vals (... ...))
                                                  (|% former| vals (... ...))))))
                   (display "           name is:")
                   (write name)
                   (newline)
                   (let ((new-<%tm-def> (inline-make-new-<%tm-def> name parsed-body)))
                     (slot-set! this-tm-defined 'tm-defs
                                (cons new-<%tm-def> (slot-ref this-tm-defined 'tm-defs)))
                     this-tm-defined))))))))))

(export-syntax tm-define)
