;;;
;;;
;;; Development exploration for new tm-define implementation for Guile 2.2
;;; based TeXmacs.
;;;
;;;

(define-module (kernel texmacs tm-define)
  #:use-module (kernel texmacs tm-define definitions)
  #:use-module (oop goops simple))


(define-syntax the-tm-defs-module
  (identifier-syntax
   (resolve-module '(kernel texmacs tm-define definitions))))


(define-public (in-text?) #t)


(define (display* . l)
  (for-each display l))

(define (write* . l)
  (for-each write l))


;;;;;;;
;;;
;;; Saving mode and require here for introspection, I guess; wasteful?
;;; They will already be incorporated into the procedure...
;;;
(define-class <%tm-def> (<object> <applicable>)
  (procedure #:init-keyword #:<%tm-def>-procedure)
  ;;
  (formals #:init-keyword #:<%tm-def>-formals #:init-value '())
  (require #:init-keyword #:<%tm-def>-require #:init-value '())
  ;;
  (<%tm-def>-module #:init-keyword #:<%tm-def>-module)
  ;;
  #:metaclass <applicable-struct-class>)

(export <%tm-def>)


(define-class <tm-defined> (<object> <applicable>)
  (procedure   #:init-keyword #:<tm-defined>-procedure)
  ;;
  (<%tm-def>s  #:init-keyword #:<tm-defined>-<%tm-def>s     #:init-value '())
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
  (<tm-defined>-module #:init-keyword #:<tm-defined>-module)
  ;;
  #:metaclass <applicable-struct-class>)

(export <tm-defined>)


(define-syntax-parameter former
  (lambda (x)
    (syntax-violation 'former "former used outside of an overloading tm-define" x)))

(export-syntax former)

(define-syntax build-<%tm-def>-procedure
  (lambda (syntax-x)
    (display "build-<%tm-def>-procedure:")
    (write* (syntax->datum syntax-x))
    (newline)
    (newline)
    (with-ellipsis :::
      (syntax-case syntax-x ()
        ((_ (ign) (fmls :::) (#:no-require) (bod :::))
         (begin
           (display "                #:no-require:")
           (newline)
           (display "                fmls:")
           (write* (syntax->datum #'(fmls :::)))
           (newline)
           (display "                bod:")
           (write* (syntax->datum #'(begin bod :::)))
           (newline)
           #t)
         #'(lambda (fmls :::)
             (begin bod :::))
         )
        ((_ (#:has-former) (fmls :::) (req) (bod :::))
         (begin
           (display "                #:has-former:")
           (newline)
           (display "                fmls:")
           (write* (syntax->datum #'(fmls :::)))
           (newline)
           (display "                req:")
           (write* (syntax->datum #'req))
           (newline)
           (display "                bod:")
           (write* (syntax->datum #'(begin bod :::)))
           (newline)
           #t)
         #'(lambda (fmls :::)
             (if req
                 (begin bod :::)
                 (former fmls :::)))
         )
        ((_ (#:no-former) (fmls :::) (req) (bod :::))
         (begin
           (display "                #:no-former:")
           (newline)
           (display "                fmls:")
           (write* (syntax->datum #'(fmls :::)))
           (newline)
           (display "                req:")
           (write* (syntax->datum #'req))
           (newline)
           (display "                bod:")
           (write* (syntax->datum #'(begin bod :::)))
           (newline)
           #t)
         ;; base <%tm-def> has no former
         #`(lambda (fmls :::)
             (if req
                 (begin bod :::))))))))


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



(define-syntax _%_make-new-<tm-defined>-with-base-<%tm-def>
  (lambda (x)
    (display "_%_make-new-<tm-defined>-with-base-<%tm-def>:")
    (write* x)
    (newline)
    (with-ellipsis !!!
      (syntax-case x ()
        ((_ name #:<%tm-def>-formals (formals !!!) #:<%tm-def>-require (require) #:<%tm-def>-body (body !!!) rest !!!)
         ;; former is not defined for the first <%tm-def> item.
         (with-syntax ((name-<tm-defined>           (syntax-symbol-append
                                                     x #f #'name '-<tm-defined>))
                       (name-<tm-defined>-procedure (syntax-symbol-append
                                                     x #f #'name '-<tm-defined>-procedure))
                       (name-<%tm-def>              (syntax-symbol-append
                                                     x #f #'name '-<%tm-def>))
                       (name-<%tm-def>-procedure    (syntax-symbol-append
                                                     x #f #'name '-<%tm-def>-procedure))
                       )
           ;; call lambda-maybe-if at macro expansion time.
           #'(let* ((name-<%tm-def>-procedure (build-<%tm-def>-procedure (#:no-former)
                                                                         (formals !!!)
                                                                         (require)
                                                                         (body !!!)))
                    (dummy (display* "  after build-<%tm-def>-procedure\n"))
                    (name-<%tm-def> (make-instance <%tm-def>
                                                   #:<%tm-def>-formals   '(formals !!!)
                                                   #:<%tm-def>-require   'require
                                                   #:<%tm-def>-procedure name-<%tm-def>-procedure
                                                   rest !!!))
                    (dummy (display* "  after make-instance <%tm-def>\n"))
                    (name-<tm-defined> (make-instance <tm-defined>
                                                      #:<tm-defined>-<%tm-def>s (list name-<%tm-def>)
                                                      rest !!!))
                    (dummy (display* "  after make-instance <tm-defined>\n"))
                    (name-<tm-defined>-procedure
                     (let ((this-<tm-defined> name-<tm-defined>))
                       ;; closure over this-<tm-defined>
                       (lambda (formals !!!)
                         ;; car of that list at run-time determines top
                         ;; tm-def, probably latest defined one...
                         ((car (slot-ref this-<tm-defined> '<%tm-def>s))
                          formals !!!))))
                    (dummy (display* "  after define name-<tm-defined>-procedure\n\n"))
                    )
               (slot-set! name-<tm-defined> 'procedure name-<tm-defined>-procedure)
               name-<tm-defined>)))))))


(define-syntax _%_make-new-<%tm-def>
  (lambda (x)
    (display "_%_make-new-<%tm-def>:")
    (write x)
    (newline)
    (with-ellipsis !!!
      (syntax-case x ()
        ((_ name #:<%tm-def>-formals (formals !!!) #:<%tm-def>-require (require) #:<%tm-def>-body (body !!!) rest !!!)
         ;; former is defined for the overloading <%tm-def> items.
         (with-syntax ((name-<%tm-def>              (syntax-symbol-append
                                                     x #f #'name '-<%tm-def>))
                       (name-<%tm-def>-procedure    (syntax-symbol-append
                                                     x #f #'name '-<%tm-def>-procedure))
                       )
           ;; call lambda-maybe-if once during macro expansion
           #'(let* ((name-<%tm-def>-procedure (build-<%tm-def>-procedure (#:has-former)
                                                               (formals !!!)
                                                               (require)
                                                               (body    !!!)))
                    (name-<%tm-def> (make-instance <%tm-def>
                                                   #:<%tm-def>-formals   '(formals !!!)
                                                   #:<%tm-def>-require   'require
                                                   #:<%tm-def>-procedure name-<%tm-def>-procedure
                                                   rest !!!)))
               ;; Todo: process @arg{rest} and modify this-tm-defined ?
               name-<%tm-def>)))))))


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
      (display "      parse-body is:")
      (write parse-body)
      (newline)
      (display "                    :")
      (write (syntax->datum parse-body))
      (newline)
      (display "      formals is:")
      (write formals)
      (newline)
      (display "                 :")
      (write (syntax->datum formals))
      (newline)
      (display "      require is:")
      (write require)
      (newline)
      (display "                 :")
      (write (syntax->datum require))
      (newline)
      (display "      body is:")
      (write body)
      (newline)
      (display "              :")
      (write (syntax->datum body))
      (newline)
      ;;
      ;;(with-ellipsis :::
      (syntax-case parse-body ()
        ((#:parse-finished)
         (let ((formals     #`(#:<%tm-def>-formals (#,@formals)))
               (require     (let ((require-datum (syntax->datum require)))
                             (cond
                              ((null? require-datum)
                               #'(#:<%tm-def>-require (#:no-require)))
                              ((> 1 (length require-datum))
                               #`(#:<%tm-def>-require (and #,@require)))
                              (else
                                #`(#:<%tm-def>-require (#,@require))))))
               (body        #`(#:<%tm-def>-body (#,@body)))
               ;;
               (type        (if (null? (syntax->datum type))        #'() #`(#:<tm-defined>-type        (#,@type))))
               (synopsis    (if (null? (syntax->datum synopsis))    #'() #`(#:<tm-defined>-synopsis    (#,@synopsis))))
               (returns     (if (null? (syntax->datum returns))     #'() #`(#:<tm-defined>-returns     (#,@returns))))
               (note        (if (null? (syntax->datum note))        #'() #`(#:<tm-defined>-note        (#,@note))))
               (argument    (if (null? (syntax->datum argument))    #'() #`(#:<tm-defined>-argument    (#,@argument))))
               (default     (if (null? (syntax->datum default))     #'() #`(#:<tm-defined>-default     (#,@default))))
               ;;
               (proposals   (if (null? (syntax->datum proposals))   #'() #`(#:<tm-defined>-proposals   (#,@proposals))))
               (secure      (if (null? (syntax->datum secure))      #'() #`(#:<tm-defined>-secure      (#,@secure))))
               (check-mark  (if (null? (syntax->datum check-mark))  #'() #`(#:<tm-defined>-check-mark  (#,@check-mark))))
               (interactive (if (null? (syntax->datum interactive)) #'() #`(#:<tm-defined>-interactive (#,@interactive))))
               (balloon     (if (null? (syntax->datum balloon))     #'() #`(#:<tm-defined>-balloon     (#,@balloon))))
               )
           (begin
             (display "#:parse-finished:")
             (newline)
             (newline)
             #`(#,@formals #,@require #,@body
                #,@type #,@synopsis #,@returns #,@note #,@argument #,@default
                #,@proposals #,@secure #,@check-mark #,@interactive #,@balloon)))
           )
        ((((sym forms ...) rest0 rest1 ...))
         (begin
           (display "      (unwrap syntax)")
           (newline)
           (parse #'((sym forms ...) rest0 rest1 ...)
                  formals require body
                  type synopsis returns note argument default
                  proposals secure check-mark interactive balloon))
         )
        ;; The user wanted #:foo but wrote :foo. Fix it.
        (((sym forms ...) rest0 rest1 ...)
         (keyword-like? #'sym)            ; keyword-like? => #t implies symbol? => #f and
                                          ; keyword? => #f
         (begin
           (display "      keyword-like?  ")
           (write #'sym)
           (newline)
           (parse #`((#,(->keyword (syntax->datum #'sym)) forms ...) rest0 rest1 ...)
                  formals require body
                  type synopsis returns note argument default
                  proposals secure check-mark interactive balloon))
         )
        (((sym) rest0 rest1 ...)
         (keyword-like? #'sym)            ; keyword-like? => #t implies symbol? => #f and
                                          ; keyword? => #f
         (begin
           (display "      keyword-like?  ")
           (write #'sym)
           (newline)
           (parse #`((#,(->keyword (syntax->datum #'sym))) rest0 rest1 ...)
                  formals require body
                  type synopsis returns note argument default
                  proposals secure check-mark interactive balloon))
         )
        (((#:mode predicate-fn) rest0 rest1 ...)
         (not (identifier? #'predicate-fn))
         #'(syntax-violation 'tm-define "(#:mode predicate-fn) expected" parse-body)
         )
        (((#:mode predicate-fn) rest0 rest1 ...)
         (begin
           (display "      #:mode ")
           (write #'predicate-fn)
           (newline)
           (parse #'(rest0 rest1 ...) formals #`(#,@require (predicate-fn)) body
                  type synopsis returns note argument default
                  proposals secure check-mark interactive balloon))
         )
        (((#:require forms ...) rest0 rest1 ...)
         (begin
           (display "      #:require ")
           (write #'(forms ...))
           (newline)
           (parse #'(rest0 rest1 ...) formals #`(#,@require forms ...) body
                  type synopsis returns note argument default
                  proposals secure check-mark interactive balloon))
         )
        (((#:type forms ...) rest0 rest1 ...)
         (parse #'(rest0 rest1 ...) formals require body
                #`(#,@type forms ...) synopsis returns note argument default
                proposals secure check-mark interactive balloon)
         )
        (((#:synopsis forms ...) rest0 rest1 ...)
         (parse #'(rest0 rest1 ...) formals require body
                type #`(#,@synopsis forms ...) returns note argument default
                proposals secure check-mark interactive balloon)
         )
        (((#:returns forms ...) rest0 rest1 ...)
         (parse #'(rest0 rest1 ...) formals require body
                type synopsis #`(#,@returns forms ...) note argument default
                proposals secure check-mark interactive balloon)
         )
        (((#:note forms ...) rest0 rest1 ...)
         (parse #'(rest0 rest1 ...) formals require body
                type synopsis returns #`(#,@note forms ...) argument default
                proposals secure check-mark interactive balloon)
         )
        (((#:argument forms ...) rest0 rest1 ...)
         (parse #'(rest0 rest1 ...) formals require body
                type synopsis returns note #`(#,@argument forms ...) default
                proposals secure check-mark interactive balloon)
         )
        (((#:default forms ...) rest0 rest1 ...)
         (parse #'(rest0 rest1 ...) formals require body
                type synopsis returns note argument #`(#,@default forms ...)
                proposals secure check-mark interactive balloon)
         )
        (((#:proposals forms ...) rest0 rest1 ...)
         (parse #'(rest0 rest1 ...) formals require body
                type synopsis returns note argument default
                #`(#,@proposals forms ...) secure check-mark interactive balloon)
         )
        (((#:secure) rest0 rest1 ...)
         (begin
           (display "      #:secure")
           (newline)
           (parse #'(rest0 rest1 ...) formals require body
                  type synopsis returns note argument default
                  proposals #'(#t) check-mark interactive balloon))
         )
        (((#:secure forms ...) rest0 rest1 ...)
         (begin
           (display "      #:secure forms:")
           (write #'(forms ...))
           (newline)
           (parse #'(rest0 rest1 ...) formals require body
                  type synopsis returns note argument default
                  proposals #'(#t) check-mark interactive balloon))
         )
        (((#:check-mark forms ...) rest0 rest1 ...)
         (parse #'(rest0 rest1 ...) formals require body
                type synopsis returns note argument default
                proposals secure #`(#,@check-mark (forms ...)) interactive balloon)
         )
        (((#:interactive forms ...) rest0 rest1 ...)
         (parse #'(rest0 rest1 ...) formals require body
                type synopsis returns note argument default
                proposals secure check-mark #`(#,@interactive forms ...) balloon)
         )
        (((#:balloon forms ...) rest0 rest1 ...)
         (parse #'(rest0 rest1 ...) formals require body
                type synopsis returns note argument default
                proposals secure check-mark interactive #`(#,@balloon forms ...))
         )
        (((kw forms ...) rest0 rest1 ...)
         (keyword? (syntax->datum #'kw))
         #'(syntax-violation 'tm-define "Unknown keyword " kw))
        ((forms ...)
         (begin
           (display "parse finished! Forms found: ")
           (write #'(forms ...))
           (newline)
           (parse #'(#:parse-finished) formals require #'(forms ...) ; body
                  type synopsis returns note argument default
                  proposals secure check-mark interactive balloon))))
      )


    (syntax-case x ()
      ((_ (name arg ...) body ...)
       (and (identifier? #'name)
            (not (module-defined? the-tm-defs-module (syntax->datum #'name))))
       (begin
         (display "new tm-define:")
         (display (syntax->datum #'name))
         (display "\n")
         (with-syntax ((name-<tm-defined>           (syntax-symbol-append
                                                     x #f #'name '-<tm-defined>))
                       (name-<tm-defined>-procedure (syntax-symbol-append
                                                     x #f #'name '-<tm-defined>-procedure))
                       (name-<%tm-def>              (syntax-symbol-append
                                                     x #f #'name '-<%tm-def>))
                       (name-<%tm-def>-procedure    (syntax-symbol-append
                                                     x #f #'name '-<%tm-def>-procedure))
                       (name #'name))
           #`(letrec* ((name-<tm-defined>
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
               (display (symbol->string 'name-<tm-defined>))
               (display ":")
               (write name-<tm-defined>)
               (newline)
               (newline)
               (module-define! the-tm-defs-module
                               'name-<tm-defined>
                               name-<tm-defined>)
               (module-define! the-tm-defs-module
                               'name
                               (make-syntax-transformer
                                'name 'macro
                                (tm-defs-identifier-syntax
                                 (@@ (kernel texmacs tm-define definitions)
                                     name-<tm-defined>))))
               (module-export! the-tm-defs-module '(name))
               name-<tm-defined>)))
       )
      ((_ (name arg ...) body ...)
       (and (identifier? #'name)
            (module-defined? the-tm-defs-module (syntax->datum #'name)))
       (begin
         (display "overloading: " )
         (display (syntax->datum #'name))
         (display "\n")
         (with-syntax ((name-<tm-defined>           (syntax-symbol-append
                                                     x #f #'name '-<tm-defined>))
                       (name-<%tm-def>              (syntax-symbol-append
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
