;;; -*- scheme -*-
;;;
(assert-load-verbosity #t)
;; (read-enable  'r7rs-symbols)
;; (print-enable 'r7rs-symbols)
(read-enable 'square-brackets)
(read-enable 'keywords #f)
(read-enable 'positions)


(eval-when (compile eval load)

  (set-current-module (resolve-module '(guile)))

  (default-duplicate-binding-handler
    '(merge-generics merge-accessors replace warn-override-core warn last))

  (use-modules (oop goops))

  (add-hook! module-defined-hook
             (lambda (module)
               (set-module-duplicates-handlers!
                module
                (lookup-duplicates-handlers
                 '(merge-generics merge-accessors
                   replace warn-override-core warn last)))))

  (add-to-load-path
   "/home/karlheg/src/TeXmacs/texmacs-git-svn-guile-2.2/exploratory-programming")

  ;;
  ;; Simple module interface syntax; could be extended to accept the syntax
  ;; from use-modules for resolving an interface specification...
  ;;

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


  (define-syntax use-and-re-export-modules-prepended
    (syntax-rules ()
      ((_ (mod ...) ...)
       (eval-when (expand load eval compile)
         (let* ((cm (current-module))
                (cm-public-interface
                 (module-public-interface cm)))
           (begin
             (begin
               (use-modules (mod ...)) ; generics merging
               (set-module-uses! cm
                                 (cons (resolve-interface '(mod ...))
                                       (filter (lambda (m)
                                                 (not (equal? '(mod ...)
                                                              (module-name m))))
                                               (module-uses cm))))
               (set-module-uses! cm-public-interface
                                 (cons (resolve-interface '(mod ...))
                                       (filter (lambda (m)
                                                 (not (equal? '(mod ...)
                                                              (module-name m))))
                                               (module-uses cm-public-interface)))))
             ...)
           (hash-clear! (module-import-obarray cm))
           (module-modified cm)
           (hash-clear! (module-import-obarray cm-public-interface))
           (module-modified cm-public-interface))))))

  (export-syntax use-and-re-export-modules-prepended)


  (use-and-re-export-modules-prepended (oop goops))

  ;;
  ;; This ensures that the car of the guile module's public interface's module
  ;; uses list is the (kernel texmacs tm-define definitions) module's public
  ;; interface, and its' cadr is the (kernel texmacs tm-define) module's public
  ;; interface.
  ;;
  (use-and-re-export-modules-prepended (kernel texmacs tm-define))
  (use-and-re-export-modules-prepended (kernel texmacs tm-define definitions))

  )



(eval-when (compile eval load)

  (set-current-module (resolve-module '(guile-user)))

  (default-duplicate-binding-handler
    '(merge-generics merge-accessors replace warn-override-core warn last))

  (use-modules (oop goops))

  (define-method (write o)
    (write o (current-output-port)))

  (define-method (display o)
    (display o (current-output-port)))

  (use-modules (kernel texmacs tm-define))

  (use-modules (test-tm-define tests))

  )

