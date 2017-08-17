
(eval-when (compile eval load)
  (set! %load-path
        (cons "/home/karlheg/src/TeXmacs/texmacs-git-svn-guile-2.2/exploratory-programming"
              %load-path))

  (set-current-module (resolve-module '(guile)))

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

  (use-and-re-export-modules (kernel texmacs tm-define))

  (use-modules (kernel texmacs tm-define definitions))

  (let* ((cm (module-public-interface (current-module)))
         (mu (module-uses cm))
         (dm (resolve-interface '(kernel texmacs tm-define definitions))))
    (unless (memq dm mu)
      (set-module-uses! cm (cons dm mu)))))

(eval-when (compile eval load)
  (set-current-module (resolve-module '(guile-user)))
  (use-modules (oop goops))
  )

