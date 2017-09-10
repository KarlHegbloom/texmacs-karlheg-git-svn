;;; -*- scheme -*-
;;;
(assert-load-verbosity #t)
;; (read-enable  'r7rs-symbols)
;; (print-enable 'r7rs-symbols)
(read-enable 'square-brackets)
(read-enable 'keywords #f)
(read-enable 'positions)

(eval-when (compile eval load)
  (add-to-load-path
   "/home/karlheg/src/TeXmacs/texmacs-git-svn-guile-2.2/exploratory-programming")

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

;; (tm-define (blah x y)
;;   (display* '(blah x " " y "\n")))

;; (tm-define (blah x y)
;;   (#:require (in-text?))
;;   (display* '(blah-in-text x " " y "\n")))

