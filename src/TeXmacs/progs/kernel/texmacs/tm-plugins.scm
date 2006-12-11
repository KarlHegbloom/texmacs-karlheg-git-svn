
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : tm-plugins.scm
;; DESCRIPTION : Configuration of plugins
;; COPYRIGHT   : (C) 1999  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (kernel texmacs tm-plugins)
  (:use (kernel texmacs tm-define) (kernel texmacs tm-modes)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lazy exports from other modules
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(lazy-define (utils plugins plugin-convert) plugin-supports-math-input-ref)
(lazy-define (utils plugins plugin-convert) plugin-math-input)
(lazy-define (utils plugins plugin-cmd) plugin-serializer-set!)
(lazy-define (utils plugins plugin-cmd) plugin-commander-set!)
(lazy-define (utils plugins plugin-cmd) plugin-supports-completions?)
(lazy-define (utils plugins plugin-cmd) plugin-supports-completions-set!)
(lazy-define (utils plugins plugin-cmd) plugin-supports-input-done?)
(lazy-define (utils plugins plugin-cmd) plugin-supports-input-done-set!)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Connection types for plugins
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define connection-defined (make-ahash-table))
(define connection-default (make-ahash-table))
(define connection-variant (make-ahash-table))
(define connection-varlist (make-ahash-table))
(define connection-handler (make-ahash-table))

(define (connection-setup name val . opt)
  (ahash-set! connection-defined name #t)
  (if (null? opt)
      (ahash-set! connection-default name val)
      (with l (ahash-ref connection-varlist name)
	(if (not l) (set! l '("default")))
	(ahash-set! connection-variant (list name (car opt)) val)
	(ahash-set! connection-varlist name (rcons l (car opt))))))

(define-public (connection-defined? name)
  (lazy-plugin-force)
  (ahash-ref connection-defined name))

(define-public (connection-info name session)
  (lazy-plugin-force)
  (with pos (string-index session #\:)
    (if pos (connection-info name (substring session 0 pos))
	(with val (ahash-ref connection-variant (list name session))
	  (if val val (ahash-ref connection-default name))))))

(define (connection-insert-handler name channel routine)
  (if (not (ahash-ref connection-handler name))
      (ahash-set! connection-handler name '()))
  (ahash-set! connection-handler name
	      (cons (list 'tuple channel routine)
		    (ahash-ref connection-handler name))))

(define-public (connection-get-handlers name)
  (lazy-plugin-force)
  (with r (ahash-ref connection-handler name)
    (if r (cons 'tuple r) '(tuple))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Menu for the supported sessions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define supported-sessions-list '())
(define supported-sessions-table (make-ahash-table))

(define (supported-sessions-add name menu-name)
  (if (symbol? name) (set! name (symbol->string name)))
  (set! supported-sessions-list (cons name supported-sessions-list))
  (ahash-set! supported-sessions-table name menu-name))

(define (supported-sessions-menu-entry name)
  (define (menu-item variant)
    (list variant (lambda () (make-session name variant))))
  (let* ((menu-name (ahash-ref supported-sessions-table name))
	 (l (ahash-ref connection-varlist name)))
    (if (not l)
	(list menu-name (lambda () (make-session name "default")))
	`(-> ,menu-name ,@(map menu-item l)))))

(define-public (supported-sessions-menu)
  (lazy-plugin-force)
  (with l (list-sort supported-sessions-list string<=?)
    (menu-dynamic ,@(map supported-sessions-menu-entry l))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Supported scripting languages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define supported-scripts-list '())
(define supported-scripts-table (make-ahash-table))

(define (supported-scripts-add name menu-name)
  (if (symbol? name) (set! name (symbol->string name)))
  (set! supported-scripts-list (cons name supported-scripts-list))
  (ahash-set! supported-scripts-table name menu-name))

(tm-define (supported-scripts-menu-entry name)
  (let* ((fun `(lambda () (init-env "prog-scripts" ,name)))
	 (menu-name (ahash-ref supported-scripts-table name)))
    (list menu-name (eval fun))))

(define-public (supported-scripts-menu)
  (lazy-plugin-force)
  (with l (list-sort supported-scripts-list string<=?)
    (menu-dynamic ,@(map supported-scripts-menu-entry l))))

(tm-define (local-supported-scripts-menu-entry name)
  (let* ((fun `(lambda () (make-with "prog-scripts" ,name)))
	 (menu-name (ahash-ref supported-scripts-table name)))
    (list menu-name (eval fun))))

(define-public (local-supported-scripts-menu)
  (lazy-plugin-force)
  (with l (list-sort supported-scripts-list string<=?)
    (menu-dynamic ,@(map local-supported-scripts-menu-entry l))))

(tm-define (scripts-preferences-menu-entry name)
  (let* ((fun `(lambda () (set-preference "scripting language" ,name)))
	 (menu-name (ahash-ref supported-scripts-table name)))
    (list menu-name (eval fun))))

(define-public (scripts-preferences-menu)
  (lazy-plugin-force)
  (with l (list-sort supported-scripts-list string<=?)
    (menu-dynamic ,@(map scripts-preferences-menu-entry l))))

(tm-define (supports-scripts? name)
  (if (symbol? name) (set! name (symbol->string name)))
  (not (not (ahash-ref supported-scripts-table name))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configuration of plugins
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public plugin-old-data-table (make-ahash-table))
(define-public plugin-data-table (make-ahash-table))

(define (plugin-configure-cmd name cmd)
  (cond ((or (func? cmd :require 1) (func? cmd :version 1))
	 (ahash-set! plugin-data-table name ((second cmd))))
        ((func? cmd :setup 1)
	 (if (!= (ahash-ref plugin-data-table name)
		 (ahash-ref plugin-old-data-table name))
	     ((second cmd))))
	((func? cmd :prioritary 1)
	 (ahash-set! plugin-data-table (list name :prioritary) (cadr cmd)))
        ((func? cmd :initialize 1)
	 ((second cmd)))
	((func? cmd :launch 1)
	 (connection-setup name `(tuple "pipe" ,(second cmd))))
	((func? cmd :launch 2)
	 (connection-setup name `(tuple "pipe" ,(third cmd)) (cadr cmd)))
	((func? cmd :socket 2)
	 (connection-setup name `(tuple "socket" ,(second cmd) ,(third cmd))))
	((func? cmd :socket 3)
	 (connection-setup
	  name `(tuple "socket" ,(third cmd) ,(fourth cmd)) (cadr cmd)))
	((func? cmd :link 3)
	 (connection-setup
	  name `(tuple "dynlink" ,(second cmd) ,(third cmd) ,(fourth cmd))))
	((func? cmd :link 4)
	 (connection-setup
	  name `(tuple "dynlink" ,(third cmd) ,(fourth cmd) ,(fifth cmd))
	  (cadr cmd)))
	((func? cmd :handler 2)
	 (connection-insert-handler
	  name (second cmd) (symbol->string (third cmd))))
	((func? cmd :session 1)
	 (supported-sessions-add name (second cmd)))
	((func? cmd :scripts 1)
	 (supported-scripts-add name (second cmd)))
	((func? cmd :filter-in 1)
	 (noop))
	((func? cmd :serializer 1)
	 (plugin-serializer-set! name (second cmd)))
	((func? cmd :commander 1)
	 (plugin-commander-set! name (second cmd)))
	((func? cmd :tab-completion 1)
	 (if (second cmd) (plugin-supports-completions-set! name)))
	((func? cmd :test-input-done 1)
	 (if (second cmd) (plugin-supports-input-done-set! name)))))

(define-public (plugin-configure-cmds name cmds)
  "Helper function for plugin-configure"
  (if (and (nnull? cmds) (ahash-ref plugin-data-table name))
      (begin
        (plugin-configure-cmd name (car cmds))
	(plugin-configure-cmds name (cdr cmds)))))

(define-public (plugin-configure-sub cmd)
  "Helper function for plugin-configure"
  (if (and (list? cmd) (= (length cmd) 2)
	   (in? (car cmd) '(:require :version :setup :initialize)))
      (list (car cmd) (list 'unquote `(lambda () ,(cadr cmd))))
      cmd))

(define-public-macro (plugin-configure name2 . options)
  "Declare and configure plug-in with name @name2 according to @options"
  (let* ((name (if (string? name2) name2 (symbol->string name2)))
	 (in-name (string->symbol (string-append "in-" name "%")))
	 (name-scripts (string->symbol (string-append name "-scripts%"))))
    `(begin
       (texmacs-modes (,in-name (== (get-env "prog-language") ,name)))
       (texmacs-modes (,name-scripts (== (get-env "prog-scripts") ,name)))
       (ahash-set! plugin-data-table ,name #t)
       (plugin-configure-cmds ,name
	 ,(list 'quasiquote (map plugin-configure-sub options))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initialization of plugins
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define plugin-initialize-todo (make-ahash-table))

(define (plugin-load-setup)
  (if (url-exists? "$TEXMACS_HOME_PATH/system/setup.scm")
      (set! plugin-old-data-table
	    (list->ahash-table
	      (load-object "$TEXMACS_HOME_PATH/system/setup.scm")))))

(define (plugin-save-setup)
  (if (!= plugin-old-data-table plugin-data-table)
      (save-object "$TEXMACS_HOME_PATH/system/setup.scm"
		   (ahash-table->list plugin-data-table))))

(define (plugin-all-initialized?)
  (with l (ahash-table->list plugin-initialize-todo)
    (not (list-or (map cdr l)))))

(define-public (plugin-initialize name*)
  "Initialize plugin with name @name*"
  (if (== (ahash-size plugin-old-data-table) 0) (plugin-load-setup))
  (if (ahash-ref plugin-initialize-todo name*)
      (let* ((name (symbol->string name*))
	     (file (string-append "plugins/" name "/progs/init-" name ".scm"))
	     (u (url "$TEXMACS_HOME_PATH:$TEXMACS_PATH" file)))
	(ahash-set! plugin-initialize-todo name* #f)
	(if (url-exists? u)
	    (with fname (url-materialize u "r")
	      ;;(display* "loading plugin " name* "\n")
	      (load fname)))
	(if (plugin-all-initialized?) (plugin-save-setup)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lazy initialization of plugins
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (lazy-plugin-initialize name)
  "Initialize the plug-in @name in a lazy way"
  (ahash-set! plugin-initialize-todo name #t)
  (if (eval (ahash-ref plugin-old-data-table (list name :prioritary)))
      (plugin-initialize name)
      (delayed
       (:idle 1000)
       (plugin-initialize name))))

(define plugin-initialize-done? #f)

(define-public (lazy-plugin-force)
  "Force all lazy plugin initializations to take place"
  (if plugin-initialize-done? #f
      (with l (ahash-table->list plugin-initialize-todo)
	(for-each plugin-initialize (map car l))
	(set! plugin-initialize-done? #t)
	#t)))
