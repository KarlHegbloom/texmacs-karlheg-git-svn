;;;; readline.scm --- support functions for command-line editing
;;;;
;;;; 	Copyright (C) 1997, 1999, 2000 Free Software Foundation, Inc.
;;;; 
;;;; This program is free software; you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation; either version 2, or (at your option)
;;;; any later version.
;;;; 
;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;; 
;;;; You should have received a copy of the GNU General Public License
;;;; along with this software; see the file COPYING.  If not, write to
;;;; the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
;;;; Boston, MA 02111-1307 USA
;;;; 
;;;; Contributed by Daniel Risacher <risacher@worldnet.att.net>.
;;;; Extensions based upon code by
;;;; Andrew Archibald <aarchiba@undergrad.math.uwaterloo.ca>.

(define-module (ice-9 readline)
  :use-module (ice-9 session)
  :use-module (ice-9 regex)
  :no-backtrace)

;;; Dynamically link the glue code for accessing the readline library,
;;; but only when it isn't already present.

(if (not (feature? 'readline))
    (dynamic-call "scm_init_readline" (dynamic-link "libguilereadline.dll")))

(if (not (feature? 'readline))
    (scm-error 'misc-error
	       #f
	       "readline is not provided in this Guile installation"
	       '()
	       '()))

;;; MDJ 980513 <djurfeldt@nada.kth.se>:
;;; There should probably be low-level support instead of this code.

(define prompt "")
(define prompt2 "")
(define input-port (current-input-port))
(define output-port (current-output-port))
(define read-hook #f)

(define (make-readline-port)
  (let ((read-string "")
	(string-index -1))
    (letrec ((get-character
	      (lambda ()
		(cond 
		 ((eof-object? read-string)
		  read-string)
		 ((>= string-index (string-length read-string))
		  (begin
		    (set! string-index -1)
		    #\nl))
		 ((= string-index -1)
		  (begin
		    (set! read-string
			  (%readline (if (string? prompt)
					 prompt
					 (prompt))
				     input-port
				     output-port
				     read-hook))
		    (set! string-index 0)
		    (if (not (eof-object? read-string))
			(begin
			  (or (string=? read-string "")
			      (begin
				(add-history read-string)
				(set! prompt prompt2)))
			  (get-character))
			read-string)))
		 (else 
		  (let ((res (string-ref read-string string-index)))
		    (set! string-index (+ 1 string-index))
		    res))))))	      
      (make-soft-port
       (vector write-char display #f get-character #f)
       "rw"))))

;;; We only create one readline port.  There's no point in having
;;; more, since they would all share the tty and history ---
;;; everything except the prompt.  And don't forget the
;;; compile/load/run phase distinctions.  Also, the readline library
;;; isn't reentrant.
(define the-readline-port #f)

(define history-variable "GUILE_HISTORY")
(define history-file (string-append (getenv "HOME") "/.guile_history"))

(define-public readline-port
  (let ((do (lambda (r/w)
	      (if (memq 'history-file (readline-options-interface))
		  (r/w (or (getenv history-variable)
			   history-file))))))
    (lambda ()
      (if (not the-readline-port)
	  (begin
	    (do read-history) 
	    (set! the-readline-port (make-readline-port))
	    (add-hook! exit-hook (lambda () (do write-history)))))
      the-readline-port)))

;;; The user might try to use readline in his programs.  It then
;;; becomes very uncomfortable that the current-input-port is the
;;; readline port...
;;;
;;; Here, we detect this situation and replace it with the
;;; underlying port.
;;;
;;; %readline is the low-level readline procedure.

(define-public (readline . args)
  (let ((prompt prompt)
	(inp input-port))
    (cond ((not (null? args))
	   (set! prompt (car args))
	   (set! args (cdr args))
	   (cond ((not (null? args))
		  (set! inp (car args))
		  (set! args (cdr args))))))
    (apply %readline
	   prompt
	   (if (eq? inp the-readline-port)
	       input-port
	       inp)
	   args)))

(define-public (set-readline-prompt! p . rest)
  (set! prompt p)
  (if (not (null? rest))
      (set! prompt2 (car rest))))

(define-public (set-readline-input-port! p)
  (set! input-port p))

(define-public (set-readline-output-port! p)
  (set! output-port p))

(define-public (set-readline-read-hook! h)
  (set! read-hook h))

(if (feature? 'regex)
    (begin
      (define-public apropos-completion-function
	(let ((completions '()))
	  (lambda (text cont?)
	    (if (not cont?)
		(set! completions
		      (map symbol->string
			   (apropos-internal
			    (string-append "^" (regexp-quote text))))))
	    (if (null? completions)
		#f
		(let ((retval (car completions)))
		  (begin (set! completions (cdr completions))
			 retval))))))

      (set! *readline-completion-function* apropos-completion-function)
      ))

(define-public (activate-readline)
  (if (and (isatty? (current-input-port))
	   (not (and (module-defined? the-root-module
				      'use-emacs-interface)
		     use-emacs-interface)))
      (let ((read-hook (lambda () (run-hook before-read-hook))))
	(set-current-input-port (readline-port))
	(set! repl-reader
	      (lambda (prompt)
		(dynamic-wind
		    (lambda ()
		      (set-readline-prompt! prompt "... ")
		      (set-readline-read-hook! read-hook))
		    (lambda () (read))
		    (lambda ()
		      (set-readline-prompt! "" "")
		      (set-readline-read-hook! #f)))))
	(set! (using-readline?) #t))))
