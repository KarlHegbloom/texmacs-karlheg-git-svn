;;; coding: utf-8
;;;; 	Copyright (C) 1996, 1997, 1998, 1999 Free Software Foundation, Inc.
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
;;;; The author can be reached at djurfeldt@nada.kth.se
;;;; Mikael Djurfeldt, SANS/NADA KTH, 10044 STOCKHOLM, SWEDEN
;;;; (I didn't write this!)
;;;;


;;; *********************************************************************
;;; * This is the Guile side of the Emacs interface                     *
;;; * Experimental hACK---the real version will be coming soon (almost) *
;;; *********************************************************************

;;; {Session support for Emacs}
;;;

(define-module (ice-9 emacs)
  :use-module (ice-9 debug)
  :use-module (ice-9 threads)
  :use-module (ice-9 session)
  :no-backtrace)

(define emacs-escape-character #\sub)

(define emacs-output-port (current-output-port))

(define (make-emacs-command char)
  (let ((cmd (list->string (list emacs-escape-character char))))
    (lambda ()
      (display cmd emacs-output-port))))

(define enter-input-wait  (make-emacs-command #\s))
(define exit-input-wait   (make-emacs-command #\f))
(define enter-read-character #\r)
(define sending-error	  (make-emacs-command #\F))
(define sending-backtrace (make-emacs-command #\B))
(define sending-result    (make-emacs-command #\x))
(define end-of-text	  (make-emacs-command #\.))
(define no-stack	  (make-emacs-command #\S))
(define no-source	  (make-emacs-command #\R))

;; {Error handling}
;;

(add-hook! before-backtrace-hook sending-backtrace)
(add-hook! after-backtrace-hook end-of-text)
(add-hook! before-error-hook sending-error)
(add-hook! after-error-hook end-of-text)

;; {Repl}
;;

(set-current-error-port emacs-output-port)

(add-hook! before-read-hook
	   (lambda ()
	     (enter-input-wait)
	     (force-output emacs-output-port)))

(add-hook! after-read-hook
	   (lambda ()
	     (exit-input-wait)
	     (force-output emacs-output-port)))

;;; {Misc.}

(define (make-emacs-load-port orig-port)
  (letrec ((read-char-fn  (lambda args
			    (let ((c (read-char orig-port)))
			      (if (eq? c #\soh)
				  (throw 'end-of-chunk)
				  c)))))
    
    (make-soft-port
     (vector #f #f #f
	     read-char-fn
	     (lambda () (close-port orig-port)))
     "r")))

(set-current-input-port (make-emacs-load-port (current-input-port)))

(define (result-to-emacs exp)
  (sending-result)
  (write exp emacs-output-port)
  (end-of-text)
  (force-output emacs-output-port))

(define load-acknowledge (make-emacs-command #\l))

(define load-port (current-input-port))

(define (flush-line port)
  (let loop ((c (read-char port)))
    (if (not (eq? c #\nl))
	(loop (read-char port)))))

(define whitespace-chars (list #\space #\tab #\nl #\np))

(define (flush-whitespace port)
  (catch 'end-of-chunk
	 (lambda ()
	   (let loop ((c (read-char port)))
	     (cond ((eq? c the-eof-object)
		    (error "End of file while recieving Emacs data"))
		   ((memq c whitespace-chars) (loop (read-char port)))
		   ((eq? c #\;) (flush-line port) (loop (read-char port)))
		   (else (unread-char c port))))
	   #f)
	 (lambda args
	   (read-char port) ; Read final newline
	   #t)))

(define (emacs-load filename linum colnum module interactivep)
  (set-port-filename! %%load-port filename)
  (set-port-line! %%load-port linum)
  (set-port-column! %%load-port colnum)
  (lazy-catch #t
	      (lambda ()
		(let loop ((endp (flush-whitespace %%load-port)))
		  (if (not endp)
		      (begin
			(save-module-excursion
			 (lambda ()
			   (if module
			       (set-current-module (resolve-module module #f)))
			   (let ((result
				  (start-stack read-and-eval!
					       (read-and-eval! %%load-port))))
			     (if interactivep
				 (result-to-emacs result)))))
			(loop (flush-whitespace %%load-port)))
		      (begin
			(load-acknowledge)))
		  (set-port-filename! %%load-port #f)))	;reset port filename
	      (lambda (key . args)
		(set-port-filename! %%load-port #f)
		(cond ((eq? key 'end-of-chunk)
		       (fluid-set! the-last-stack #f)
		       (set! stack-saved? #t)
		       (scm-error 'misc-error
				  #f
				  "Incomplete expression"
				  '()
				  '()))
		      ((eq? key 'exit))
		      (else
		       (save-stack 2)
		       (catch 'end-of-chunk
			      (lambda ()
				(let loop ()
				  (read-char %%load-port)
				  (loop)))
			      (lambda args
				#f))
		       (apply throw key args))))))

(define (emacs-eval-request form)
  (result-to-emacs (eval form)))

;;*fixme* Not necessary to use flags no-stack and no-source
(define (get-frame-source frame)
  (if (or (not (fluid-ref the-last-stack))
	  (>= frame (stack-length (fluid-ref the-last-stack))))
      (begin
	(no-stack)
	#f)
      (let* ((frame (stack-ref (fluid-ref the-last-stack)
			       (frame-number->index frame)))
	     (source (frame-source frame)))
	(or source
	    (begin (no-source)
		   #f)))))

(define (emacs-select-frame frame)
  (let ((source (get-frame-source frame)))
    (if source
	(let ((fname (source-property source 'filename))
	      (line (source-property source 'line))
	      (column (source-property source 'column)))
	  (if (and fname line column)
	      (list fname line column)
	      (begin (no-source)
		     '())))
	'())))

(define (object->string x . method)
  (with-output-to-string
    (lambda ()
      ((if (null? method)
	   write
	   (car method))
       x))))

(define (format template . rest)
  (let loop ((chars (string->list template))
	     (result '())
	     (rest rest))
    (cond ((null? chars) (list->string (reverse result)))
	  ((char=? (car chars) #\%)
	   (loop (cddr chars)
		 (append (reverse
			  (string->list
			   (case (cadr chars)
			     ((#\S) (object->string (car rest)))
			     ((#\s) (object->string (car rest) display)))))
			 result)
		 (cdr rest)))
	  (else (loop (cdr chars) (cons (car chars) result) rest)))))

(define (error-args->string args)
  (let ((msg (apply format (caddr args) (cadddr args))))
    (if (symbol? (cadr args))
	(string-append (symbol->string (cadr args))
		       ": "
		       msg)
	msg)))

(define (emacs-frame-eval frame form)
  (let ((source (get-frame-source frame)))
    (if source
	(catch #t
	       (lambda ()
		 (list 'result
		       (object->string
			(local-eval (with-input-from-string form read)
				    (memoized-environment source)))))
	       (lambda args
		 (list (car args)
		       (error-args->string args))))
	(begin
	  (no-source)
	  '()))))

(define (emacs-symdoc symbol)
  (if (or (not (module-bound? (current-module) symbol))
	  (not (procedure? (eval symbol))))
      'nil
      (procedure-documentation (eval symbol))))

;;; A fix to get the emacs interface to work together with the module system.
;;;
(variable-set! (builtin-variable '%%load-port) load-port)
(variable-set! (builtin-variable '%%emacs-load) emacs-load)
(variable-set! (builtin-variable '%%emacs-eval-request) emacs-eval-request)
(variable-set! (builtin-variable '%%emacs-select-frame) emacs-select-frame)
(variable-set! (builtin-variable '%%emacs-frame-eval) emacs-frame-eval)
(variable-set! (builtin-variable '%%emacs-symdoc) emacs-symdoc)
(variable-set! (builtin-variable '%%apropos-internal) apropos-internal)
