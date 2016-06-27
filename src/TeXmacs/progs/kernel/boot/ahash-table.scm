;;; coding: utf-8

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : ahash-table.scm
;; DESCRIPTION : adaptive hash tables
;; COPYRIGHT   : (C) 2003  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (texmacs-module (kernel boot ahash-table))
(define-module (kernel boot ahash-table)
  :use-module (texmacs-core))

(use-modules (kernel boot abbrevs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Adaptive hash tables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public make-ahash-table #f)
(define-public ahash-ref #f)
(define-public ahash-get-handle #f)
(define-public ahash-size #f)
(define-public ahash-set! #f)
(define-public ahash-remove! #f)
(define-public ahash-fold #f)
(define-public ahash-table->list #f)
(if (vector? (make-hash-table 1))
    (begin ;; old style
      (set! make-ahash-table
            (lambda ()
              (cons (make-hash-table 1) 0)))

      (set! ahash-ref
            (lambda (h key)
              (hash-ref (car h) key)))

      (set! ahash-get-handle
            (lambda (h key)
              (hash-get-handle (car h) key)))

      (set! ahash-size (lambda (h) (cdr h)))

      (set! ahash-slots!
            (lambda (h new-size)
              (let ((new-h (make-hash-table new-size)))
                (hash-fold (lambda (key value dummy) (hash-set! new-h key value))
                           #f (car h))
                (set-car! h new-h))))

      (set! ahash-set!
            (lambda (h key value)
              (if (hash-get-handle (car h) key)
                  (hash-set! (car h) key value)
                  (begin
                    (if (>= (cdr h) (vector-length (car h)))
                        (ahash-slots! h (+ (* 2 (vector-length (car h))) 1)))
                    (set-cdr! h (+ (cdr h) 1))
                    (hash-set! (car h) key value)))))

      (set! ahash-remove!
            (lambda (h key)
              (let ((removed (hash-remove! (car h) key)))
                (if removed
                    (begin
                      (set-cdr! h (- (cdr h) 1))
                      (if (< (+ (* 4 (cdr h)) 1) (vector-length (car h)))
                          (ahash-slots! h (quotient (vector-length (car h)) 2)))))
                removed)))

      (set! ahash-fold
            (lambda (folder init h)
              (hash-fold folder init (car h))))

      (set! ahash-table->list
            (lambda (h)
              (hash-fold acons '() (car h)))))

    (begin ;; new style
      (set! make-ahash-table make-hash-table)
      (set! ahash-ref hash-ref)
      (set! ahash-get-handle hash-get-handle)
      (set! ahash-size
            (lambda (h)
              (hash-fold (lambda (key value seed) (+ 1 seed)) 0 h)))
      (set! ahash-set! hash-set!)
      (set! ahash-remove! hash-remove!)
      (set! ahash-fold hash-fold)
      (set! ahash-table->list
            (lambda (h)
              (hash-fold acons '() h)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Extra routines on adaptive hash tables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (list->ahash-table l)
  (let ((t (make-ahash-table)))
    (for-each (lambda (x) (ahash-set! t (car x) (cdr x))) l)
    t))

(define-public-macro (ahash-with t var val . body)
  (let ((old-val (gensym))
	(ret-val (gensym)))
    `(with ,old-val (ahash-ref ,t ,var)
       (ahash-set! ,t ,var ,val)
       (with ,ret-val (begin ,@body)
	 (ahash-set! ,t ,var ,old-val)
	 ,ret-val))))

(define-public (ahash-table-invert t)
  (let* ((l (ahash-table->list t))
	 (u (map (lambda (x) (cons (cdr x) (car x))) l)))
    (list->ahash-table u)))

(define-public (ahash-table-append . tl)
  (with ls (map ahash-table->list tl)
    (list->ahash-table (apply append ls))))

(define-public (ahash-table-map fun t)
  (let* ((l (ahash-table->list t))
	 (r (map (lambda (x) (cons (car x) (fun (cdr x)))) l)))
    (list->ahash-table r)))

(define-public (ahash-ref* h var val)
  (or (ahash-ref h var) val))

(define-public (ahash-table-select t l)
  (let ((r (make-ahash-table)))
    (for-each (lambda (x)
                (if (ahash-ref t x) (ahash-set! r x (ahash-ref t x)))) l)
    r))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dictionaries
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (fill-dictionary-entry d key im)
  (if (nnull? key)
      (begin
	(ahash-set! d (car key) im)
	(fill-dictionary-entry d (cdr key) im))))

(define-public (fill-dictionary d l)
  "Fill hash table @d with list of entries @l"
  ;; Note: depreciated
  (if (nnull? l)
      (begin
	(let* ((r (reverse (car l))))
	  (fill-dictionary-entry d (cdr r) (car r)))
	(fill-dictionary d (cdr l)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Simple definition of hash tables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (define-table-decls h l)
  (define (insert binding)
    (ahash-set! h (car binding) (cdr binding)))
  (for-each insert l))

(define-public-macro (define-table name . l)
  `(begin
     (when (not (defined? ',name))
       (if (defined? 'tm-define)
           (tm-define ,name (make-ahash-table))
           (define-public ,name (make-ahash-table))))
     (define-table-decls ,name ,(list 'quasiquote l))))

(define-public-macro (extend-table name . l)
  `(define-table-decls ,name ,(list 'quasiquote l)))
