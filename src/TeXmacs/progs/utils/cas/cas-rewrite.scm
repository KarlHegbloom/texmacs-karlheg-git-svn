
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : cas-out.scm
;; DESCRIPTION : Conversion of content markup into presentation markup
;; TODO        : Parameterize by arbitrary grammar modules
;; COPYRIGHT   : (C) 2005  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (texmacs-module (utils cas cas-rewrite))
(define-module (utils cas cas-rewrite)
  :use-module (texmacs-core))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rewriting routines for putting expressions into operational forms
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (cas-map fun x)
  "Map function to arguments of outermost function of @x"
  (cond ((list? x) (map fun x))
	(else x)))

(define (cas-normal-associative-sub x op)
  (cond ((and (pair? x) (== (car x) op))
	 (append-map (cut cas-normal-associative-sub <> op) (cdr x)))
	(else (list (cas-map (cut cas-normal-associative <> op) x)))))

(tm-define (cas-normal-associative x op)
  "Makes all operations @op inside @x n-ary"
  (cond ((and (pair? x) (== (car x) op))
	 (cons op (append-map (cut cas-normal-associative-sub <> op) (cdr x))))
	(else (cas-map (cut cas-normal-associative <> op) x))))

(define (cas-opposite x)
  (cond ((== x 0) 0)
	((and (number? x) (> x 0)) (list '- x))
	((func? x '+) (cons '+ (map cas-opposite (cdr x))))
	((func? x '- 1) (cas-normal-opposites (cadr x)))
	((func? x '-)
	 (cons* '+ (cas-opposite (cadr x))
		   (map cas-normal-opposites (cddr x))))
	(else (with y (cas-normal-opposites x)
		(if (func? y '- 1) (cadr y) (list '- y))))))

(tm-define (cas-normal-opposites x)
  "Turn all subtractions inside @x into unary opposites"
  (cond ((and (number? x) (< x 0)) (list '- (- x)))
	((func? x '- 0) 0)
	((func? x '- 1) (cas-opposite (cadr x)))
	((func? x '-) (cons* '+ (cadr x) (map cas-opposite (cddr x))))
	((or (func? x '*) (func? x '/))
	 (let* ((l1 (map cas-normal-opposites (cdr x)))
		(l2 (map (lambda (y) (if (func? y '- 1) (cadr y) y)) l1))
		(l3 (map (lambda (y) (if (func? y '- 1) -1 1)) l1)))
	   (if (== (apply * l3) 1)
	       (cons (car x) l2)
	       (list '- (cons (car x) l2)))))
	(else (cas-map cas-normal-opposites x))))

(define (cas-inverse-pow x flag?)
  (let ((what (cas-normal-inverses (cadr x)))
	(pow  (cas-normal-inverses (caddr x))))
    (when (func? what '/ 1)
      (set! what (cadr what))
      (set! flag? (not flag?)))
    (when (func? pow '- 1)
      (set! pow (cadr pow))
      (set! flag? (not flag?)))
    (with y (if (== pow 1) what (list '^ what pow))
      (if flag? (list '/ y) y))))

(define (cas-inverse x)
  (cond ((== x 1) 1)
	((func? x '*) (cons '* (map cas-inverse (cdr x))))
	((func? x '/ 1) (normalize (cadr x)))
	((func? x '/)
	 (cons* '* (cas-inverse (cadr x))
		   (map cas-normal-inverses (cddr x))))
	((func? x '^ 2) (cas-inverse-pow x #t))
	(else (with y (cas-normal-inverses x)
		(if (func? y '/ 1) (cadr y) (list '/ y))))))

(tm-define (cas-normal-inverses x)
  "Turn all fractions inside @x into unary inverses"
  (cond ((func? x '/ 0) 1)
	((func? x '/ 1) (cas-inverse (cadr x)))
	((func? x '/)
	 (cons* '* (cas-normal-inverses (cadr x)) (map cas-inverse (cddr x))))
	((func? x '^ 2) (cas-inverse-pow x #f))
	(else (cas-map cas-normal-inverses x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sorting mathematical expressions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (cas-size x)
  "Size of @x as an expression"
  (cond ((null? x) 0)
	((pair? x) (+ (cas-size (car x)) (cas-size (cdr x))))
	(else 1)))

(define (cas->number x)
  (cond ((number? x) x)
	((and (func? x '- 1) (cas->number (cadr x))) => -)
	(else #f)))

(define (number->cas x)
  (if (< x 0) (list '- (- x)) x))

(define (cas-sub<? x y flag?)
  (let* ((nx (cas->number x))
	 (ny (cas->number y))
	 (sx (cas-size x))
	 (sy (cas-size y)))	 
    (cond ((or nx ny)
	   (cond ((not ny) flag?)
		 ((not nx) (not flag?))
		 (else (< nx ny))))
	  ((< sx sy) flag?)
	  ((> sx sy) (not flag?))
	  ((or (symbol? x) (symbol? y))
	   (cond ((not (symbol? y)) #t)
		 ((not (symbol? x)) #f)
		 (else (string<? (symbol->string x) (symbol->string y)))))
	  ((or (string? x) (string? y))
	   (cond ((not (string? y)) #t)
		 ((not (string? x)) #f)
		 (else (string<? x y))))
	  ((or (pair? x) (pair? y))
	   (cond ((not (pair? y)) #t)
		 ((not (pair? x)) #f)
		 ((cas-sub<? (car x) (car y) flag?) #t)
		 ((cas-sub<? (car y) (car x) flag?) #f)
		 (else (cas-sub<? (cdr x) (cdr y) flag?))))
	  (else (and (null? x) (nnull? y))))))

(tm-define (cas-default<=? x y)
  "Default lexicographical and total size ordering on expressions @x and @y"
  (or (cas-sub<? x y #t) (== x y)))

(tm-define (cas-sum<=? x y)
  "Ordering for @x and @y in sums"
  (or (cas-sub<? x y #f) (== x y)))

(tm-define (cas-product<=? x y)
  "Ordering for @x and @y in products"
  (cond ((and (func? x '^ 2) (not (func? y '^ 2)))
	 (cas-product<=? x `(^ ,y 1)))
	((and (func? y '^ 2) (not (func? x '^ 2)))
	 (cas-product<=? `(^ ,x 1) y))
	((or (cas->number x) (cas->number y))
	 (cond ((not (cas->number y)) #t)
	       ((not (cas->number x)) #f)
	       (else (< (cas->number x) (cas->number y)))))
	(else (cas-default<=? x y))))

(tm-define (cas-sort x)
  "Sort the expression @x"
  (cond ((func? x '+) (cons '+ (list-sort (cdr x) cas-sum<=?)))
	((func? x '*) (cons '* (list-sort (cdr x) cas-product<=?)))
	(else (cas-map cas-sort x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Polynomial sorting
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (cas-degree-max l)
  (if (null? l) -1
      (let ((d1 (car l))
	    (d2 (cas-degree-max (cdr l))))
	(if (cas-default<=? (cas-sort d1) (cas-sort d2)) d2 d1))))

(define (cas-degree-sum l)
  (if (null? l) 0
      (let* ((d1 (car l))
	     (d2 (cas-degree-sum (cdr l)))
	     (n1 (cas->number d1))
	     (n2 (cas->number d2))
	     (l1 (if (func? d1 '+) (cdr d1) (list d1)))
	     (l2 (if (func? d2 '+) (cdr d2) (list d2))))
	(cond ((== d1 0) d2)
	      ((== d2 0) d1)
	      ((and n1 n2) (cas->number (+ n1 n2)))
	      (else (cons '+ (append l1 l2)))))))

(define (cas-degree-opposite d)
  (cas-opposite d))

(tm-define (cas-degree x var)
  "Determine the degree of @x in @var"
  (cond ((or (func? x '-) (func? x '+))
	 (cas-degree-max (map (cut cas-degree <> var) (cdr x))))
	((func? x '*)
	 (cas-degree-sum (map (cut cas-degree <> var) (cdr x))))
	((func? x '/ 1)
	 (cas-degree-opposite (cas-degree (cadr x) var)))
	((func? x '/)
	 (cas-degree (cons '* (cadr x) (map cas-inverse (cddr x)))))
	((and (func? x '^) (== (cadr x) var)) (caddr x))
	((== x var) 1)
	(else 0)))

(define (cas-coefficient x)
  (cond ((number? x) x)
	((and (func? x '*) (pair? (cdr x)) (number? (cadr x))) (cadr x))
	(else 1)))

(define (cas-monomial x)
  (cond ((number? x) 1)
	((and (func? x '*) (pair? (cdr x)) (number? (cadr x)))
	 (cond ((null? (cddr x)) 1)
	       ((null? (cdddr x)) (caddr x))
	       (else (cons '* (cddr x)))))
	(else x)))

(tm-define (cas-term<=? x y vars)
  "Order @x and @y accordings degrees in variables in the list @vars"
  (if (null? vars)
      (let* ((mx (cas-monomial x))
	     (my (cas-monomial y))
	     (cx (cas-coefficient x))
	     (cy (cas-coefficient y)))
	(if (== mx my) (<= cx cy) (cas-sum<=? mx my)))
      (let* ((dx (cas-degree x (car vars)))
	     (dy (cas-degree y (car vars))))
	(if (== dx dy)
	    (cas-term<=? x y (cdr vars))
	    (cas-default<=? dy dx)))))

(tm-define (cas-radicals x)
  "Get all 'polynomial' variables of @x"
  (cond ((and (pair? x) (in? (car x) '(+ - * /)))
	 (append-map cas-radicals (cdr x)))
	((func? x '^ 2) (list (cadr x)))
	(else '())))

(define (list-no-duplicates l)
  (cond ((or (null? l) (null? (cdr l))) l)
	((== (car l) (cadr l)) (list-no-duplicates (cdr l)))
	(else (cons (car l) (list-no-duplicates (cdr l))))))

(tm-define (cas-polynomial-sort x)
  "Recursively sort the expression @x as a polynomial"
  (cond ((func? x '+)
	 (set! x (cons '+ (map cas-polynomial-sort (cdr x))))
	 (with vars (reverse (list-sort (cas-radicals x) cas-product<=?))
	   (set! vars (list-no-duplicates vars))
	   (cons '+ (list-sort (cdr x) (cut cas-term<=? <> <> vars)))))
	((func? x '*)
	 (set! x (cons '* (map cas-polynomial-sort (cdr x))))
	 (cons '* (list-sort (cdr x) cas-product<=?)))
	(else (cas-map cas-polynomial-sort x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Simplification of constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (cas-simplify-constants-sub l op neu)
  (cond ((null? l) '())
	((== (car l) neu) (cas-simplify-constants-sub (cdr l) op neu))
	((== (car l) 0) '(0)) ;; only occurs for multiplication
	((null? (cdr l)) l)
	((and (cas->number (car l)) (cas->number (cadr l)))
	 (cas-simplify-constants-sub
	  (cons (op (cas->number (car l)) (cas->number (cadr l)))
		(cddr l)) op neu))
	(else (cons (car l) (cas-simplify-constants-sub (cdr l) op neu)))))

(tm-define (cas-simplify-constants x)
  "Simplify constants in @x"
  (cond ((or (func? x '+) (func? x '*))
	 (let* ((l (map cas-simplify-constants (cdr x)))
		(op (if (== (car x) '+) + *))
		(neu (if (== (car x) '+) 0 1))
		(r (cas-simplify-constants-sub l op neu)))
	   (cond ((null? r) neu)
		 ((null? (cdr r)) (car r))
		 (else (cons (car x) r)))))
	((func? x '- 1)
	 (let* ((y (cas-simplify-constants (cadr x)))
		(n (cas->number y)))
	   (if n (number->cas (- n)) (list '- y))))
	((func? x '/ 1)
	 (let* ((y (cas-simplify-constants (cadr x)))
		(n (cas->number y)))
	   (if (in? n '(1 -1)) y (list '/ y))))
	((func? x '^ 2)
	 (let* ((y1 (cas-simplify-constants (cadr x)))
		(y2 (cas-simplify-constants (caddr x)))
		(n1 (cas->number y1))
		(n2 (cas->number y2)))
	   (cond ((== n1 0) 0)
		 ((== n1 1) 1)
		 ((== n2 0) 1)
		 ((== n2 1) y1)
		 (else (list '^ y1 y2)))))
	(else (cas-map cas-simplify-constants x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rewriting routines for putting an expression into a printable form
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (cas-arrange-subtractions x)
  "Avoid sums which start with unary minus when possible"
  (cond ((and (func? x '+) (> (length x) 2) (< (length x) 6)
	      (func? (cadr x) '- 1))
	 (with l (map cas-arrange-subtractions (cdr x))
	   (with i (list-find-index l (lambda (y) (not (func? y '- 1))))
	     (if (not i) (cas-map cas-arrange-subtractions x)
		 `(+ ,(list-ref l i)
		     ,@(list-head l i)
		     ,@(list-tail l (+ i 1)))))))
	(else (cas-map cas-arrange-subtractions x))))

(tm-define (cas-make-fractions x)
  "Turn n-ary multiplications with unary inverses inside @x into fractions"
  (cond ((func? x '/ 1) (list '/ 1 (cas-make-fractions (cadr x))))
	((func? x '*)
	 (receive (dl nl) (list-partition (cdr x) (lambda (y) (func? y '/ 1)))
	   (let* ((nl* (map cas-make-fractions nl))
		  (dl* (map cas-make-fractions (map cadr dl)))
		  (num (if (null? nl*) 1 (cons '* nl*)))
		  (den (if (null? dl*) 1 (cons '* dl*))))
	     (if (== den 1) num (list '/ num den)))))
	(else (cas-map cas-make-fractions x))))

(define (cas-make-binary-sub l op inv neu)
  (cond ((null? l) neu)
	((null? (cdr l)) (car l))
	((func? (cadr l) inv 1)
	 (cas-make-binary-sub
	  (cons (list inv (car l) (cadadr l)) (cddr l)) op inv neu))
	(else
	 (cas-make-binary-sub
	  (cons (list op (car l) (cadr l)) (cddr l)) op inv neu))))

(tm-define (cas-make-binary x op inv neu)
  "Make n-ary operations @op and unary inverses @inv inside @x binary"
  (with make-binary (cut cas-make-binary <> op inv neu)
    (cond ((and (pair? x) (== (car x) op))
	   (cas-make-binary-sub (map make-binary (cdr x)) op inv neu))
	  (else (cas-map make-binary x)))))
