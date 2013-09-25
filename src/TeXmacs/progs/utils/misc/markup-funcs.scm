
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : markup-funcs.scm
;; DESCRIPTION : additional rendering macros written in scheme
;; COPYRIGHT   : (C) 2001  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (utils misc markup-funcs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TeXmacs version and release
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (texmacs-version-release* t)
  (:secure #t)
  (texmacs-version-release (tree->string t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Map
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (ext-map fun to)
  (:secure #t)
  (with (op . args) (tree->list to)
    (with f (lambda (x) (list 'compound fun x))
      (list 'quote (cons 'tuple (map f args))))))

(tm-define (ext-concat-tuple tup sep fin)
  (:secure #t)
  (with (op . l) (tree->list tup)
    (cond ((null? l) "")
	  ((null? (cdr l)) (car l))
	  (else `(concat ,(car l)
			 ,@(map (lambda (x) (list 'concat sep x)) (cDdr l))
			 ,(if (tm-equal? fin '(uninit)) sep fin)
			 ,(cAr l))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rewriting document titles as a function of several style parameters
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (rewrite-select pat)
  (if (atomic-tree? pat)
      (with s (tree->string pat)
	(if (not (string-starts? s "("))
	    (string->object s)
	    s))
      (with (op . r) (tree->list pat)
	(cond ((== op 'pat-any) :%1)
	      ((== op 'pat-any-repeat) :*)
	      ((== op 'pat-or) (cons :or (map rewrite-select r)))
	      ((== op 'pat-and) (cons :and (map rewrite-select r)))
	      ((== op 'pat-group) (cons :group (map rewrite-select r)))
	      ((== op 'pat-and-not) (cons :and-not (map rewrite-select r)))
	      (else #f)))))

(tm-define (ext-select body args)
  (:secure #t)
  (with (op body2 . pat) (tree->list args)
    (list 'quote (cons 'tuple (select body (map rewrite-select pat))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Listings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (ext-listing-row body row)
  `(row (cell (with "color" "dark grey" "prog-language" "verbatim"
                    ,(number->string (+ row 1))))
        (cell (document ,(tm-ref body row)))))

(tm-define (ext-listing body)
  (:secure #t)
  (if (tm-func? body 'document)
      `(tformat
         (twith "table-width" "1par")
         (twith "table-hmode" "exact")
         (twith "table-hyphen" "y")
         (cwith "1" "-1" "1" "1" "cell-halign" "r")
         (cwith "1" "-1" "1" "1" "cell-lsep" "0em")
         (cwith "1" "-1" "2" "2" "cell-halign" "l")
         (cwith "1" "-1" "2" "2" "cell-rsep" "0em")
         (cwith "1" "-1" "2" "2" "cell-hpart" "1")
         (cwith "1" "-1" "2" "2" "cell-hyphen" "t")
         (table ,@(map (lambda (row) (ext-listing-row body row))
                       (.. 0 (tm-arity body)))))
      body))
