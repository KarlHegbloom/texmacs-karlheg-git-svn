
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : locus-edit.scm
;; DESCRIPTION : editing routines for loci
;; COPYRIGHT   : (C) 2006  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (texmacs-module (link locus-edit))

(define-module (link locus-edit)
  :use-module (texmacs-core))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Unique identifiers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (base256->number s)
  (if (== s "") 0
      (+ (* 256 (base256->number (string-drop-right s 1)))
	 (char->integer (string-ref s (- (string-length s) 1))))))

(define seed-val (+ (* 4294967296 (abs (texmacs-time)))
		    (* 65536 (getpid))
		    ;;(if (defined? 'cuserid) (base256->number (cuserid)) 0)
		    0))

(define texmacs-seed (seed->random-state seed-val))
(define texmacs-serial-id (random 9342813113834066795298815 texmacs-seed))

(define (base64 x)
  (if (== x 0) '()
      (append (base64 (quotient x 64))
	      (list (remainder x 64)))))

(define (aschar x)
  (cond ((< x 10) (integer->char (+ x 48)))
	((< x 36) (integer->char (+ x 55)))
	((< x 62) (integer->char (+ x 61)))
	((== x 62) #\{)
	(else #\})))

(define (number->base64 x)
  (list->string (map aschar (base64 x))))

(tm-define (create-unique-id)
  (:synopsis "Create a unique file or locus identifier")
  (set! texmacs-serial-id (+ texmacs-serial-id 1))
  (string-append "+" (number->base64 texmacs-serial-id)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Creation of loci
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (make-locus)
  (with t (if (selection-active-any?) (selection-tree) "")
    (if (selection-active-any?) (clipboard-cut "null"))
    (insert-go-to `(locus (id ,(create-unique-id)) ,t)
		  (cons 1 (path-end t '())))))

(tm-define (locus-id t)
  (:synopsis "Return the unique identifier of the locus @t or #f.")
  (and (tm-func? t 'locus)
       (>= (tm-length t) 2)
       (tm-func? (tm-ref t 0) 'id 1)
       (tree-atomic? (tm-ref t 0 0))
       (tree->string (tm-ref t 0 0))))

(tm-define (locus-set id t)
  (:synopsis "Replace the contents of all loci with identifier @id by @t.")
  (for-each (lambda (l) (tree-assign! l t)) (id->trees id)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Links at the locus
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (locus-insert-link t ln)
  (tree-insert! t (- (tree-arity t) 1) `(,(tree-copy ln))))

(tm-define (locus-remove-link ln)
  (tree-remove (tree-up ln) (tree-index ln) 1))

(define (locus-remove-match ln match-with)
  (if (== ln match-with) (locus-remove-link ln)))

(tm-define (locus-remove-all-links t ln)
  (for-each (cut locus-remove-match <> ln)
	    (reverse (cDr (tree-children t)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utility routines for loci
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tree->locus t)
  (and-with p (tree-up t)
    (and (tm-func? p 'locus) p)))

(tm-define (id->loci id)
  (filter-map tree->locus (id->trees id)))
