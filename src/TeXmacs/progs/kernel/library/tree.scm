
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : tree.scm
;; DESCRIPTION : routines for trees and for modifying documents
;; COPYRIGHT   : (C) 2002  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (kernel library tree)
  (:use (kernel library list)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Extra routines on trees
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (tree . l)
  (if (string? (car l))
      (string->tree (car l))
      (tm->tree l)))

(define-public (atomic-tree? t)
  (and (tree? t) (tree-atomic? t)))

(define-public (compound-tree? t)
  (and (tree? t) (tree-compound? t)))

(define-public (tree->list t)
  (cons (tree-label t) (tree-children t)))

(define-public (tree-explode t)
  (if (atomic-tree? t)
      (tree->string t)
      (cons (tree-label t) (tree-children t))))

(define-public (tree-get-path t)
  (and (tree? t)
       (let ((ip (tree-ip t)))
	 (and (or (null? ip) (!= (cAr ip) -5))
	      (reverse ip)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Navigation inside trees
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (tree-up t . opt)
  "Get the parent of @t."
  (let* ((p   (tree->path t))
	 (nr  (if (null? opt) 1 (car opt)))
	 (len (if (list? p) (length p) -1)))
    (and (>= len nr) (path->tree (list-head p (- len nr))))))

(define-public (tree-down t . opt)
  "Get the child where the cursor is."
  (let* ((p   (tree->path t))
	 (q   (cDr (cursor-path)))
	 (nr  (if (null? opt) 1 (car opt))))
    (and p (list-starts? (cDr q) p)
	 (>= (length q) (+ (length p) nr))
	 (path->tree (list-head q (+ (length p) nr))))))

(define-public (tree-index t)
  "Get the child number of @t in its parent."
  (with p (tree->path t)
    (and (pair? p) (cAr p))))

(define-public (tree-down-index t)
  "Get the number the child where the cursor is."
  (let ((p (tree->path t))
	(q (cDr (cursor-path))))
    (and (list-starts? (cDr q) p)
	 (list-ref q (length p)))))

(define-public (tree-inside? t ref)
  "Is @t inside @ref?"
  (let ((p (tree->path ref))
	(q (tree->path t)))
    (and p q (list-starts? q p))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Special trees
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (cursor-tree)
  (path->tree (cDr (cursor-path))))

(define-public (table-cell-tree row col)
  (path->tree (table-cell-path row col)))

(define the-action-path '(-1))

(define-public (action-set-path p)
  (set! the-action-path p))

(define-public (exec-delayed-at cmd t)
  (let* ((ip (tree-ip t))
	 (old-path the-action-path)
	 (new-path (if (or (null? ip) (>= (car ip) 0)) (reverse ip) '(-1))))
    (action-set-path new-path)
    (exec-delayed (lambda () (cmd) (action-set-path old-path)))))

(define-public (action-path)
  (and (!= the-action-path '(-1)) the-action-path))

(define-public (action-tree)
  (and (!= the-action-path '(-1)) (path->tree the-action-path)))

(define-public-macro (with-action t . body)
  `(and-with ,t (action-tree)
     ,@body))

(define-public (mutator-tree)
  (with p (mutator-path)
    (if (nnull? p) (path->tree p) #f)))

(define-public-macro (with-mutator t . body)
  `(with ,t (mutator-tree)
     (if ,t (begin ,@body))))
