
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : std-text-edit.scm
;; DESCRIPTION : editing routines for text mode
;; COPYRIGHT   : (C) 2001  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (texmacs-module (text std-text-edit)
;;   (:use (utils library tree)
;; 	(utils edit variants)
;; 	(text std-text-drd)))

(define-module (text std-text-edit)
  :use-module (texmacs-core))

(use-modules (utils library tree)
             (utils edit variants)
             (text std-text-drd))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Inserting a title and an abstract
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (document-propose-title?)
  (with bt (buffer-tree)
    (with brothers (map tree-label (tree-children bt))
      (and-with t (tree-ref bt :down)
        (and (tree-is? bt 'document)
	     (match? (cursor-tree) "")
	     (not (in? 'doc-data brothers))
             (not (style-has? "beamer-style")))))))

(tm-define (document-propose-abstract?)
  (with bt (buffer-tree)
    (with brothers (map tree-label (tree-children bt))
      (and-with t (tree-ref bt :down)
        (and (tree-is? bt 'document)
	     (match? (cursor-tree) "")
             (in? 'doc-data brothers)
	     (not (in? 'abstract-data brothers)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Inserting document, author and abstract data
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (doc-title-context? t)
  (and (tree-search-upwards t 'doc-data)
       (or (tree-in? t (doc-title-tag-list))
           (and (tree-is? t 'date) (tree-is? t :up 'doc-date)))))

(tm-define (doc-author-context? t)
  (and (tree-search-upwards t 'doc-data)
       (tree-in? t (author-data-tag-list))))

(define doc-data-inactive-tags
  (doc-title-inactive-tag-list))

(tm-define (make-doc-data)
  (insert-go-to '(doc-data (doc-title "")) '(0 0 0)))

(tm-define (make-doc-data-element l)
  (with-innermost t 'doc-data
    (with pos (1+ (tree-down-index t))
      (cond ((== l 'doc-author)
	     (tree-insert! t pos `((,l (author-data (author-name "")))))
	     (tree-go-to t pos 0 0 0 0))
	    ((== l 'doc-note)
	     (tree-insert! t pos `((,l (document ""))))
	     (tree-go-to t pos 0 0 0))
	    ((== l 'doc-title-options)
	     (tree-insert! t pos `((,l))))
	    ((in? l doc-data-inactive-tags)
	     (tree-insert! t pos `((doc-inactive (,l ""))))
	     (tree-go-to t pos 0 0 0))
	    (else
	     (tree-insert! t pos `((,l "")))
	     (tree-go-to t pos 0 0))))))

(tm-define (make-author-data-element l)
  (with-innermost t 'author-data
    (with pos (1+ (tree-down-index t))
      (cond ((in? l '(author-affiliation author-note))
	     (tree-insert! t pos `((,l (document ""))))
	     (tree-go-to t pos 0 0 0))
	    (else
	     (tree-insert! t pos `((,l "")))
	     (tree-go-to t pos 0 0))))))

(tm-define (abstract-data-context? t)
  (tree-in? t (abstract-data-tag-list)))

(tm-define (make-abstract-data)
  (insert-go-to '(abstract-data (abstract "")) '(0 0 0)))

(tm-define (make-abstract-data-element l)
  (with-innermost t 'abstract-data
    (with pos (1+ (tree-down-index t))
      (tree-insert! t pos `((,l "")))
      (tree-go-to t pos 0 0))))

(tm-define (kbd-enter t shift?)
  (:require (tree-is? t 'title))
  (go-end-line)
  (insert-return))

(tm-define (kbd-enter t shift?)
  (:require (tree-is? t 'doc-title))
  (make-doc-data-element 'doc-author))

(tm-define (kbd-enter t shift?)
  (:require (tree-is? t 'author-name))
  (make-author-data-element 'author-affiliation))

(tm-define (kbd-enter t shift?)
  (:require (or
              (tree-is? t 'abstract-arxiv)
              (tree-is? t 'abstract-pacs)
              (tree-is? t 'abstract-acm)
              (tree-is? t 'abstract-msc)
              (tree-is? t 'abstract-keywords)))
  (with t (tree-search-upwards
            t '(abstract-msc abstract-acm abstract-pacs
                             abstract-arxiv abstract-keywords))
    (with pos (1+ (tree-down-index t))
      (tree-insert! t pos `((concat "")))
      (tree-go-to t pos 0 0))))

(tm-define (kbd-enter t shift?)
  (:require (tree-is? t 'doc-inactive))
  (doc-data-activate-here))

(tm-define (set-doc-title-options opts)
  (with-innermost t 'doc-data
    (with opts-trees (select t '(doc-title-options))
      (if (null? opts)
          (when (nnull? opts-trees)
            (with old (car opts-trees)
              (tree-remove (tree-up old) (tree-index old) 1)))
          (begin
            (when (null? opts-trees)
              (make-doc-data-element 'doc-title-options)
              (set! opts-trees (select t '(doc-title-options))))
            (tree-set (car opts-trees) `(doc-title-options ,@opts)))))))

(tm-define (get-doc-title-options)
  (with-innermost t 'doc-data
    (with opts-trees (select t '(doc-title-options :%1))
      (map tree->stree opts-trees))))

(tm-define (test-doc-title-clustering? mode)
  (with cl (list "cluster-all" "cluster-by-affiliation")
    (with old (get-doc-title-options)
      (if mode (in? mode old) (null? (list-intersection cl old))))))

(tm-define (set-doc-title-clustering mode)
  (:check-mark "*" test-doc-title-clustering?)
  (with cl (list "cluster-all" "cluster-by-affiliation")
    (with old (list-difference (get-doc-title-options) cl)
      (set-doc-title-options (if mode (cons mode old) old)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Activation and disactivation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (doc-data-go-to-active t i)
  (cond ((< i 0) (tree-go-to t :end))
	((tree-in? t i (doc-title-inactive-tag-list))
	 (doc-data-go-to-active t (- i 1)))
	((not (cursor-inside? (tree-ref t i)))
	 (tree-go-to t i :end))))

(tm-define (doc-data-activate-here)
  (with-innermost dd 'doc-data
    (with-innermost t 'doc-inactive
      (tree-remove-node! t 0)
      (doc-data-go-to-active dd (tree-down-index dd)))))

(tm-define (doc-data-has-hidden?)
  (with-innermost t 'doc-data
    (with l (cdr (tree->list t))
      (with fun (lambda (t) (or (tree-in? t (doc-title-inactive-tag-list))
				(tree-is? t 'doc-inactive)))
	(list-or (map fun l))))))

(tm-define (doc-data-disactivated?)
  (with-innermost t 'doc-data
    (with l (cdr (tree->list t))
      (list-or (map (lambda (t) (== (tm-car t) 'doc-inactive)) l)))))

(define (doc-data-activate-one t)
  (when (tree-is? t 'doc-inactive)
    (tree-remove-node! t 0)))

(tm-define (doc-data-activate-all)
  (with-innermost t 'doc-data
    (with i (tree-down-index t)
      (with l (cdr (tree->list t))
	(for-each doc-data-activate-one l))
      (doc-data-go-to-active t i))))

(define (doc-data-disactivate-one t)
  (if (in? (tm-car t) doc-data-inactive-tags)
      (tree-insert-node! t 0 '(doc-inactive))))

(tm-define (doc-data-disactivate-all)
  (with-innermost t 'doc-data
    (with l (cdr (tree->list t))
      (for-each doc-data-disactivate-one l))))

(tm-define (doc-data-activate-toggle)
  (if (doc-data-disactivated?)
      (doc-data-activate-all)
      (doc-data-disactivate-all)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Making letter headings or titles
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (go-end-of-header-element)
  (if (inside? 'address) (go-end-of 'address))
  (if (inside? 'destination) (go-end-of 'destination))
  (if (inside? 'cc) (go-end-of 'cc))
  (if (inside? 'encl) (go-end-of 'encl))
  (go-end-line))

(tm-define (make-header l)
  (go-end-of-header-element)
  (if (!= (tree->stree (paragraph-tree)) "") (insert-return))
  (make l))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sectional commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (section-context? t)
  (tree-in? t (numbered-unnumbered-append (section-tag-list))))

(tm-define (previous-section)
  (with bt (buffer-tree)
    (and (cursor-inside? bt)
	 (with bp (list-drop (cursor-path) (length (tree->path bt)))
	   (with sp (path-previous-section bt bp)
	     (and (!= sp bp) (path->tree (append (tree->path bt) sp))))))))

(tm-define (make-section l)
  (if (or (selection-active-any?) (not (make-return-after)))
      (make l)))

(tm-define (make-unnamed-section l)
  (if (or (selection-active-any?) (not (make-return-after)))
      (make l)
      (make-return-before)))

(tm-define (kbd-enter t shift?)
  (:require (section-context? t))
  (tree-go-to t :end)
  (insert-return))

(tm-define (label-insert t)
  (:require (section-context? t))
  (tree-go-to t :end)
  (make 'label))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Routines for lists, enumerations and description
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (list-context? t)
  (tree-in? t (list-tag-list)))

(tm-define (itemize-context? t)
  (tree-in? t (itemize-tag-list)))

(tm-define (enumerate-context? t)
  (tree-in? t (enumerate-tag-list)))

(tm-define (itemize-enumerate-context? t)
  (or (tree-in? t (itemize-tag-list))
      (tree-in? t (enumerate-tag-list))))

(tm-define (make-tmlist l)
  (make l)
  (make-item))

(tm-define (make-item)
  (if (not (make-return-after))
      (with lab (inside-which (list-tag-list))
	(cond ((in? lab (itemize-tag-list)) (make 'item))
	      ((in? lab (enumerate-tag-list)) (make 'item))
	      ((in? lab (description-tag-list)) (make 'item*))))))

(tm-define (kbd-enter t shift?)
  (:require (list-context? t))
  (if shift? (make-return-after) (make-item)))

(tm-define (kbd-enter t shift?)
  (:require (tree-is? t 'item*))
  (go-end-of 'item*))

(tm-define (numbered-context? t)
  (:require (or (itemize-context? t) (enumerate-context? t)))
  #t)

(tm-define (numbered-numbered? t)
  (:require (enumerate-context? t))
  #t)

(tm-define (numbered-toggle t)
  (:require (itemize-context? t))
  (variant-set t 'enumerate))

(tm-define (numbered-toggle t)
  (:require (enumerate-context? t))
  (variant-set t 'itemize))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Inserting formulas
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (make-equation)
  (make 'equation)
  (temp-proof-fix))

(tm-define (make-equation*)
  (make 'equation*)
  (temp-proof-fix))

(tm-define (make-eqnarray*)
  (make 'eqnarray*)
  (temp-proof-fix))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Routines for inserting miscellaneous content
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (make-aux env aux)
  (if (not (make-return-after))
      (insert (list (string->symbol env) aux '(document "")))))

(tm-define (make-aux* env aux name)
  (if (not (make-return-after))
      (insert (list (string->symbol env) aux name '(document "")))))

(tm-define (make-bib style file-name)
  (:argument style "Bibliography style")
  (:proposals style '("tm-plain" "tm-alpha" "tm-acm" "tm-ieeetr" "tm-siam"))
  (:argument file-name "Bibliography file")
  (if (not (make-return-after))
      (insert (list 'bibliography "bib" style file-name '(document "")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Editing algorithms
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (algorithm-context? t)
  (tree-in? t (algorithm-tag-list)))

(tm-define (algorithm-root s)
  (cond ((symbol-ends? s '*)
	 (algorithm-root (symbol-drop-right s 1)))
	((symbol-starts? s 'specified-)
	 (algorithm-root (symbol-drop s 10)))
	((symbol-starts? s 'named-)
	 (algorithm-root (symbol-drop s 6)))
	(else s)))

(tm-define (algorithm-numbered? t)
  (let* ((l (tree-label t))
	 (r (algorithm-root l)))
    (in? l (list r (symbol-append 'specified- r)))))

(tm-define (algorithm-named? t)
  (with l (tree-label t)
    (symbol-starts? l 'named-)))

(tm-define (algorithm-specified? t)
  (with l (tree-label t)
    (or (symbol-starts? l 'named-specified-)
	(symbol-starts? l 'specified-))))

(tm-define (algorithm-toggle-number t)
  (let* ((l (tree-label t))
	 (r (algorithm-root l)))
    (if (algorithm-numbered? t)
	(if (algorithm-specified? t)
	    (variant-set t (symbol-append 'specified- r '*))
	    (variant-set t (symbol-append r '*)))
	(if (algorithm-specified? t)
	    (variant-set t (symbol-append 'specified- r))
	    (variant-set t r)))))

(tm-define (algorithm-toggle-name t)
  (let* ((l (tree-label t))
	 (r (algorithm-root l)))
    (if (algorithm-named? t)
	(begin
	  (if (algorithm-specified? t)
	      (tree-assign-node! t (symbol-append 'specified- r))
	      (tree-assign-node! t r))
	  (tree-remove! t 0 1))
	(begin
	  (if (algorithm-specified? t)
	      (tree-assign-node! t (symbol-append 'named-specified- r))
	      (tree-assign-node! t (symbol-append 'named- r)))
	  (tree-insert! t 0 '(""))
	  (tree-go-to t 0 :start)))))

(tm-define (algorithm-toggle-specification t)
  (let* ((l (tree-label t))
	 (r (algorithm-root l)))
    (if (algorithm-specified? t)
	(begin
	  (cond ((algorithm-named? t)
		 (tree-assign-node! t (symbol-append 'named- r)))
		((algorithm-numbered? t)
		 (tree-assign-node! t r))
		(else
		 (tree-assign-node! t (symbol-append r '*))))
	  (tree-remove! t (- (tree-arity t) 2) 1))
	(begin
	  (cond ((algorithm-named? t)
		 (tree-assign-node! t (symbol-append 'named-specified- r)))
		((algorithm-numbered? t)
		 (tree-assign-node! t (symbol-append 'specified- r)))
		(else
		 (tree-assign-node! t (symbol-append 'specified- r '*))))
	  (tree-insert! t (- (tree-arity t) 1) '((document "")))
	  (tree-go-to t (- (tree-arity t) 2) :start)))))
