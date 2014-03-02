
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : format-edit.scm
;; DESCRIPTION : routines for formatting text
;; COPYRIGHT   : (C) 2001  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (generic format-edit)
  (:use (utils base environment)
	(utils edit selections)
	(utils library cursor)
	(generic generic-edit)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Simplification
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (with-simplify-sub t var)
  (cond ((tree-is-buffer? t) (noop))
        ((tree-func? t 'document 1)
         (with-simplify-sub (tree-up t) var))
        ((tree-func? t 'with)
         (with-simplify-sub (tree-up t) var)
         (for (i (reverse (.. 0 (quotient (tree-arity t) 2))))
           (when (== (tree-ref t (* 2 i)) var)
             (tree-remove! t (* 2 i) 2)))
         (when (tree-func? t 'with 1)
           (tree-remove-node! t 0)))))

(tm-define (with-simplify t)
  (when (and (not (tree-is-buffer? t)) (tree->path t))
    (with-simplify (tree-up t))
    (when (tree-is? t 'with)
      (for (var (map car (list->assoc (cDr (tree-children t)))))
        (with-simplify-sub (tree-up t) var)))))

(tm-define (with-merge t)
  (when (and (tree-is? t 'with) (tree-is? t :up 'with))
    (let* ((p (tree-up t))
           (c (map tree-copy (cDr (tree-children t)))))
      (tree-remove-node t (- (tree-arity t) 1))
      (tree-insert p (- (tree-arity p) 1) c))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Modifying environment variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (test-env? var val)
  (== (get-env var) val))

(tm-property (make-with var val)
  (:check-mark "o" test-env?))

(tm-define (make-interactive-with var)
  (:interactive #t)
  (interactive (lambda (s) (make-with var s))
    (list (logic-ref env-var-description% var) "string" (get-env var))))

(tm-define (make-interactive-with-opacity)
  (:interactive #t)
  (interactive (lambda (s) (make-with-like `(with-opacity ,s "")))
    (list "opacity" "string" '())))

(define (add-with l t)
  (if (tm-is? t 'with)
      (with l (tm-children t)
        `(with ,@(cDr l) ,(add-with l (cAr l))))
      `(with ,@l ,t)))

(tm-define (make-multi-with l)
  (when (nnull? l)
    (with t (if (selection-active-any?) (selection-tree) "")
      (if (selection-active-any?) (clipboard-cut "null"))
      (insert-go-to (add-with l t) (cons (length l) (path-end t '())))
      (with-simplify (cursor-tree)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Modifying paragraph properties
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (make-line-with var val)
  (:synopsis "Make 'with' with one or more paragraphs as its scope")
  (:check-mark "o" test-env?)
  (if (not (selection-active-normal?))
      (select-line))
  (make-with var val)
  (insert-return)
  (remove-text #f))

(tm-define (make-interactive-line-with var)
  (:interactive #t)
  (interactive (lambda (s) (make-line-with var s))
    (list (logic-ref env-var-description% var) "string" (get-env var))))

(tm-define (make-multi-line-with l)
  (when (nnull? l)
    (when (not (selection-active-normal?))
      (select-line))
    (make-multi-with l)
    (insert-return)
    (remove-text #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Inserting and toggling with-like tags
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (with-like-search t)
  (if (with-like? t) t
      (and (or (tree-atomic? t) (tree-in? t '(concat document)))
	   (and-with p (tree-ref t :up)
	     (with-like-search p)))))

(tm-define (with-like-check-insert t)
  (cond ((with u (cursor-tree)
	   (and (with-like? u) (with-same-type? t u)))
	 (with u (cursor-tree)
	   (tree-go-to u :last (if (== (cAr (cursor-path)) 0) :start :end))
	   #t))
	((with u (cursor-tree*)
	   (and (with-like? u) (with-same-type? t u)))
	 (with u (cursor-tree*)
	   (tree-go-to u :last :start)
	   #t))
	((and-with u (with-like-search (cursor-tree)) (with-same-type? t u))
	 (with sym (symbol->string (tree-label t))
	   (set-message `(concat "Warning: already inside '" ,sym "'")
			`(concat "make '" ,sym "'"))
	   #t))
	(else #f)))

(tm-define (make-with-like w)
  (cond ((func? w 'with 3)
	 (make-with (cadr w) (caddr w)))
	((and (tm-compound? w) (== (tm-arity w) 1))
	 (make (car w)))
	((selection-active-any?)
	 (let* ((selection (selection-tree))
		(ins `(,@(cDr w) ,selection))
		(end (path-end ins '())))
	   (clipboard-cut "nowhere")
	   (insert-go-to ins (cons (- (tm-arity ins) 1) end))))
	(else
	  (insert-go-to w (list (- (tm-arity w) 1) 0)))))

(tm-define (toggle-with-like w back)
  (with t (if (and (selection-active-any?)
		   (== (selection-tree) (path->tree (selection-path))))
	      (path->tree (selection-path))
	      (with-like-search (tree-ref (cursor-tree) :up)))
    ;;(display* "t= " t "\n")
    (cond ((not (and t (with-like? t) (with-same-type? t w)))
           (make-with-like w))
          ((or (not back) (tree-empty? (tm-ref t :last)))
           (tree-remove-node! t (- (tree-arity t) 1))
           (tree-correct-node (tree-ref t :up)))
          ((tree-at-start? (tm-ref t :last))
           (tree-go-to t 0))
          ((tree-at-end? (tm-ref t :last))
           (tree-go-to t 1))
          (else (make-with-like back)))))

(tm-define (toggle-bold)
  (toggle-with-like '(with "font-series" "bold" "")
                    '(with "font-series" "medium" "")))

(tm-define (toggle-italic)
  (toggle-with-like '(with "font-shape" "italic" "")
                    '(with "font-shape" "right" "")))

(tm-define (toggle-small-caps)
  (toggle-with-like '(with "font-shape" "small-caps" "")
                    '(with "font-shape" "right" "")))

(tm-define (toggle-underlined)
  (toggle-with-like '(underline "") #f))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customizable environments
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (customizable-context? t)
  (nnull? (customizable-parameters t)))

(tm-define (customizable-parameters t)
  (list))

(tm-define (tree-with-set t var val)
  (tree-set! t `(with ,var ,val ,t))
  (with-simplify t)
  (with-merge t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Spacing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-property (make-hspace spc)
  (:argument spc "Horizontal space"))

(tm-property (make-space spc)
  (:argument spc "Horizontal space"))

(tm-property (make-var-space spc base top)
  (:argument spc "Horizontal space")
  (:argument base "Base level")
  (:argument top "Top level"))

(tm-property (make-htab spc)
  (:argument spc "Minimal space"))

(tm-property (make-vspace-before spc)
  (:argument spc "Vertical space"))

(tm-property (make-vspace-after)
  (:argument spc "Vertical space"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Page breaking
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (make-page-break)
  (make 'page-break)
  (insert-return))

(tm-define (make-new-page)
  (make 'new-page)
  (insert-return))

(tm-define (make-new-dpage)
  (make 'new-dpage)
  (insert-return))
