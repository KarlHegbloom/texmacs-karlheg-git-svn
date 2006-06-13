
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : fold-menu.scm
;; DESCRIPTION : menus for folding
;; COPYRIGHT   : (C) 1999  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (dynamic fold-menu)
  (:use (dynamic fold-edit)))

(menu-bind fold-menu
  (when (with t (tree-innermost dynamic-context?)
	  (and t (toggle-second-context? t)))
    ("Fold" (dynamic-previous)))
  (when (with t (tree-innermost dynamic-context?)
	  (and t (toggle-first-context? t)))
    ("Unfold" (dynamic-next))))

(menu-bind switch-menu
  (when (with t (tree-innermost dynamic-context?)
	  (and t (switch-context? t)))
    ("Add branch before" (switch-insert-at :current))
    ("Add branch after" (switch-insert-at :var-next))
    ("Remove this branch" (switch-remove-at :current))
    ---
    (when (< 0 (switch-index))
      ("Switch to first" (dynamic-first)))
    (when (< 0 (switch-index))
      ("Switch to previous" (dynamic-previous)))
    (when (< (switch-index) (switch-index :last))
      ("Switch to next" (dynamic-next)))
    (when (< (switch-index) (switch-index :last))
      ("Switch to last" (dynamic-last)))))

(define (fold/unfold-menu-entry x which action)
  (with sym (string->symbol x)
    (list 'when (lambda () (ahash-ref which sym))
	  (list (upcase-first x)
		(lambda () (dynamic-operate-on-buffer (list action sym)))))))

(tm-define (fold-environments-menu)
  (receive (l first second) (fold-get-environments-in-buffer)
    (if (null? l) (menu-dynamic ())
	(menu-dynamic
	  ---
	  ,@(map (lambda (x) (fold/unfold-menu-entry x second :fold)) l)))))

(tm-define (unfold-environments-menu)
  (receive (l first second) (fold-get-environments-in-buffer)
    (if (null? l) (menu-dynamic ())
	(menu-dynamic
	  ---
	  ,@(map (lambda (x) (fold/unfold-menu-entry x first :unfold)) l)))))

(menu-bind insert-fold-menu
  ("First" (dynamic-operate-on-buffer :first))
  ("Previous" (dynamic-traverse-buffer :previous))
  ("Next" (dynamic-traverse-buffer :next))
  ("Last" (dynamic-operate-on-buffer :last))
  ---
  (-> "Folded"
      ("Default" (make-toggle 'folded))
      ---
      ("Plain" (make-toggle 'folded-plain))
      ("Standard" (make-toggle 'folded-std))
      ("Environment" (make-toggle 'folded-env))
      ("Grouped" (make-toggle 'folded-grouped))
      ---
      (link fold-menu))
  (-> "Summarized"
      ("Default" (make-toggle 'summarized))
      ---
      ("Plain" (make-toggle 'summarized-plain))
      ("Standard" (make-toggle 'summarized-std))
      ("Environment" (make-toggle 'summarized-env))
      ("Grouped" (make-toggle 'summarized-grouped))
      ---
      (link fold-menu))
  (-> "Switch"
      ("Standard" (make-switch 'switch))
      ("Screens" (make-switch 'screens))
      ("Tiny" (make-switch 'tiny-switch))
      ---
      (link switch-menu))
  (-> "Unroll"
      ("Standard" (make-switch 'unroll))
      ---
      (link switch-menu))
  (-> "Expanded"
      ("Standard" (make-switch 'expanded))
      ("Slides" (make-switch 'slides))
      ---
      (link switch-menu))
  (-> "Traversal"
      ("Fold back" (make 'fold-back))
      ("Keep unfolded" (make 'keep-unfolded))
      (when #f
	("Animate folding" (noop))
	("Animate unfolding" (noop))))
  ---
  (-> "Fold"
      ("All" (dynamic-operate-on-buffer :fold))
      (link fold-environments-menu))
  (-> "Unfold"
      ("All" (dynamic-operate-on-buffer :unfold))
      (link unfold-environments-menu))
  (-> "Compress"
      ("Preserve tags" (dynamic-operate-on-buffer :compress))
      ("Change tags" (dynamic-operate-on-buffer :var-compress)))
  (-> "Expand"
      ("Preserve tags" (dynamic-operate-on-buffer :expand))
      ("Change tags" (dynamic-operate-on-buffer :var-expand))))
