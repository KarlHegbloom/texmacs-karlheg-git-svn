
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; arch-tag: 38b946f6-8203-490c-ba78-c58b6103fd91
;;
;; MODULE      : slides browse
;; DESCRIPTION : Navigate nested switches
;; COPYRIGHT   : (C) 2003  David Allouche
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (slides browse)
  (:use (texmacs edit edit-fold) (slides search-in-tree))
  (:export next-slide prev-slide))

(define (next-slide)
  (go-innermost-switch)
  (if (inside? "switch")
      (next-slide-sub)))

(define (prev-slide)
  (go-innermost-switch)
  (if (inside? "switch")
      (prev-slide-sub)))

(define (next-slide-sub)
  (if (< (switch-get-position) (switch-get-last))
      (begin
	(switch-to "next")
	(recursive-switch-to-sub "first"))
      (let ((oldp (the-path)))
	(go-outer-switch)
	(if (not (== (the-path) oldp))
	    (next-slide-sub)))))

(define (prev-slide-sub)
  (if (< 0 (switch-get-position))
      (begin
	(switch-to "previous")
	(recursive-switch-to-sub "last"))
      (let ((oldp (the-path)))
	(go-outer-switch)
	(if (not (== (the-path) oldp))
	    (prev-slide-sub)))))

(define (recursive-switch-to-sub where)
  (let ((oldp (the-path)))
    (go-inner-switch)
    (if (not (== (the-path) oldp))
	(begin
	  (switch-to where)
	  (recursive-switch-to-sub where)))))

;; Moving in the switch hierarchy

(define (go-innermost-switch)
  (let ((p (search-innermost-switch)))
    (if p
	(tm-go-to
	 (if (== (tree-get-label (subtree (the-buffer) p)) "document")
	     (append p '(0 0))
	     (rcons p 0))))))

(define (search-innermost-switch)
  (let rec ((t (the-buffer)) (p '()))
    (define (proc p2 t2)
      (let ((p3 (rec (safe-tree-ref t2 1) '())))
	(if p3 (append p2 '(1) p3) (rcons p2 1))))
    (search-in-tree-from t p "switch" proc)))

(define (go-outermost-switch)
  (let ((oldp (the-path)))
    (go-outer-switch)
    (if (not (== oldp (the-path)))
	(go-outermost-switch))))

(define (go-this-switch)
  (let ((p (search-upwards "switch")))
    (if (pair? p)
	(tm-go-to (append p '(1 0 0)))
	(tm-go-to '(0 0)))))

(define (go-outer-switch)
  (let ((p (search-upwards "switch")))
    (if (pair? p)
	(tm-go-to (append p '(0)))))
  (go-this-switch))

(define (go-inner-switch)
  (define (proc p t) p)
  (let ((old-p (the-path)))
    (go-this-switch)
    (let ((p1 (search-upwards "switch")))
      (let ((p2 (search-in-tree-from
		 (if (null? p1)
		     (the-buffer)
		     (subtree (the-buffer) (rcons p1 1)))
		 '() "switch" proc)))
	(if p2
	    (if (null? p1)
		(tm-go-to (append p2 '(1 0 0)))
		(tm-go-to (append p1 '(1) p2 '(1 0 0))))
	    (tm-go-to old-p))))))
