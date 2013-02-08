
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : r-edit.scm
;; DESCRIPTION : editing r programs
;; COPYRIGHT   : (C) 2008  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (r-edit)
  (:use (prog prog-edit)
	(utils misc tm-keywords)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Automatic indentation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define r-indenters
  '("=" "==" "!=" "<-" "<<-" "->" "->>" "if" "else" "repeat" "for"))

(define (standard-indent? s)
  (with indent?
      (lambda (x) (or (== s x) (string-ends? s (string-append " " x))))
    (not (not (list-find r-indenters indent?)))))

(define (reference-row-bis row)
  (with s (program-row (- row 1))
    (cond ((not s) row)
	  ((standard-indent? s) (reference-row-bis (- row 1)))
	  (else row))))

(define (reference-row row)
  (let* ((r1 (program-previous-match row #\{ #\}))
	 (r2 (program-previous-match row #\( #\)))
	 (r3 (program-previous-match row #\[ #\]))
	 (rr (min r1 r2 r3)))
    (reference-row-bis rr)))

(define (compute-indentation-bis row)
  (let* ((prev (max 0 (- row 1)))
	 (s (program-row prev))
	 (i (string-get-indent s))
	 (last (- (string-length s) 1))
	 (curly  (string-bracket-backward s last #\{ #\}))
	 (round  (string-bracket-backward s last #\( #\)))
	 (square (string-bracket-backward s last #\[ #\])))
    (if (and curly round  (< round  curly)) (set! curly #f))
    (if (and curly square (< square curly)) (set! curly #f))
    (if (and round square (< square round)) (set! round #f))
    (cond ((== row 0) i)
	  (curly (+ i 2))
	  (round (+ round 1))
	  (square (+ square 1))
	  ((standard-indent? s) (+ i 2))
	  (else
	    ;;(display* "row= " prev "\n")
	    ;;(display* "ref= " (reference-row prev) "\n")
	    (with ref (reference-row prev)
	      (string-get-indent (program-row ref)))))))

(define (compute-indentation row)
  (let* ((s (program-row row))
	 (i (string-get-indent s)))
    (if (and (< i (string-length s)) (== (string-ref s i) #\}))
	(max 0 (- (compute-indentation-bis row) 2))
	(compute-indentation-bis row))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User interface for automatic indentation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (r-indent)
  (:synopsis "indent current line of a r program")
  (and-with doc (program-tree)
    (with i (compute-indentation (program-row-number))
      (program-set-indent i)
      (program-go-to (program-row-number) i))))

(tm-define (kbd-variant t forward?)
  (:require (and (in-prog-r?) (not (inside? 'session))))
  (r-indent))

(tm-define (insert-return)
  (:mode in-prog-r?)
  (insert-raw-return)
;;	(insert (stree->tree `(new-line)))
  (r-indent))
