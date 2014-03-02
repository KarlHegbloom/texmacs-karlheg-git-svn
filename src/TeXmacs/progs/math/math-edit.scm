
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : math-edit.scm
;; DESCRIPTION : editing mathematics
;; COPYRIGHT   : (C) 1999  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (math math-edit)
  (:use (utils library tree)
	(utils library cursor)
	(utils edit auto-close)
	(math math-drd)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Some drd properties, which should go into table-drd.scm later on
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-group variant-tag (math-table-tag))
(define-group similar-tag (math-table-tag))

(define-group math-table-tag
  matrix det)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Invisible operators
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (insert-invisible s)
  (:synopsis "insert an invisible mathematical symbol")
  (cond ((in? (before-cursor) '(" " "*")) (noop))
	((in? (after-cursor) '(" " "*")) (noop))
	(else (insert s))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Special customizations inside equation environments
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (kbd-enter t shift?)
  (:require (tree-is? t 'equation))
  (go-end-of 'equation)
  (insert-return))

(tm-define (kbd-enter t shift?)
  (:require (tree-is? t 'equation*))
  (go-end-of 'equation*)
  (insert-return))

(tm-define (label-insert t)
  (:require (tree-is? t 'eqnarray*))
  (go-end-line)
  (make 'label))

(tm-define (math-make-math)
  (if (inside? 'math)
      (go-end-of 'math)
      (set-message (string-append "Warning: already inside mathematics")
		   (string-append "make 'math'"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutines for moving punctuation symbols around
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (string-ref-nspace? s i)
  (!= (string-ref s i) #\space))

(define (string-ref-npunct? s i)
  (nin? (string-ref s i) '(#\space #\. #\, #\: #\;)))

(define (string-search-forwards s i n pred?)
  (cond ((>= i n) i)
	((pred? s i) i)
	(else (string-search-forwards s (+ i 1) n pred?))))

(define (string-search-backwards s i b pred?)
  (cond ((<= i b) i)
	((pred? s (- i 1)) i)
	(else (string-search-backwards s (- i 1) b pred?))))

(define (atomic-cut-left-until t pred?)
  (if (atomic-tree? t)
      (let* ((s (tree->string t))
	     (n (string-length s))
	     (i (string-search-forwards s 0 n pred?)))
	(if (> i 0)
	    (with ss (substring s 0 i)
	      (tree-remove! t 0 i)
	      (tree-correct-old t)
	      ss)
	    ""))
      ""))

(define (atomic-cut-right-until t pred?)
  (if (atomic-tree? t)
      (let* ((s (tree->string t))
	     (n (string-length s))
	     (i (string-search-backwards s n 0 pred?)))
	(if (< i n)
	    (with ss (substring s i n)
	      (tree-remove! t i (- n i))
	      (tree-correct-old t)
	      ss)
	    ""))
      ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Switching between different types of formulas
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-macro (concat-isolate! t)
  `(cond ((not (tree-is? t :up 'concat)) (noop))
	 ((not (tree-is? t :up :up 'document)) (noop))
	 ((= (tree-arity (tree-up t)) 1) (tree-set! t :up t))
	 ((< (tree-index t) (- (tree-arity (tree-up t)) 1))
	  (tree-split (tree-up t 2)
		      (tree-index (tree-up t))
		      (+ (tree-index t) 1))
	  (concat-isolate! t))
	 (else
	  (tree-split (tree-up t 2)
		      (tree-index (tree-up t))
		      (tree-index t))
	  (concat-isolate! t))))

(define (math->equation* t)
  (let* ((c (and (tree-is? t :up 'concat) (tree-is? t :up :up 'document)))
         (r (and c (atomic-cut-left-until (tree-ref t :next)
                                          string-ref-npunct?)))
         (l (and c (atomic-cut-right-until (tree-ref t :previous)
                                           string-ref-nspace?))))
    (concat-isolate! t)
    (if (tree-is? t :up 'document)
        (begin
          (if (not r) (set! r ""))
          (if (not l) (set! l ""))
          (tree-set! t `(equation* (document ,(tree-ref t 0))))
          (while (string-ends? r " ")
            (set! r (string-drop-right r 1)))
          (with-cursor (tree->path t 0 0 :end)
            (insert r))))))

(define (equation*->math t)
  (if (or (not (tree-is? t 0 'document))
          (= (tree-arity (tree-ref t 0)) 1))
      (begin
        (if (tree-is? t 0 'document)
            (tree-set! t 0 (tree-ref t 0 0)))
        (tree-set! t `(math ,(tree-ref t 0)))
        (with r (atomic-cut-right-until (tree-end (tree-ref t 0))
                                        string-ref-npunct?)
          (with-cursor (tree->path t :start)
            (kbd-backspace)
            (if (and (!= (cursor-path) (cursor-after (go-start-paragraph)))
                     (!= (cursor-path) (cursor-after (go-end-paragraph))))
                (insert " ")))
          (with-cursor (tree->path t :end)
            (insert r)
            (kbd-delete)
            (if (and (!= (cursor-path) (cursor-after (go-start-paragraph)))
                     (!= (cursor-path) (cursor-after (go-end-paragraph))))
                (insert " ")))))))

(define (with-math-context? t)
  (match? t '(with "mode" "math" :%1)))

(tm-define (variant-circulate t forward?)
  (:require (with-math-context? t))
  (tree-set! t `(math ,(tree-ref t 2)))
  (math->equation* t))

(tm-define (variant-circulate t forward?)
  (:require (tree-is? t 'math))
  (math->equation* t))

(tm-define (variant-circulate t forward?)
  (:require (tree-in? t '(equation equation*)))
  (equation*->math t))

(tm-define (variant-formula t)
  (with-innermost t '(math equation equation*)
    (when (tree-in? t '(equation equation*))
      (equation*->math t))))

(tm-define (variant-equation t)
  (with-innermost t '(math equation equation*)
    (when (tree-in? t '(math))
      (math->equation* t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Structured editing of roots
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (sqrt-toggle t)
  (when (tree-is? t 'sqrt)
    (cond ((== (tree-arity t) 1)
           (tree-insert! t 1 '(""))
           (tree-go-to t 1 0))
          ((== (tree-arity t) 2)
           (tree-remove! t 1 1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Structured editing of scripts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (script-context? t)
  (tree-in? t '(lsub lsup rsub rsup)))

(tm-define (script-only-script? t)
  (and (tree-is? (tree-up t) 'concat)
       (not (or (and (tree-ref t :previous)
                     (script-context? (tree-ref t :previous)))
                (and (tree-ref t :next)
                     (script-context? (tree-ref t :next)))))))

(tm-define (variant-circulate t forward?)
  (:require (script-context? t))
  (when (script-only-script? t)
    (cond ((tree-is? t 'lsub) (variant-set t 'lsup))
          ((tree-is? t 'lsup) (variant-set t 'lsub))
          ((tree-is? t 'rsub) (variant-set t 'rsup))
          ((tree-is? t 'rsup) (variant-set t 'rsub)))))

(tm-define (structured-insert-vertical t downwards?)
  (:require (tree-in? t '(lsub rsub)))
  (when (and (not downwards?) (script-only-script? t))
    (tree-go-to t :end)
    (cond ((tree-is? t 'lsub) (make 'lsup))
          ((tree-is? t 'rsub) (make 'rsup)))))

(tm-define (structured-insert-vertical t downwards?)
  (:require (tree-in? t '(lsup rsup)))
  (when (and downwards? (script-only-script? t))
    (tree-go-to t :end)
    (cond ((tree-is? t 'lsup) (make 'lsub))
          ((tree-is? t 'rsup) (make 'rsub)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Structured editing of wide accents
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define wide-list-1
  '("~" "^" "<bar>" "<vect>" "<check>" "<breve>" "<invbreve>"))

(define wide-list-2
  '("<acute>" "<grave>" "<dot>" "<ddot>" "<abovering>"))

(define wide-list-3
  '("<wide-overbrace>" "<wide-underbrace*>"
    "<wide-sqoverbrace>" "<wide-squnderbrace*>"
    "<wide-varrightarrow>" "<wide-varleftarrow>" "<wide-bar>"))

(tm-define (variant-circulate t forward?)
  (:require (tree-in? t '(wide wide*)))
  (when (tree-atomic? (tree-ref t 1))
    (with s (tree->string (tree-ref t 1))
      (and-with i (list-find-index wide-list-1 (lambda (x) (== x s)))
        (with j (modulo (+ i (if forward? 1 -1)) (length wide-list-1))
          (tree-set t 1 (list-ref wide-list-1 j))))
      (and-with i (list-find-index wide-list-2 (lambda (x) (== x s)))
        (with j (modulo (+ i (if forward? 1 -1)) (length wide-list-2))
          (tree-set t 1 (list-ref wide-list-2 j))))
      (and-with i (list-find-index wide-list-3 (lambda (x) (== x s)))
        (with j (modulo (+ i (if forward? 1 -1)) (length wide-list-3))
          (tree-set t 1 (list-ref wide-list-3 j)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Wide arrows
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (make-long-arrow s)
  (when (and (string? s) (string-starts? s "<") (string-ends? s ">"))
    (with rs (string-append "<rubber-" (substring s 1 (string-length s)))
      (insert-go-to `(long-arrow ,rs "") '(1 0)))))

(tm-define (make-long-arrow* s)
  (when (and (string? s) (string-starts? s "<") (string-ends? s ">"))
    (with rs (string-append "<rubber-" (substring s 1 (string-length s)))
      (insert-go-to `(long-arrow ,rs "" "") '(2 0)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Modifying the shape of brackets
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define brackets
  '(("(" ")")
    ("[" "]")
    ("{" "}")
    ("<langle>" "<rangle>")
    ("|" "|")
    ("<||>" "<||>")
    ("<lfloor>" "<rfloor>")
    ("<lceil>" "<rceil>")
    ("<llbracket>" "<rrbracket>")))

(tm-define (variant-circulate t forward?)
  (:require (tree-in? t '(around around*)))
  (when (and (== (tree-arity t) 3)
             (tree-atomic? (tree-ref t 0))
             (tree-atomic? (tree-ref t 2)))
    (let* ((l (tree->string (tree-ref t 0)))
           (r (tree->string (tree-ref t 2)))
           (p (list l r)))
      (when (in? p brackets)
        (let* ((i (list-find-index brackets (lambda (x) (== x p))))
               (j (modulo (+ i (if forward? 1 -1)) (length brackets)))
               (nl (car (list-ref brackets j)))
               (nr (cadr (list-ref brackets j))))
          (tree-assign (tree-ref t 0) nl)
          (tree-assign (tree-ref t 2) nr))))))

(define bigops
  '("<int>" "<intlim>" "<oint>" "<ointlim>"
    "<sum>" "<prod>" "<amalg>"
    "<cap>" "<cup>" "<sqcap>" "<sqcup>"
    "<vee>" "<wedge>" "<curlyvee>" "<curlywedge>"
    "<odot>" "<otimes>" "<oplus>"
    "<triangleup>" "<triangledown>"
    "<box>" "<parallel>" "<interleave>"))

(tm-define (variant-circulate t forward?)
  (:require (tree-is? t 'big-around))
  (when (and (== (tree-arity t) 2)
             (tree-atomic? (tree-ref t 0)))
    (with s (tree->string (tree-ref t 0))
      (when (in? s bigops)
        (let* ((i (list-find-index bigops (lambda (x) (== x s))))
               (j (modulo (+ i (if forward? 1 -1)) (length bigops)))
               (ns (list-ref bigops j)))
          (tree-assign (tree-ref t 0) ns))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Not necessarily matching brackets
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (find-non-bracket t)
  (cond ((not (tree-up t)) t)
        ((tm-in? t '(around around* big-around temp-slot))
         (find-non-bracket (tree-up t)))
        ((tm-in? (tree-up t) '(concat around around* big-around temp-slot))
         (find-non-bracket (tree-up t)))
        (else t)))

(define (find-and-remove-temp-slot x)
  ;;(display* "x= " x "\n")
  (cond ((tree-atomic? x) #f)
        ((tree-is? x 'temp-slot)
         (tree-go-to x 0 0)
         (remove-structure-upwards))
        ((tree? x)
         (list-or (map find-and-remove-temp-slot (tree-children x))))
        (else #f)))

(tm-define (brackets-refresh)
  (insert-go-to '(temp-slot "") '(0 0))
  (let* ((t (find-non-bracket (cursor-tree)))
	 (u (tree-downgrade-brackets t #t #f))
	 (v (tree-upgrade-brackets u "math"))
         (w (tree-downgrade-big v)))
    ;;(tree-set! t v)
    (tree-set! t w)
    (find-and-remove-temp-slot t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Match bracket with missing bracket
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (count-missing t open?)
  (cond ((tree-atomic? t) 0)
	((tree-in? t '(around around*))
	 (+ (count-missing (tree-ref t 1) open?)
	    (if (deleted? t (if open? 0 2)) 1 0)))
	((tree-is? t (if open? 'right 'left)) 1)
	((tree-is? t 'concat)
	 (with l (tree-children t)
	   (apply + (map (lambda (x) (count-missing x open?)) l))))
	(else 0)))

(define (try-matching-insert open? which large?)
  (try-modification
    (let* ((nr (count-missing (find-non-bracket (cursor-tree)) open?))
	   (tag (if large? 'around* 'around)))
      ;;(display* nr ", " (find-non-bracket (cursor-tree)) "\n")
      (if open?
	  (insert-go-to (list tag which "" "<nobracket>") '(1 0))
	  (insert-go-to (list tag "<nobracket>" "" which) '(1)))
      (brackets-refresh)
      ;;(display* (count-missing (find-non-bracket (cursor-tree)) open?) ", "
      ;;(find-non-bracket (cursor-tree)) "\n")
      (> nr (count-missing (find-non-bracket (cursor-tree)) open?)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Matching brackets
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (deleted? t i)
  (== (tm->stree (tree-ref t i)) "<nobracket>"))

(define (make-small s)
  (cond ((nstring? s) "<nobracket>")
	((== s ".") "<nobracket>")
	((<= (string-length s) 1) s)
	(else (string-append "<" s ">"))))

(define (find-adjacent-around del?)
  (let* ((ret #f)
	 (p (cursor-path))
	 (p* (cursor-path*)))
    (with t (tree-innermost '(around around*))
      (when t
	(when (== p (tree->path t 1 :start))
	  (when (deleted? t 0)
	    (set! ret t)))
	(when (== p (tree->path t 1 :end))
	  (when (or (not del?) (deleted? t 2))
	    (set! ret t)))))
    (when (not ret)
      (with t (path->tree (cDr p))
	(when (tree-in? t '(around around*))
	  (when (== (cAr p) 0)
	    (when (deleted? t 0)
	      (set! ret t)))
	  (when (== (cAr p) 1)
	    (when (or (not del?) (deleted? t 2))
	      (set! ret t))))))
    (when (and (not ret) (!= p p*))
      (with t (path->tree (cDr p*))
	(when (tree-in? t '(around around*))
	  (when (== (cAr p*) 0)
	    (when (deleted? t 0)
	      (set! ret t))))))
    ret))

(tm-define (math-bracket-open lb rb large?)
  (when (== large? 'default)
    (set! large? (!= (get-preference "use large brackets") "off")))
  (when (== (get-preference "automatic brackets") "off")
    (make-bracket-open lb rb large?)
    (brackets-refresh))
  (when (!= (get-preference "automatic brackets") "off")
    (if (selection-active-normal?)
	(begin
	  (clipboard-cut "temp")
          (insert-go-to `(,(if large? 'around* 'around) ,lb "" ,rb) '(1 0))
	  (clipboard-paste "temp"))
        (let* ((t (find-adjacent-around #t))
               (u (find-adjacent-around #f)))
          (cond ((and t (deleted? t 2)
                      (or (not (deleted? t 0))
                          (tree-at-end? t)))
                 (tree-set t 2 lb)
                 (tree-go-to t :end))
                ((and t (deleted? t 0))
                 (tree-set t 0 lb)
                 (tree-go-to t 1 :start))
                ((and u (== lb rb)
                      (== (tree->stree (tree-ref u 2)) rb))
                 (tree-go-to u :end))
                ((and u (== rb "|")
                      (== (tree->stree (tree-ref u 0)) "<langle>"))
                 (tree-set u 2 rb)
                 (tree-go-to u :end))
                ((try-matching-insert #t lb large?)
                 (noop))
                (else
                  (insert-go-to `(,(if large? 'around* 'around) ,lb "" ,rb)
                                '(1 0))))))))

(tm-define (math-separator sep large?)
  (when (== large? 'default)
    (set! large? (!= (get-preference "use large brackets") "off")))
  (when (and (string? sep) (string-starts? sep "<") (string-ends? sep ">"))
    (set! sep (substring sep 1 (- (string-length sep) 1))))
  (when (== (get-preference "automatic brackets") "off")
    (make-separator sep large?)
    (brackets-refresh))
  (when (!= (get-preference "automatic brackets") "off")
    (make-separator sep large?)))

(tm-define (math-bracket-close rb lb large?)
  (when (== large? 'default)
    (set! large? (!= (get-preference "use large brackets") "off")))
  (when (== (get-preference "automatic brackets") "off")
    (make-bracket-close rb lb large?)
    (brackets-refresh))
  (when (!= (get-preference "automatic brackets") "off")
    (let* ((t (find-adjacent-around #t))
	   (u (find-adjacent-around #f)))
      (cond ((and t (deleted? t 0)
                  (or (not (deleted? t 2))
                      (not (tree-at-end? t))))
	     (tree-set t 0 rb)
	     (tree-go-to t 1 :start))
	    ((and t (deleted? t 2))
	     (tree-set t 2 rb)
	     (tree-go-to t :end))
	    (u
	     (tree-set u 2 rb)
	     (tree-go-to u :end))
	    ((try-matching-insert #f rb large?)
	     (noop))
	    (else
	      (set-message "Error: bracket does not match"
			   (force-string rb)))))))

(tm-define (math-big-operator op)
  ;;(when (== (get-preference "automatic brackets") "off")
  ;;  (make-big-operator op)
  ;;  (brackets-refresh))
  ;;(when (!= (get-preference "automatic brackets") "off")
  ;;  (insert-go-to `(big-around ,(make-small op) "") '(1 0)))
  (insert `(big ,op)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Correction of mathematical formulas
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (manual-correct-math t)
  (with mode (tree->stree (get-env-tree-at "mode" (tree->path t :start)))
    (if (!= mode "math")
	(manual-correct t)
	(with r (manual-correct `(math ,t))
	  (if (tm-func? r 'math 1) (tm-ref r 0) r)))))

(define (math-correct-tree t)
  (with r (manual-correct-math t)
    (when (!= r t)
      (tree-set! t r))))

(define (math-manually-correct-tree t)
  (with r (manual-correct-math t)
    (when (!= r t)
      (import-from (version version-compare))
      (let* ((t1 (tree->stree t))
	     (t2 (tree->stree r))
	     (tb (compare-versions t1 t2))
	     (rt (stree->tree tb)))
	(tree-set! t rt)
	(tree-go-to t :start)
	(version-next-difference)))))

(tm-define (math-correct-all)
  (:synopsis "Correct selected formula or all formulas in document")
  (if (selection-active-any?)
      (if (== (selection-tree) (path->tree (selection-path)))
	  (math-correct-tree (path->tree (selection-path)))
	  (set-message "Only implemented for complete subtrees"
		       "correct formula"))
      (math-correct-tree (buffer-tree))))

(tm-define (math-correct-manually)
  (:synopsis "Manually correct selected formula or all formulas in document")
  (if (selection-active-any?)
      (if (== (selection-tree) (path->tree (selection-path)))
	  (math-manually-correct-tree (path->tree (selection-path)))
	  (set-message "Only implemented for complete subtrees"
		       "correct formula"))
      (math-manually-correct-tree (buffer-tree))))
