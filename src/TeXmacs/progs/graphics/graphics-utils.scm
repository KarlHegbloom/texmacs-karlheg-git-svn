
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : graphics-utils.scm
;; DESCRIPTION : utilities routines for graphics mode
;; COPYRIGHT   : (C) 2001  Joris van der Hoeven
;;               (C) 2004, 2005, 2006  Joris van der Hoeven and Henri Lesourd
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (graphics graphics-utils)
  (:use (utils library cursor) (utils library tree)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic scheme processing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;NOTE: This section is OK.
(define-macro (define-export-macro head . body)
 `(begin
     (define-macro ,(car head)
        (lambda ,(cdr head) ,@body)
     )
     (export ,(car head))))
     (export define-export-macro)
  ;; NOTE: Deprecated. It seems that as soon as macros become a little bit
  ;;   complex, the Guile macroexpander interacts poorly with the memoizing
  ;;   stuff in (define-public-macro), and then it becomes unstable. This
  ;;   is the reason why (define-export-macro) exists : it is *strictly*
  ;;   equivalent to (define-macro) + (export), and never raises problems.
  ;;
  ;; NOTE: Not deprecated anymore. using (define-public-macro) for
  ;;   defining (foreach-cons) is the cause of an unfixeable and
  ;;   ununderstandeable crash in (group-zoom). I don't understand
  ;;   why, (foreach-cons) is just expanding a (do) loop, it *should*
  ;;   work without any problems ! Thus, using (define-public-macro)
  ;;   is currently banned from the graphics-*.scm code.

;; Conversions
(tm-define (tree->object t)
  (if (or (symbol? t) (number? t))
      t
      (tm->stree t)))

(tm-define (object->tree o) (tm->tree o))

;;These abbreviations are very convenient
;;to use. A nice naming scheme is :
;;
;;  -> b=bool ;
;;  -> i=integer ;
;;  -> f=float ;
;;  -> sy=symbol ;
;;  -> s=string ;
;;  -> o=Scheme object ;
;;  -> p=path.
;;  -> t=tree.
;;
;;  One can add the missing ones on demand.
(tm-define f2s float->string)
(tm-define s2f string->float)
(tm-define sy2s symbol->string)
(tm-define s2sy string->symbol)
(tm-define o2s object->string)
(tm-define s2o string->object)
(tm-define t2o tree->object)
(tm-define o2t object->tree)

;; Error
(tm-define (graphics-error . msg)
  (foreach (e msg)
     (display e))
  (newline) ;(quit-TeXmacs)
)

;; Lists as bags
;; FIXME: One more time, due to an incomplete implementation
;;   of some very basic functionnality, we had to do our own
;;   hack...
;(tm-define seek-eq? memq)
;(tm-define remove-eq? delq1!)

(tm-define (complete-eq? x y)
  (if (and (tree? x) (tree? y))
      (tree-eq? x y)
      (eq? x y)))

(tm-define (seek-eq? x l)
  (if (pair? l)
      (if (complete-eq? x (car l))
          l
          (seek-eq? x (cdr l)))
      #f))

(tm-define (remove-eq0? x l)
  (define l0 (cons 'X l))
  (define (seek l prec)
     (if (pair? l)
         (if (complete-eq? x (car l))
             (set-cdr! prec (cdr l))
             (seek (cdr l) (cdr prec))))
  )
  (seek l l0)
  (cdr l0))

(define-export-macro (remove-eq? x l)
  (if (symbol? l)
     `(begin
	 (set! ,l (remove-eq0? ,x ,l))
	,l)
     `(remove-eq0? ,x ,l)))

;; Iterators
(define-public foreach for)

(define-public-macro (foreach-number what . body)
  (let ((n (length what)))
    (cond ((== n 3)
         ;;(foreach-number (i i0 iN) body[i])
          `(for (,(car what) ,(cadr what) ,(caddr what)) ,@body))
          ((== n 4)
         ;;(foreach-number (i i0 [< <= > >=] iN) body[i])
           (if (in? (caddr what) '(< <=))
              `(for (,(car what)
		   ,(cadr what) ,(cadddr what) 1 ,(caddr what)) ,@body)
              `(for (,(car what)
		   ,(cadr what) ,(cadddr what) -1 ,(caddr what)) ,@body)))
          ((== n 5)
         ;;(foreach-number (i i0 [< <= > >=] iN step) body[i])
           (if (in? (caddr what) '(< <=))
              `(for (,(car what)
		     ,(cadr what) ,(cadddr what)
		     ,(car (cddddr what)) ,(caddr what)) ,@body)
              `(for (,(car what)
		     ,(cadr what) ,(cadddr what)
		     ,(- 0 (car (cddddr what))) ,(caddr what)) ,@body)))
          (else '(noop)))))

(define-public-macro (foreach-cons i . b)
;;(foreach-cons (e l) i   body[elt]) -> for each cons e of the list l
;;(foreach-cons (e l1 l2) body[elt]) -> for each cons e in [l1...l2]
  (if (null? (cddr i))
    `(do ((,(car i) ,(cadr i) (cdr ,(car i)))
         )
         ((null? ,(car i)) ,(cadr i))
        ,(cons 'begin b)
     )
    `(do ((,(car i) ,(cadr i) (cdr ,(car i)))
         )
         ((or (null? ,(car i))
              (eq? ,(car i) ,(caddr i))
          )
          ,(cadr i)
         )
        ,(cons 'begin b))))
;;TODO: Extend (foreach-cons) for recursively traversing tree nodes.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Other utility routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (string-number=? s1 s2)
  (and (string? s1) (string? s2)
       (let* ((i1 (string->float s1))
	      (i2 (string->float s2)))
	 (and i1 i2 (== i1 i2)))))

(tm-define (string-symbol=? s1 s2)
  (if (symbol? s1)
      (set! s1 (symbol->string s1)))
  (if (symbol? s2)
      (set! s2 (symbol->string s2)))
  (== s1 s2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutines for accessing trees & managing listprops
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;FIXME: Should use (tm-adjust-path), otherwise, crashes in some cases
(tm-define (tm-upwards-path p tags nottags)
  (if (in? (tree-label (path->tree p)) tags)
      p
      (if (in? (tree-label (path->tree p)) nottags)
	  #f
	  (if (> (length p) 2)
	      (tm-upwards-path (cDr p) tags nottags)
	      #f))))
;; TODO: Put this one in kernel/library/tree.scm

;;NOTE: This section is OK.
(tm-define nothing (gensym))
(define list-find-prop-cons #f)
(tm-define (list-find-prop l var)
  (define (find l)
     (if (or (null? l) (null? (cdr l)))
	 nothing
	 (if (== (car l) var)
	     (begin
		(set! list-find-prop-cons (cdr l))
		(cadr l))
	     (if (null? (cdr l))
		 nothing
		 (find (cddr l)))))
  )
  (set! list-find-prop-cons #f)
  (if (null? l)
      nothing
      (find (cdr l))))

(tm-define (list-find&set-prop l var val)
  (list-find-prop l var)
  (if list-find-prop-cons
      (begin
	 (set-car! list-find-prop-cons val)
         l)
     `(with ,var ,val . ,l)))

(tm-define (tm-find-prop p var)
;;(tm-find-prop '<tree <with|a|1|...>> "a")              -> <tree 1>
;;(tm-find-prop (tree->path '<tree <with|a|1|...>>) "a") -> <tree 1>
  (if (null? p)
      nothing
      (let* ((t (if (tree? p) p (path->tree p)))
	     (n (tree-arity t))
	 )
	 (if (> n 2)
	     (with res nothing
		(foreach-number (i 0 < (- (/ n 2) 1))
		   (if (== (tree->stree (tree-ref t (* 2 i))) var)
		       (set! res (tree-ref t (+ (* 2 i) 1))))
		)
		res
	     )
	     nothing))))

(tm-define (find-prop l var . default)
;;(find-prop '(with "a" 1) "a")             -> 1
;;(find-prop '(with "a" 1) "b" "not found") -> "not found"
;;(find-prop '<tree <with|a|1|...>> "a")    -> 1
  (set! default (if (pair? default) (car default) #f))
  (with val ((if (tree? l) tm-find-prop list-find-prop) l var)
     (if (== val nothing)
	 default
	 (if (tree? val) (tree->stree val) val))))
;; TODO : Put this in utils/library/tree.scm

(tm-define (get-upwards-tree-property p var)
  (if (null? p)
      nothing
      (with q (tm-upwards-path p '(with) '())
	 (if (not q)
	     nothing
	     (with val (tm-find-prop q var)
		(if (== val nothing)
		    (get-upwards-property (cDr q) var)
		    val))))))

(tm-define (get-upwards-property p var)
  (t2o (get-upwards-tree-property p var)))

(tm-define (get-upwards-property-1 p var)
  (if (null? p)
      nothing
      (with q (tm-upwards-path p '(with) '())
	 (if (equal? q (cDr p))
	     (get-upwards-property p var)
	     nothing))))
;; TODO : Put this in utils/library/tree.scm

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutines for accessing the graphics
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;NOTE: This section is OK.
(tm-define gr-tags-all '(point
			 line cline spline cspline
			 arc carc
			 text-at
			 gr-group))
(tm-define gr-tags-curves       '(line cline spline cspline arc carc))
(tm-define gr-tags-point-curves `(point . ,gr-tags-curves))
(tm-define gr-tags-noncurves    '(point text-at gr-group))
(tm-define gr-tags-oneshot      '(point arc carc text-at gr-group))

(tm-define (stree-at p)
  (tree->stree (path->tree p)))
;; TODO: Put this in kernel/library/tree.scm

(tm-define (graphics-graphics-path)
  ;; path to innermost graphics tag
  (let* ((p (cDr (cursor-path)))
	 (t (path->tree p)))
    (if (eq? (tree-label t) 'graphics) p
	(with u (tree-innermost 'graphics)
	  (and u (tree->path u))))))

(tm-define (graphics-path path)
  (if (or (null? path) (null? (cdr path)))
      #f
      (with p (cDr path)
	 (with o (path->tree p)
	    (if (and (tree? o) ;; (in? (tree-label o) gr-tags-all)
		     (not (eq? (tree-label o) 'string))
		     (tm-upwards-path (cDr p) '(graphics) '(text-at)))
		(begin
		  ;;(display* "gp=" (path->tree (cDr path)) "\n")
		  (if (eq? (tree-label o) 'graphics) #f p))
		(graphics-path (cDr path)))))))

(tm-define (graphics-active-path)
  ;; path to active tag
  (graphics-path (cursor-path)))

(tm-define (graphics-group-path)
  ;; path to innermost group
  (graphics-graphics-path))

(tm-define (make-graphics)
  (graphics-reset-context 'begin)
  (insert-raw-go-to 
   '(with "gr-mode" "point" 
          "gr-frame" (tuple "scale" "1cm" (tuple "0.5gw" "0.5gh"))
          "gr-geometry" (tuple "geometry" "1par" "0.6par")
      (graphics))
   '(6 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutines for accessing the properties of the graphics
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;NOTE: This section is OK.
(define (graphics-get-raw-property var)
  (with val (get-upwards-tree-property (graphics-graphics-path) var)
     (if (eq? val nothing)
	 (get-default-tree-val var)
	 (if (eq? (tm-car val) 'quote)
	     (tree-ref val 0)
	     val))))
         
(tm-define (graphics-frozen-property? var)
  (with val (graphics-get-raw-property var)
     (and (tree? val) (== (tree-label val) 'freeze))))
         
(tm-define (graphics-frozen-property! var b)
  (if b
      (if (not (graphics-frozen-property? var))
          (graphics-set-property var
            `(quote (freeze ,(tree->stree (get-env-tree var))))))
      (if (graphics-frozen-property? var)
          (graphics-set-property
	     var (tree-ref (graphics-get-raw-property var) 0)))))
         
(tm-define (graphics-get-property var)
  (with val (graphics-get-raw-property var)
     (tree->stree
        (if (graphics-frozen-property? var)
            (tree-ref val 0) 
            val))))
               
(tm-define (graphics-change-property var val)
  (set! val (t2o val))
  (if (graphics-frozen-property? var)
      (graphics-set-property var `(quote (freeze ,val)))
      (graphics-set-property var val)))

(tm-define (graphics-set-property var val)
  (with p (graphics-graphics-path)
    (if p (path-insert-with p var val))))

(tm-define (graphics-remove-property var)
  (with p (graphics-graphics-path)
    (if p (path-remove-with p var))))

;; Magnification
(tm-define (graphics-eval-magnification)
  (tree->stree (get-env-tree-at
		  "magnification" (rcons (graphics-graphics-path) 0))))

(tm-define (graphics-eval-magnification-at path)
  (tree->stree (get-env-tree-at "magnification" path)))

(define (convert-magn m)
  (if (== m "default")
      (set! m 1.0))
  (if (string? m)
      (set! m (s2f m)))
  m)
  ;; FIXME: Using this function is crappy ; it would be
  ;;   much better to be sure that a magnification equal
  ;;   to "default" never enters the functions below.
  ;;   But this seems to depend on some realtime-dependent
  ;;   behaviour, thus testing this has not (yet) been
  ;;   done correctly.

(tm-define (multiply-magnification magn h)
  (set! h (convert-magn h))
  (cond ((equal? h 1.0)
         #t
        )
        ((and (pair? magn) (eq? (car magn) 'times))
         (set! h (* h (s2f (cadr magn))))
        )
        ((or (string? magn) (number? magn))
	 (set! magn (convert-magn magn))
         (set! h (* h magn))
        )
        (else
           (set! h 1.0))
  )
  (if (equal? h 1.0)
      #f
     `(times ,(f2s h) (value "magnification"))))

(tm-define (local-magnification amagn)
  (set! amagn (convert-magn amagn))
 `(times ,(f2s (/ amagn (s2f (graphics-eval-magnification))))
	  (value "magnification")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Enriching graphics with properties like color, line width, etc.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;NOTE: This section is OK.
(tm-define (graphics-valid-attribute? attr tag)
  (cond ((== tag 'point)
	 (in? attr '("color" "fill-color" "point-style")))
	((not (in? tag gr-tags-noncurves))
	 (in? attr '("color" "fill-color" "line-width"
		     "magnification" "dash-style" "dash-style-unit"
		     "line-arrows")))
	((== tag 'text-at)
	 (in? attr '("magnification" "text-at-halign" "text-at-valign")))
	((== tag 'gr-group)
	 (in? attr '("color" "fill-color"
		     "point-style" "line-width"
		     "magnification" "dash-style" "dash-style-unit"
		     "line-arrows"
		     "text-at-halign" "text-at-valign")))
	(else #f)))

(tm-define (graphics-enrich-filter t l)
  (if (null? l) l
      (let* ((head (car l))
	     (tail (graphics-enrich-filter t (cdr l))))
	(if (or (== (cadr head) "default")
		(== (cadr head) (get-default-val (car head)))
		(and (== (car head) "magnification")
		     (or (== (cadr head) "1.0")
			 (== (cadr head) '(times "1.0" (value "magnification")))))
		(not (graphics-valid-attribute? (car head) t)))
	    tail
	    (cons* (car head) (cadr head) tail)))))

(tm-define (graphics-enrich-sub t l)
  (with f (graphics-enrich-filter (car t) l)
    (if (null? f)
	t
	`(with ,@f ,t))))

(tm-define (graphics-enrich-bis t color ps lw mag st stu lp fc ha va)
  (let* ((mode (car t)))
    (cond ((== mode 'point)
	   (graphics-enrich-sub t
	    `(("color" ,color)
	      ("fill-color" ,fc)
	      ("point-style" ,ps))))
	  ((not (in? mode gr-tags-noncurves))
	   (graphics-enrich-sub t
	    `(("color" ,color)
	      ("line-width" ,lw)
	      ("magnification" ,mag)
	      ("dash-style" ,st) ("dash-style-unit" ,stu)
	      ("line-arrows" ,lp)
	      ("fill-color" ,fc))))
	  ((== mode 'text-at)
	   (graphics-enrich-sub t
	    `(("magnification" ,mag)
	      ("text-at-halign" ,ha)
	      ("text-at-valign" ,va))))
	  ((== mode 'gr-group)
	   (graphics-enrich-sub t
	    `(("color" ,color)
	      ("point-style" ,ps)
	      ("line-width" ,lw)
	      ("magnification" ,mag)
	      ("dash-style" ,st) ("dash-style-unit" ,stu)
	      ("line-arrows" ,lp)
	      ("fill-color" ,fc)
	      ("text-at-halign" ,ha)
	      ("text-at-valign" ,va))))
	  (else
	   (graphics-enrich-sub t '())))))

(tm-define (graphics-enrich t)
  (let* ((color (graphics-get-property "gr-color"))
	 (ps (graphics-get-property "gr-point-style"))
	 (lw (graphics-get-property "gr-line-width"))
	 (mag "1.0")
	 (st (graphics-get-property "gr-dash-style"))
	 (stu (graphics-get-property "gr-dash-style-unit"))
	 (lp (graphics-get-property "gr-line-arrows"))
	 (fc (graphics-get-property "gr-fill-color"))
	 (ha (graphics-get-property "gr-text-at-halign"))
	 (va (graphics-get-property "gr-text-at-valign")))
    (graphics-enrich-bis t color ps lw mag st stu lp fc ha va)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutines for modifying the innermost group of graphics
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (graphics-group-insert-bis t go-into)
 ;(display* "t=" t "\n")
  (let* ((p (graphics-group-path))
	 (p2 #f)
    )
    (if (null? layer-of-last-removed-object)
	(set! layer-of-last-removed-object #f))
    (if p (with n (if layer-of-last-removed-object
		      (if (pair? layer-of-last-removed-object)
			  (with val (car layer-of-last-removed-object)
			     (set! layer-of-last-removed-object
				   (cdr layer-of-last-removed-object))
                             val)
			  layer-of-last-removed-object)
                      (tree-arity (path->tree p)))
	    (tree-insert (path->tree p) n (list t))
	    (if (func? t 'with)
		(if (and go-into (func? (cAr t) 'text-at))
		    (set! p2 (append p (list n (- (length t) 2) 0 0)))
		    (set! p2 (append p (list n (- (length t) 2) 1))))
		(if (and go-into (func? t 'text-at))
		    (set! p2 (append p (list n 0 0)))
		    (set! p2 (append p (list n 0))))
	    )
	    (go-to p2)
	    (graphics-path p2)
	  )
	  #f)))

(tm-define (graphics-group-insert t)
  (graphics-group-insert-bis t #t))

(tm-define (graphics-group-enrich-insert t)
  (graphics-group-insert (graphics-enrich t)))

(tm-define (graphics-group-enrich-insert-bis
	    t color ps lw mag st stu lp fc ha va go-into)
  (graphics-group-insert-bis
    (graphics-enrich-bis t color ps lw mag st stu lp fc ha va) go-into))

(tm-define (graphics-group-start)
  (graphics-finish)
  (with p (graphics-group-path)
    (if p (go-to (rcons p 0)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutines for modifying the active tag
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;NOTE: This section is OK.
(tm-define (graphics-tree path)
  (with p (graphics-path path)
    (if p (path->tree p) #f)))

(tm-define (graphics-object path)
  (with p (graphics-path path)
    (if p (tree->stree (path->tree p)) #f)))

(tm-define (graphics-active-tree)
  (with p (graphics-active-path)
    (if p (path->tree p) #f)))

(tm-define (graphics-active-object)
  (with p (graphics-active-path)
    (if p (tree->stree (path->tree p)) #f)))

(tm-define (graphics-active-type)
  (with t (graphics-active-tree)
    (if t (tm-car t) #f)))
  ;;NOTE: Currently unused.
  ;;TODO: Test it.

(tm-define (graphics-active-val var)
  (graphics-active-property var (get-default-val var)))
  ;;NOTE: Currently unused.
  ;;TODO: Test it.

(tm-define (get-default-tree-val var)
  (get-init-tree var))

(tm-define (get-default-val var)
  (tree->stree (get-init-tree var)))

(tm-define (graphics-active-color)
  (graphics-active-val "color"))
  ;;NOTE: Currently unused.
  ;;TODO: Test it.

(tm-define (graphics-active-lwidth)
  (graphics-active-val "line-width"))
  ;;NOTE: Currently unused.
  ;;TODO: Test it.

(tm-define (graphics-path-property-bis p var default-val)
  (with c (get-upwards-property p var)
    (if (== c nothing) default-val c)))

(tm-define (graphics-path-property p var)
  (graphics-path-property-bis p var "default"))

(tm-define (graphics-path-property-bis-1 p var default-val)
  (with c (get-upwards-property-1 p var)
    (if (== c nothing) default-val c)))

(tm-define (graphics-path-property-1 p var)
  (graphics-path-property-bis-1 p var "default"))

(tm-define (graphics-active-property var default-val)
  (graphics-path-property-bis (graphics-active-path) var default-val))

(tm-define (graphics-active-assign t)
  (with p (graphics-active-path)
    (if p (begin
	    (tree-assign (path->tree p) t)
	    (go-to (rcons p 1))))))

(tm-define (graphics-active-set-tag l)
  (with t (graphics-active-object)
    (if t (graphics-active-assign (cons l (cdr t))))))

(tm-define (graphics-active-insert t)
  (with p (graphics-active-path)
    (if p (with n (tree-arity (path->tree p))
	    (tree-insert (path->tree p) n (list t))
	    (go-to (rcons p 1))))))

(tm-define (graphics-object-root-path p)
  (let* ((q (tm-upwards-path p '(with) '()))
	 (path (if (and q
			(== (+ (length q) 1) (length p)))
		   q p
	       )))
	path))
    
(tm-define (graphics-remove p . parms)
  (with p0 (graphics-object-root-path p)
     (set! layer-of-last-removed-object
	   (if (and (pair? parms) (eq? (car parms) 'memoize-layer))
	       (if (list? layer-of-last-removed-object)
		   (cons (cAr p0) layer-of-last-removed-object)
		   (cAr p0))
	       #f))
     (tree-remove (path->tree (cDr p0)) (cAr p0) 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Box info & frame
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;NOTE: This section is OK.
(tm-define (box-info t cmd)
  (tree->stree (texmacs-exec `(box-info ,t ,cmd))))

(tm-define (frame-direct p)
  (tree->stree (texmacs-exec `(frame-direct ,p))))

(tm-define (frame-inverse p)
  (tree->stree (texmacs-exec `(frame-inverse ,p))))

(tm-define (interval-intersects i1 i2)
  (let* ((i1a (car i1))
	 (i1b (cadr i1))
	 (i2a (car i2))
	 (i2b (cadr i2))
     )
     (or (and (<= i1a i2a) (>= i1b i2b))
	 (and (<= i2a i1a) (>= i2b i1b))
	 (and (>= i1a i2a) (<= i1a i2b))
	 (and (>= i2a i1a) (<= i2a i1b)))))

(tm-define (box-intersects t1 t2)
  (define (max-box t)
     (let* ((bx1 (box-info t "lbrt"))
	    (bx2 (box-info t "LBRT"))
	)
	(set! bx1 (map s2f (cdr bx1)))
	(set! bx2 (map s2f (cdr bx2)))
       `(,(min (car bx1) (car bx2))
	 ,(min (cadr bx1) (cadr bx2))
	 ,(max (caddr bx1) (caddr bx2))
	 ,(max (cadddr bx1) (cadddr bx2))))
  )
  (let* ((box1 (max-box t1))
	 (box2 (max-box t2))
    )
    (and (interval-intersects `(,(car box1) ,(caddr box1))
			      `(,(car box2) ,(caddr box2)))
	 (interval-intersects `(,(cadr box1) ,(cadddr box1))
			      `(,(cadr box2) ,(cadddr box2))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Enhanced trees
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (enhanced-tree? t)
  (eq? (tree-label t) 'with))

(tm-define (enhanced-tree->radical t)
  (if (enhanced-tree? t)
      (tree-ref t (- (tree-arity t) 1))
      t))

(tm-define (radical->enhanced-tree r)
  (with t (tree-up r)
     (if (enhanced-tree? t) t r)))

(tm-define (enhanced-tree-arity t)
  (tree-arity (enhanced-tree->radical t)))

(tm-define (enhanced-tree-ref t i)
  (tree-ref (enhanced-tree->radical t) i))

(tm-define (enhanced-tree-set! t i val)
  (tree-set! (enhanced-tree->radical t) i val))

(tm-define (enhanced-tree-insert! t i val)
  (tree-insert! (enhanced-tree->radical t) i `(,val)))

(tm-define (enhanced-tree-remove! t i n)
  (tree-remove! (enhanced-tree->radical t) i n))

(tm-define (enhanced-tree-property-ref t var)
  (define res #f)
  (if (string? var)
      (set! var (stree->tree var)))
  (if (enhanced-tree? t)
      (foreach-number (i 0 < (- (tree-arity r) 1))
	 (if (equal? (tree-ref t i) var)
	     (set! res (tree-ref t (+ i 1))))
	 (set! i (+ 1 i))))
  res)

(tm-define (enhanced-tree-property-set! t var val)
  (define found #f)
  (if (string? var)
      (set! var (stree->tree var)))
  (if (enhanced-tree? t)
      (begin
	 (foreach-number (i 0 < (- (tree-arity r) 1))
	    (if (equal? (tree-ref t i) var)
		(begin
		   (set! found #t)
		   (tree-set! t (+ i 1) val)))
	    (set! i (+ 1 i))
	 )
         (if (not found)
	     (tree-insert! t (tree-arity t) `(,var ,val))))))
