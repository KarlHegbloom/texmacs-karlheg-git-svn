
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : htmltm.scm
;; DESCRIPTION : conversion of Html trees to TeXmacs trees
;; COPYRIGHT   : (C) 2003  David Allouche
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (convert html htmltm)
  (:use
    (convert tools tmlength) (convert tools tmcolor)
    (convert tools tmtable) (convert tools stm)
    (convert tools sxml)  (convert tools sxhtml)
    (convert tools environment)
    (convert tools xmltm) (convert mathml mathtm))
  (:export 
    parse-html-snippet parse-html-document
    html->texmacs
    htmltm-as-serial ;; for htmltm-test
    ))

(define (assoc-string-ci key alist)
  (list-find alist (lambda (pair) (string-ci=? key (car pair)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HTML color library
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (html-color->tmcolor s)
  (let ((rgb255 (or (html-named-color->rgb255 s)
		    (html-rgb-color->rgb255 s))))
    (if rgb255 (rgb255->tmcolor rgb255) #f)))

(define html-named-colors
  '(("black" (0 0 0)) ("silver" (192 192 192)) ("gray" (128 128 128))
    ("white" (255 255 255)) ("maroon" (128 0 0)) ("red" (255 0 0))
    ("purple" (128 0 128)) ("fuchsia" (255 0 255)) ("green" (0 128 0))
    ("lime" (0 255 0)) ("olive" (128 128 0)) ("yellow" (255 255 0))
    ("navy" (0 0 128)) ("blue" (0 0 255)) ("teal" (0 128 128))
    ("aqua" (0 0 255))))

(define (html-named-color->rgb255 s)
  (cond ((assoc-string-ci s html-named-colors) => second)
	(else #f)))

(define (html-rgb-color->rgb255 s)
  (let ((cs (string->list s)))
    (if (and (char=? #\# (car cs))
	     (== 7 (length cs))
	     (list-every char-hexadecimal? (cdr cs)))
	(with (hash r1 r2 g1 g2 b1 b2) cs
	  (map hexadecimal->integer `((,r1 ,r2) (,g1 ,g2) (,b1 ,b2))))
	#f)))

(define (hexadecimal->integer cs)
  (let next ((i 0) (cs cs))
    (if (pair? cs)
	(next (+ (* 16 i) (hexadecimal-digit->integer (car cs))) (cdr cs))
	i)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (htmltm-table env a c)
  ;; TODO: support @lang attributes
  ;; NOT SUPPORTED: @summary - for spoken rendering
  ;;                @title   - no tooltip feature
  ;;                @style   - no CSS support
  ;;                @events  - no event support
  (let ((cells (table-cells env a c)))
    (if (null? cells) '() ; empty table (corner case, invalid HTML)
	((cut table-align env a <>)
	 (append (list (tmtable->stm (tmtable (table-formats env a c) cells)))
		 (table-label env a))))))

(define (table-label env a)
  (let ((label (xmltm-attr->label a 'id)))
    (if label (list label) '())))

(define (table-align env a stms)
  ;; (tmhtml-env shtml-attribute-list symbol (list stm) -> (list stm))
  ;; NOTE: may be generalized to support @align for P, DIV and Hn.
  (let ((m (and-let* ((p (assoc 'align a))
		      (list-length=2? p))
	     (list-find '("left" "center" "right")
			(cut string-ci=? (second p) <>)))))
    (if (not m) stms
	(list `(document
		(with "paragraph mode" ,m
		  ,(stm-remove-unary-document
		    (htmltm-serial (htmltm-preserve-space? env) stms))))))))

(define (table-formats env a c)
  ;; As a convention, global properties are placed at the end of the list.
  ;; Remember that tmtable->tm reverses the list of table formats.
  (append (table-content-formats env c)
	  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	  ;; TODO: table cellspacing and cellpadding
	  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	  (table-background env a) (table-borders env a c)
	  (list (tmformat-table "cell hyphen" "t"))
	  (table-width env a)))

(define (table-width env a)
  ;; TODO: extend the typesetter to support hyphenated cells balancing
  (let ((len (htmltm-dimension a 'width)))
    (list (tmformat-frame "table width"
			  (if (tmlength-null? len)
			      (tmlength 1 'par)
			      len)))))

(define (table-background env a)
  (or (and-let* ((html-color (shtml-attr-non-null a 'background))
		 (tmcolor (html-color->tmcolor html-color)))
	(list (tmformat-table "cell background"
				    (html-color->tmcolor color))))
      '()))

(define (table-borders env a c)
  ;; HTML: BORDER alone is understood as FRAME=BORDER.
  ;;
  ;; Mozilla: Without RULES, a BORDER or FRAME attribute enables
  ;; Netscape-style table rendering (with bevels). An additional FRAME
  ;; attribute (non-valid) may disable the frame bevels but not the cell
  ;; bevels. With RULES, borders are rules are drawn as simple lines.
  ;; An empty (or invalid) FRAME attribute is understood as FRAME=BORDER.
  ;; An empty (or invalid) BORDER attribute is understood as BORDER=1.
  ;; An empty (or invalid) RULES attribute is understood as RULES=ALL.
  ;; A BORDER attribute with a negative number value is understood as BORDER=0
  ;; and disables bevelled table rendering.
  ;;
  ;; TeXmacs cannot draw bevelled tables, so we let an explicit FRAME
  ;; attributes overrides an empty BORDER attribute.

  (define (format-frame . names)
    (lambda (value)
      (map (lambda (s) (tmformat-frame s value)) names)))

  (define frame-values-alist
    `(("void" ,(delay (format-frame)))
      ("above" ,(delay (format-frame "table tborder")))
      ("below" ,(delay (format-frame "table bborder")))
      ("hsides" ,(delay (format-frame "table tborder" "table bborder")))
      ("lhs" ,(delay (format-frame "table lborder")))
      ("rhs" ,(delay (format-frame "table rborder")))
      ("vsides" ,(delay (format-frame "table lborder" "table rborder")))
      ("box" ,(delay (format-frame "table tborder" "table bborder"
				   "table lborder" "table rborder")))
      ("border" ,(delay (format-frame "table tborder" "table bborder"
				      "table lborder" "table rborder")))))
  (define rules-values-alist
    `(("none" ,(delay '()))
      ("groups" ,(delay (rules-groups)))
      ("rows" ,(delay (list (tmformat-table-but-bottom
			     "cell bborder" (tmlength 1 'px)))))
      ("cols" ,(delay (list (tmformat-table-but-right
			     "cell rborder" (tmlength 1 'px)))))
      ("all" ,(delay (list (tmformat-table-but-bottom
			    "cell bborder" (tmlength 1 'px))
			   (tmformat-table-but-right
			    "cell rborder" (tmlength 1 'px)))))))

  ;; Handle invalid values from FRAME and RULES here.
  (define (frame-value s)
    (let ((p (assoc-string-ci s frame-values-alist)))
      (if p (second p) (frame-value "border"))))
  (define (rules-value s)
    (let ((p (assoc-string-ci s rules-values-alist)))
      (if p (second p) (rules-value "all"))))
  (define (rules-groups) '()) ;;;;;;; TODO ;;;;;;;
  (let ((rules (rules-value "none"))	; default values
	(border 1)
	(frame (frame-value "void")))
    (define (enable-bevels! n)
      (set! frame (frame-value "border"))
      (set! rules (rules-value "all"))
      (set! border n))
    (and-let* ((@border (assoc 'border a))) ; special logic for BORDER
      (if (list-length=2? @border)
	  (let ((n (string->number (second @border))))
	    (cond ((not n) (enable-bevels! 1)) ; invalid BORDER value
		  ((<= n 0) (set! border 0))   ; zero border
		  (else (enable-bevels! n))))  ; valid BORDER value
	  (enable-bevels! 1)))		       ; empty BORDER attribute
    ;; Handle invalid empty FRAME and RULES here.
    (and-let* ((@frame (assoc 'frame a)))
      (if (list-length=2? @frame)
	  (set! frame (frame-value (second @frame)))
	  (set! frame (frame-value "border"))))
    (and-let* ((@rules (assoc 'rules a)))
      (if (list-length=2? @rules)
	  (set! rules (rules-value (second @rules)))
	  (set! rules (rules-value "all"))))
    (append
     (force rules)
     (if (= border 0) '()
	 ((force frame) (tmlength border 'px))))))

(define (table-content-formats env c)
  (sxhtml-table-fold table-content-formats/kons '() `(h:table ,@c)))

(define (table-content-formats/kons msg i j kar kdr)
  ;; NOTE: ignored cell attributes: header, scope, abbr, axis, class, title,
  ;;   style, intrisic events.
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; TODO: cell attributes: nowrap, width, height, id, bgcolor, align, char,
  ;;   charoff, valign.
  ;; TODO: row and column attributes (beware of alignement inheritance rules).
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (cond ((eq? msg :cell)
	 (let ((attrs (sxml-attr-list kar)))
	   (define (span->format html-name tm-name)
	     (let ((span (shtml-decode-span attrs html-name)))
	       (if (= 1 span) '()
		   (list (tmformat-cell (1+ i) (1+ j) tm-name span)))))
	   (append (span->format 'colspan "cell col span")
		   (span->format 'rowspan "cell row span")
		   kdr)))
	(else kdr)))

(define (table-cells env a c)
  ;; TODO: ID attributes on table elements
  (let ((table `(h:table ,@c)))
    (with (nrows ncols) (sxhtml-table-dimension table)
      ((cut <> :out-table #f #f #f)
       (sxhtml-table-fold (lambda (msg i j kar kdr) (kdr msg i j kar))
			  (cut table-cells/table ncols env '() <...>)
			  table)))))

(define (table-cells/table ncols env table msg i j kar)
  (cond ((eq? msg :out-table) (reverse! table))
	((eq? msg :in-row-group)
	 (xpath-descend
	  env kar
	  (lambda (new-env)
	    (cut table-cells/row-group ncols (list new-env env) table <...>))))
	;; no else clause
	))

(define (table-cells/row-group ncols envs table msg i j kar)
  (cond ((eq? msg :out-row-group)
	 (cut table-cells/table ncols (second envs) table <...>))
	((eq? msg :in-row)
	 (xpath-descend
	  (car envs) kar
	  (lambda (new-env)
	    (cut table-cells/row ncols (cons new-env envs)
		 table 0 '() <...>))))
	;; no else clause
	))

(define (cons-empty-cells n row)
  (do ((row row (cons "" row))
       (n n (1- n)))
      ((zero? n) row)))

(define (table-cells/row ncols envs table next-j row msg i j kar)
  (cond ((eq? msg :out-row)
	 (cut table-cells/row-group ncols (cdr envs)
	      (cons (reverse! (cons-empty-cells (- ncols next-j) row))
		    table)
	      <...>))
	((eq? msg :cell)
	 (cut table-cells/row ncols envs table (1+ j)
	      (cons (xpath-descend
		     (car envs) kar
		     (lambda (new-env)
		       (htmltm-args-serial
			new-env
			(htmltm-space-mixed new-env (sxml-content kar)))))
		    (cons-empty-cells (- j next-j) row))
	      <...>))
	;; no else clause
	))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Specific markup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; In the following functions, the attributes are:
;;  @env -- the dynamic environment of the converter
;;  @a   -- attribute list of the current element
;;  @c   -- contents of the current element

(define (htmltm-list-item env a c)
  ;; List markers are glued by the list handler.
  (list (xmltm-label-decorate
	 a 'id (htmltm-serial (htmltm-preserve-space? env)
			      `((document (item)) ,@(htmltm-args env c))))))

(define (htmltm-quote env a c)
  ;; WARNING: this is incomplete. Texmacs should have a macro for inline
  ;; quotation which puts in quotation marks in a language sensitive manner.
  (list (xmltm-label-decorate
	 a 'id (htmltm-serial (htmltm-preserve-space? env)
			      `("``" ,@(htmltm-args env c) "''")))))

(define (htmltm-anchor env a c)
  (list (htmltm-href->hlink
	 a (xmltm-label-decorate
	    a 'id (xmltm-label-decorate
		   a 'name (htmltm-args-serial env c))))))

(define (htmltm-href->hlink a body)
  (let ((href (shtml-attr-non-null a 'href)))
    (if href `(hlink ,body ,(xmltm-url-text href)) body)))

(define (htmltm-dimension attrs name)
  (let ((s (shtml-attr-non-null attrs name)))
    (if (not s) (tmlength)
	(cond ((string-null? s) (tmlength))
	      ((string->number s) => (lambda (n) (tmlength n 'px)))
	      ((and (string-ends? s "%")
		    (string->number (string-drop-right s 1)))
	       => (lambda (n) (tmlength (/ n 100) 'par)))
	      (else (tmlength))))))

(define (htmltm-image env a c)
  (let* ((s (xmltm-url-text (or (shtml-attr-non-null a 'src) "")))
	 (w (tmlength->string (htmltm-dimension a 'width)))
	 (h (tmlength->string (htmltm-dimension a 'height))))
    (list (xmltm-label-decorate
	   a 'id
	   (if (not (and (string-null? w) (string-null? h)))
	       `(postscript ,s ,w ,h "" "" "" "")
	       `(postscript ,s "*6383/10000" "" "" "" "" ""))))))

(define (htmltm-font env a c)
  ;; WARNING: do as old filter, but is fragile and not conformant
  (list (htmltm-with-size
	 a (htmltm-with-color
	    a (xmltm-label-decorate
	       a 'id (htmltm-args-serial env c))))))

(define (htmltm-with-size a x)
  ;; Helper for htmltm-font
  (let* ((sz (shtml-attr-non-null a 'size))
	 (mult
	  (assoc sz '(("-4" "0.5") ("-3" "0.6") ("-2" "0.7") ("-1" "0.8")
		      ("+1" "1.2") ("+2" "1.4") ("+3" "1.7") ("+4" "2")))))
    (if mult `(with "font size" ,(second mult) ,x) x)))

(define (htmltm-with-color a x)
  ;; Helper for htmltm-font
  (or (and-let* ((html-color (shtml-attr-non-null a 'color))
		 (tmcolor (html-color->tmcolor html-color)))
	`(with "color" ,(tmcolor->stm tmcolor) ,x))
      x))

(define (htmltm-br env a c)
  (if (sxhtml-list? (xpath-parent env))
      '()
      '((next-line))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main translation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (htmltm-drop env a c) '())

(define (htmltm-pass env a c)
  (let ((l (htmltm-args env c)))
    (if (and (null? l) (not (assoc 'id a))) '()
	(list (xmltm-label-decorate
	       a 'id (htmltm-serial
		      (htmltm-preserve-space? env) l))))))

(define (htmltm-args env l)
  ;; Convert the content list of an HTML element.
  (append-map (lambda (x) (xpath-descend env x (cut htmltm <> x))) l))

(define (htmltm-args-serial env l)
  (htmltm-serial (htmltm-preserve-space? env) (htmltm-args env l)))

(define (htmltm env t)
  ;; Convert a shxml element to texmacs.
  ;; Return a list which is either null, or contains a single serial node.
  ;; All methods must use this return convention.
  (sxml-dispatch (lambda (env t) (list (xmltm-text t)))
		 htmltm-pass env t))

(define (cleanup-root env root)
  (sxml-set-content root (htmltm-space-mixed env (sxml-content root))))

(define (htmltm-as-serial root)
  ;; As htmltm, but returns a serial node.
  ;; Actually also initializes the dynamic enviroment.
  ;; FIXME: move the htmlinitialization elsewhere for symmetry with htmltm.
  (define (sub env)
    (htmltm-serial (htmltm-preserve-space? env)
		   (htmltm env (cleanup-root env root))))
  (initialize-xpath
   (environment) root
   (cut initialize-htmltm <> sub)))

(define handler (cut htmltm-handler <> <> <> htmltm-args-serial))

(drd-dispatcher htmltm-methods%
  ;;; Document structure
  ((:or head title meta) htmltm-drop)
  ((:or html body) (handler :mixed :inline htmltm-pass))

  ;; Grouping
  (div  (handler :mixed :block  htmltm-pass))
  ;; TODO: convert 'align' attributes in div, p and headings
  (span (handler :mixed :inline htmltm-pass))

  ;; Headings
  (h1 (handler :mixed :block "chapter"))
  (h2 (handler :mixed :block "section"))
  (h3 (handler :mixed :block "subsection"))
  (h4 (handler :mixed :block "subsubsection"))
  (h5 (handler :mixed :block "paragraph"))
  (h6 (handler :mixed :block "subparagraph"))

  ;; Address and text direction
  (address (handler :mixed :block htmltm-pass))
  (bdo (handler :mixed :inline htmltm-pass))

  ;;; Structured text
  ;; Phrase elements
  (em      (handler :mixed :inline "em"))
  (strong  (handler :mixed :inline "strong"))
  (cite    (handler :mixed :inline "cite*"))
  (dfn     (handler :mixed :inline "dfn"))
  (code    (handler :mixed :inline "code*"))
  (samp    (handler :mixed :inline "samp"))
  (kbd     (handler :mixed :inline "kbd"))
  (var     (handler :mixed :inline "var"))
  (abbr    (handler :mixed :inline "abbr"))
  (acronym (handler :mixed :inline "acronym"))

  ;; Quotations
  ;; NOTE: there should be a texmacs macro for Q
  (q (handler :mixed :inline htmltm-quote))
  (blockquote (handler :mixed :block "quotation"))

  ;; Subscripts and superscripts
  (sub (handler :mixed :inline '(rsub)))
  (sup (handler :mixed :inline '(rsup)))

  ;; Lines and paragraphs
  (p (handler :mixed :block htmltm-pass))
  (br (handler :empty :inline htmltm-br))
  (pre (handler :pre :block "code"))

  ;; Document changes
  ((:or ins del) (handler :mixed :inline htmltm-pass))

  ;;; Lists
  (dl (handler :element :block "description"))
  (dt (handler :mixed :block "item*"))
  (dd (handler :mixed :block htmltm-pass))
  (ol (handler :element :block "enumerate"))
  (ul (handler :element :block "itemize"))
  (li (handler :mixed :block htmltm-list-item))
  (menu (handler :element :block "itemize")) ; deprecated
  (dir  (handler :element :block "itemize")) ; deprecated

  ;;; Tables
  (table (handler :element :block htmltm-table))
  ((:or col colgroup tbody thead tfoot tr td th)
   (handler :mixed :inline htmltm-pass))

  ;;; Links
  (a (handler :mixed :inline htmltm-anchor))
  ;; Elements allowed only in HEAD
  ((:or (link base)) htmltm-drop)

  ;;; Objects images and applets
  (object (handler :mixed :inline htmltm-drop))
  ;; TODO: handle cases where OBJECT is equivalent to IMG
  (param htmltm-drop) ; allowed only in object
  (img (handler :empty :inline htmltm-image))
  (applet (handler :mixed :inline htmltm-drop))
  (map (handler :element :inline htmltm-drop))
  (area htmltm-drop) ; allowed only in map

  ;;; Alignement, font styles and horizontal rules
  ;; Alignement (deprecated)
  ;; NOTE: the center macro is now deprecated.
  (center (handler :mixed :block '(with "paragraph mode" "center")))

  ;; Font style
  (tt  (handler :mixed :inline '(with "font family" "tt")))
  ; NOTE: the tt macro is now deprecated
  (i   (handler :mixed :inline '(with "font shape" "italic")))
  (b   (handler :mixed :inline '(with "font series" "bold")))
  (big (handler :mixed :inline '(with "font size" "1.2")))
  (small (handler :mixed :inline '(with "font size" "0.83")))
  ((:or s strike) (handler :mixed :inline htmltm-pass))
  (u (handler :mixed :inline "underline"))

  ;; Font modifiers (deprecated)
  (font (handler :mixed :inline htmltm-font))
  (basefont htmltm-drop)

  ;; Rules
  (hr (handler :empty :block '((value "hrule"))))

  ;;; Frames
  (frameset htmltm-drop)
  (frame htmltm-drop) ; allowed only in FRAMESET
  (noframes (handler :mixed :inline htmltm-pass))
  (iframe (handler :mixed :block htmltm-pass))

  ;;; Forms
  (form (handler :mixed :block htmltm-drop))
  ;; elements allowed only in FORM
  ((:or input button select optgroup option textarea isindex label fieldset
	legend) htmltm-drop)

  ;;; Scripting
  (script htmltm-drop)
  (noscript (handler :mixed :block htmltm-pass))

  ;; Tags present in the previous converter
  ;; Unknown: FIG FN NOTE AU LANG PERSON
  ;; ABBREV instead of ABBR
  ;; BQ as a shorthand for BLOCKQUOTE
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (parse-html-snippet s)
  (htmltm-parse s))

(define (parse-html-document s)
  `(!file ,(htmltm-parse s)))

(tm-define (html->texmacs html)
  (:type (object -> object))
  (:synopsis "Convert a parsed HTML object @t into a TeXmacs object.")
  (let* ((snippet? (not (func? html '!file 1)))
	 (body (if snippet? html (cadr html)))
	 (tm (htmltm-as-serial (sxhtml-correct-table body))))
    (if snippet? tm
	(let* ((doc (tree-simplify (object->tree (stm-unary-document tm))))
	       (body (tree1 'body doc))
	       (style (tree1 'style (string->tree "browser"))))
	  (tree2 'document body style)))))
