;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : latex-drd.scm
;; DESCRIPTION : Formal specification of the part of LaTeX
;;               which is understood by TeXmacs
;; COPYRIGHT   : (C) 1999  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (convert latex latex-drd)
  (:use (convert latex latex-symbol-drd)
	(convert latex latex-texmacs-drd)))

(logic-rules
  ((latex-tag% 'x) (latex-arity% 'x 'y)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LaTeX commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(logic-group latex-command-0%
  ,(string->symbol " ") ,(string->symbol ";") 
  ,(string->symbol ",") ,(string->symbol ":") 
  - / [ ] ! * ,(string->symbol "|") i j ss SS oe OE ae AE
  AA DH L NG O S TH aa dh dj l ng o th pounds colon and lq rq
  quad qquad enspace thinspace par smallskip medskip bigskip
  noindent newline linebreak nobreak nolinebreak strut
  pagebreak nopagebreak newpage newdoublepage clearpage cleardoublepage
  newblock bgroup egroup protect cr hfil hfill hfilll appendix limits nolimits
  dots maketitle tableofcontents TeX LaTeX onecolumn twocolumn
  begingroup endgroup printindex today bmod toprule midrule bottomrule

  ;; AMS commands
  qed dotsc dotsb dotsm dotsi dotso
  ;; temporarily
  hline
  ;; rewritten
  notin vert Vert addots
  implies iff gets
  ;; wikipedia
  infin rang

  ;; Algorithms
  AND BlankLine Ensure ENSURE FALSE GLOBALS NOT OR PRINT Require REQUIRE RETURN
  State STATE TO KwTo TRUE XOR Else ENDBODY EndFor ENDFOR EndFunction EndIf
  ENDIF ENDINPUTS EndLoop ENDLOOP ENDOUTPUTS EndProcedure ENDWHILE EndWhile
  Loop)

(logic-group latex-command-1%
  part* chapter* section* subsection* subsubsection* paragraph* subparagraph*
  nextbib
  footnote overline underline <sub> <sup> not left middle right
  big Big bigg Bigg bigl Bigl biggl Biggl
  bigm Bigm biggm Biggm bigr Bigr biggr Biggr
  bar Bar hat Hat tilde Tilde widehat widetilde vec Vec bm
  grave Grave acute Acute check Check breve Breve invbreve abovering mathring
  dot Dot ddot Ddot dddot ddddot mod pod pmod
  label ref pageref index hspace hspace* vspace vspace*
  mbox hbox textnormal text not substack
  ,(string->symbol "'") ,(string->symbol "`") ,(string->symbol "\"")
  ^ over atop choose ~ = u v H t c d b k r textsuperscript textsubscript
  thispagestyle ensuremath
  mathord mathbin mathopen mathpunct mathop mathrel mathclose mathalpha
  arabic alph Alph roman Roman fnsymbol displaylines cases underbrace overbrace
  phantom hphantom vphantom smash date terms
  newcounter stepcounter refstepcounter value
  citealt citealt* citealp*
  citetext citeauthor citeauthor* citeyear onlinecite
  epsfig url penalty centerline fbox framebox cline cmidrule
  enlargethispage
  newlength newdimen newskip
  Comment COMMENT For ForAll If Input KwData KwResult KwRet lnl nllabel
  lElse uElse Output Repeat Until UNTIL While)

(logic-group latex-command-1% ;; . needs a special treatment
  ,(string->symbol "."))

(logic-group latex-command-2%
  binom tbinom dbinom cfrac tfrac equal href
  sideset stackrel citeauthoryear
  setcounter addtocounter setlength addtolength
  scalebox texorpdfstring
  Call Function Procedure SetKw SetKwData SetKwFunction SetKwInOut)

(logic-group latex-command-3%
  ifthenelse resizebox @setfontsize eIf multicolumn)

(logic-group latex-command-4%
  mathchoice)

(logic-group latex-command-6%
  genfrac @startsection)

(logic-rules
  ((latex-command% 'x) (latex-command-0% 'x))
  ((latex-arity% 'x 0) (latex-command-0% 'x))
  ((latex-command% 'x) (latex-command-1% 'x))
  ((latex-arity% 'x 1) (latex-command-1% 'x))
  ((latex-command% 'x) (latex-command-2% 'x))
  ((latex-arity% 'x 2) (latex-command-2% 'x))
  ((latex-command% 'x) (latex-command-3% 'x))
  ((latex-arity% 'x 3) (latex-command-3% 'x))
  ((latex-command% 'x) (latex-command-4% 'x))
  ((latex-arity% 'x 4) (latex-command-4% 'x))
  ((latex-command% 'x) (latex-command-6% 'x))
  ((latex-arity% 'x 6) (latex-command-6% 'x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LaTeX commands with optional arguments
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(logic-group latex-command-0*%
  item ,(string->symbol "\\")
  BODY ELSE INPUTS LOOP OUTPUTS REPEAT)

(logic-group latex-command-1*%
  usepackage documentclass documentstyle sqrt bibitem cite caption  
  title author thanks
  part chapter section subsection subsubsection paragraph subparagraph
  includegraphics includegraphics*
  subjclass declaretheorem footnotetext
  xleftarrow xrightarrow xleftrightarrow xminus
  xLeftarrow xRightarrow xLeftrightarrow xequal
  xmapsto xmapsfrom citealp citet citep citet* citep*
  Begin ELSIF FORALL FOR IF WHILE tcp tcp* tcc tcc*)

(logic-group latex-command-2*%
  def newcommand renewcommand newtheorem newtheorem* frac parbox 
  ElseIf uElseIf lElseIf ForEach lForEach lForAll lFor)

(logic-group latex-command-3*%
  category newenvironment renewenvironment multirow)

(logic-rules
  ((latex-command-0% 'x) (latex-command-0*% 'x))
  ((latex-optional-arg% 'x) (latex-command-0*% 'x))
  ((latex-command-1% 'x) (latex-command-1*% 'x))
  ((latex-optional-arg% 'x) (latex-command-1*% 'x))
  ((latex-command-2% 'x) (latex-command-2*% 'x))
  ((latex-optional-arg% 'x) (latex-command-2*% 'x))
  ((latex-command-3% 'x) (latex-command-3*% 'x))
  ((latex-optional-arg% 'x) (latex-command-3*% 'x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Environments
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(logic-group latex-environment-0%
  begin-document begin-abstract begin-verbatim begin-proof
  begin-matrix begin-pmatrix begin-bmatrix begin-vmatrix begin-smallmatrix
  begin-cases
  begin-center begin-flushleft begin-flushright
  begin-picture)

(logic-group latex-environment-0*%
  begin-figure begin-table begin-figure* begin-table*
  begin-algorithmic begin-algorithm begin-algorithm2e)

(logic-group latex-environment-1%
  begin-tabbing begin-thebibliography begin-multicols)

(logic-group latex-environment-1*%
  begin-array begin-tabular begin-minipage)

(logic-group latex-environment-2*%
  begin-tabular* begin-tabularx)

(logic-rules
  ((latex-environment% 'x) (latex-environment-0% 'x))
  ((latex-arity% 'x 0) (latex-environment-0% 'x))
  ((latex-environment% 'x) (latex-environment-1% 'x))
  ((latex-arity% 'x 1) (latex-environment-1% 'x))
  ((latex-environment% 'x) (latex-environment-2% 'x))
  ((latex-arity% 'x 2) (latex-environment-2% 'x))
  ((latex-environment% 'x) (latex-environment-3% 'x))
  ((latex-arity% 'x 3) (latex-environment-3% 'x))
  ((latex-environment-0% 'x) (latex-environment-0*% 'x))
  ((latex-optional-arg% 'x) (latex-environment-0*% 'x))
  ((latex-environment-1% 'x) (latex-environment-1*% 'x))
  ((latex-optional-arg% 'x) (latex-environment-1*% 'x))
  ((latex-environment-2% 'x) (latex-environment-2*% 'x))
  ((latex-optional-arg% 'x) (latex-environment-2*% 'x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Modifiers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(logic-group latex-modifier-0%
  rm tt sf md bf it em sl sc rmfamily ttfamily sffamily
  mdseries bfseries upshape itshape slshape scshape
  displaystyle textstyle scriptstyle scriptscriptstyle cal frak Bbb boldmath
  tiny scriptsize footnotesize small normalsize
  large Large LARGE huge Huge
  black white grey red blue yellow green orange magenta brown pink
  centering raggedleft raggedright flushleft flushright)

(logic-group latex-modifier-1%
  textrm texttt textsf textmd textbf textup textit textsl textsc emph
  mathrm mathtt mathsf mathmd mathbf mathup mathit mathsl mathnormal
  mathcal mathfrak mathbb mathbbm mathscr operatorname boldsymbol
  lowercase MakeLowercase uppercase MakeUppercase selectlanguage)

(logic-rules
  ((latex-modifier% 'x) (latex-modifier-0% 'x))
  ((latex-arity% 'x 0) (latex-modifier-0% 'x))
  ((latex-modifier% 'x) (latex-modifier-1% 'x))
  ((latex-arity% 'x 1) (latex-modifier-1% 'x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Special types of LaTeX primitives
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(logic-group latex-control%
  $ & % ,(string->symbol "#") _ { } <less> <gtr>)

(logic-group latex-operator%
  arccos arcsin arctan arg cos cosh cot coth csc deg det dim exp gcd hom
  inf ker lg lim liminf limsup ln log max min Pr sec sin sinh sup tan tanh)

(logic-group latex-list%
  begin-itemize begin-enumerate begin-description
  begin-asparaitem begin-inparaitem begin-compactitem
  begin-asparaenum begin-inparaenum begin-compactenum)

(logic-group latex-math-environment-0%
  begin-formula begin-equation*
  begin-math begin-displaymath begin-equation
  begin-eqnarray begin-eqnarray*
  begin-align begin-align*
  begin-multline begin-multline*
  begin-gather begin-gather*
  begin-eqsplit begin-eqsplit*)

(logic-rules
  ((latex-arity% 'x 0) (latex-control% 'x))
  ((latex-arity% 'x 0) (latex-operator% 'x))
  ((latex-environment-0*% 'x) (latex-list% 'x))
  ((latex-math-environment% 'x) (latex-math-environment-0% 'x))
  ((latex-environment-0% 'x) (latex-math-environment-0% 'x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Symbols
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(logic-group latex-symbol%
  ;; Greek letters
  Gamma Delta Theta Lambda Xi Pi Sigma Upsilon Phi Psi Omega
  alpha beta gamma delta epsilon
  varepsilon zeta eta theta vartheta
  iota kappa lambda mu nu omicron
  xi pi varpi rho
  varrho sigma varsigma tau upsilon
  phi varphi chi psi omega

  ;; Binary operations
  pm mp times div ast star circ bullet cdot
  cap cup uplus sqcap sqcup vee wedge setminus wr
  diamond triangleleft triangleright land lor lnot
  oplus ominus otimes oslash odot bigcirc amalg notin

  ;; Relations
  leq le geq ge equiv models prec
  succ sim perp preceq succeq
  simeq mid ll gg asymp
  parallel subset supset approx bowtie
  subseteq supseteq cong Join sqsubset
  sqsupset ne neq smile sqsubseteq sqsupseteq
  doteq frown in ni propto
  vdash dashv
  
  ;; Arrows
  leftarrow rightarrow uparrow downarrow
  Leftarrow Rightarrow Uparrow Downarrow
  nearrow searrow swarrow nwarrow
  leftrightarrow updownarrow Updownarrow Leftrightarrow 
  leftharpoonup leftharpoondown rightharpoonup rightharpoondown
  hookleftarrow hookrightarrow
  to mapsto longmapsto
  longrightarrow longleftarrow longleftrightarrow
  Longrightarrow Longleftarrow Longleftrightarrow 
  
  ;; Miscellaneous symbols
  ldots cdots vdots ddots hdots aleph
  prime forall infty hbar emptyset
  exists nabla surd triangle
  imath jmath ell neg
  top flat natural sharp wp
  bot clubsuit diamondsuit heartsuit spadesuit
  Re Im angle partial textbackslash
  dag ddag dagger ddagger guillemotleft guillemotright

  ;; Delimiters
  uparrow Uparrow downarrow Downarrow
  updownarrow Updownarrow
  lfloor rfloor lceil rceil
  langle rangle backslash

  ;; Big delimiters
  rmoustache lmoustache rgroup lgroup lbrace rbrace
  arrowvert Arrowvert bracevert

  ;; Binary operations (latexsym or amssymb required)
  lhd rhd unlhd unrhd leadsto

  ;; Miscellaneous symbols (amssymb or graphicx required)
  Diamond mho)

(logic-group latex-big-symbol%
  sum int bigintwl oint bigointwl prod coprod
  bignone bigtimes bigoplus bigotimes bigodot
  bigvee bigwedge bigsqcup bigcup bigcap bigpluscup bigtriangledown
  bigtriangleup bigcurlyvee bigcurlywedge bigsqcap bigbox bigparallel
  biginterleave bignplus bigvarint bigiint bigiiint bigvaroint bigoiint)

(logic-rules
  ((latex-arity% 'x 0) (latex-symbol% 'x))
  ((latex-arity% 'x 0) (latex-big-symbol% 'x))
  ((latex-symbol% 'x) (latex-ams-symbol% 'x))
  ((latex-symbol% 'x) (latex-wasy-symbol% 'x))
  ((latex-symbol% 'x) (latex-stmary-symbol% 'x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Counters
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(logic-group latex-counter%
  badness enumi enumii enumiii enumiv equation figure inputlineno
  mpfootnote page setlanguage table)

(logic-rules
  ((latex-arity% 'x 0) (latex-counter% 'x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Names
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(logic-group latex-name%
  abstractname appendixname contentname figurename indexname
  litfigurename littablename partname refname tablename)

(logic-rules
  ((latex-arity% 'x 0) (latex-name% 'x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lengths
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(logic-group latex-length%
  ;; From latex.ltx
  ;; -- lengths
  @textfloatsheight arraycolsep arrayrulewidth columnsep columnseprule
  columnwidth doublerulesep emergencystretch evensidemargin fboxrule
  fboxsep footnotesep footskip headheight headsep itemindent labelsep
  labelwidth leftmargin leftmargini leftmarginii leftmarginiii
  leftmarginiv leftmarginv leftmarginvi linewidth listparindent
  marginparpush marginparsep marginparwidth oddsidemargin p@ paperheight
  paperwidth rightmargin tabbingsep tabcolsep textheight textwidth
  topmargin unitlength z@ @bls @vpt @vipt @viipt @viiipt @ixpt @xpt @xipt
  @xiipt @xivpt @xviipt @xxpt @xxvpt 
  ;; -- skips
  topsep partopsep itemsep parsep floatsep textfloatsep intextsep
  dblfloatsep dbltextfloatsep 
  ;; From latex classes
  abovecaptionskip belowcaptionskip bibindent
  ;; From fleqn
  mathindent
  ;; Plain TeX
  maxdimen hfuzz vfuzz overfullrule hsize vsize maxdepth lineskiplimit
  delimitershortfall nulldelimiterspace scriptspace mathsurround
  predisplaysize displaywidth displayindent parindent hangindent hoffset
  voffset baselineskip lineskip parskip abovedisplayskip
  abovedisplayshortskip belowdisplayskip belowdisplayshortskip leftskip
  rightskip topskip splittopskip tabskip spaceskip xspaceskip parfillskip
  thinmuskip medmuskip thickmuskip hideskip smallskipamount medskipamount
  bigskipamount normalbaselineskip normallineskip normallineskiplimit jot 
  )

(logic-rules
  ((latex-arity% 'x 0) (latex-length% 'x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; To be imported as pictures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(logic-group latex-as-pic-0%
  begin-pspicture begin-pspicture* begin-tikzpicture)

(logic-group latex-as-pic-1%
  xymatrix)

(logic-rules
  ((latex-as-pic% 'x)       (latex-as-pic-0% 'x))
  ((latex-as-pic% 'x)       (latex-as-pic-1% 'x))
  ((latex-arity%  'x 0)     (latex-as-pic-0% 'x))
  ((latex-arity%  'x 1)     (latex-as-pic-1% 'x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; To be ignored
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(logic-group latex-ignore-0%
  allowbreak notag xspace break sloppy makeatother makeatletter relax
  qedhere
  ignorespacesafterend ignorespaces balancecolumns)

(logic-group latex-ignore-0*%
	displaybreak allowdisplaybreaks)

(logic-group latex-ignore-1%
	tag hyphenation)

(logic-rules
	((latex-ignore% 'x)       (latex-ignore-0% 'x))
	((latex-ignore% 'x)       (latex-ignore-0*% 'x))
	((latex-ignore% 'x)       (latex-ignore-1% 'x))
  ((latex-arity% 'x 0)      (latex-ignore-0% 'x))
  ((latex-arity% 'x 0)      (latex-ignore-0*% 'x))
  ((latex-arity% 'x 1)      (latex-ignore-1% 'x))
  ((latex-optional-arg% 'x) (latex-ignore-1*% 'x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Using packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(logic-table latex-package-priority%
  ("geometry" 10)
  ("amsmath" 20)
  ("amssymb" 30)
  ("graphicx" 40)
  ("wasysym" 50)
  ("stmaryrd" 60)
  ("enumerate" 70)
  ("epsfig" 80)
  ("mathrsfs" 90)
  ("bbm" 100)
  ("dsfont" 110)
  ("euscript" 120)
  ("multicol" 130)
  ("hyperref" 140))

(logic-table latex-needs%
  (!verbatim "alltt")
  (!verbatim* "alltt")
  ("alltt" "alltt")

  (geometry "geometry")
  (epsfig "epsfig")
  (includegraphics "graphicx")

  (mathscr "mathrsfs")
  (EuScript "euscript")
  (mathbbm "bbm")
  (mathbbmss "bbm")
  (mathds "dsfont")
  (mathfrak "amssymb")
  (mathbb "amssymb")
  (theorembodyfont "theorem")

  (leadsto "leadsto")
  (nleadsto "leadsto")
  (Diamond "amssymb")
  (text "amsmath")
  (binom "amsmath")
  (dbinom "amsmath")
  (tbinom "amsmath")
  (dddot "amsmath")
  (ddddot "amsmath")
  (ontop "amsmath")
  (mod "amsmath")
  (pod "amsmath")
  (overset "amsmath")
  (underset "amsmath")
  (operatorname "amsmath")
  (boldsymbol "amsmath")
  (lleq "amsmath")
  (llleq "amsmath")
  (ggeq "amsmath")
  (gggeq "amsmath")
  (qed "amsmath")

  (xminus "amsmath")
  (xleftarrow "amsmath")
  (xrightarrow "amsmath")
  (xleftrightarrow "amsmath")
  (xmapsto "amsmath")
  (xmapsfrom "amsmath")
  (xequal "amsmath")
  (xLeftarrow "amsmath")
  (xRightarrow "amsmath")
  (xLeftrightarrow "amsmath")

  (proof "amsmath")

  (bigbox "stmaryrd")
  (bigcurlyvee "stmaryrd")
  (bigcurlywedge "stmaryrd")
  (biginterleave "stmaryrd")
  (bignplus "stmaryrd")
  (bigparallel "stmaryrd")
  (bigsqcap "stmaryrd")

  (btimes "graphicx")
  (Backepsilon "graphicx")
  (Mho "graphicx")
  (mho "graphicx")
  (upequal "graphicx")

  (ifthenelse "ifthen")
  (captionof "capt-of")
  (widthof "calc")
  
  (color "xcolor")

  (omicron "pslatex")
  (multicols "multicol")
  (bundle "epic")
  (chunk "epic")
  (bundle "ecltree")
  (chunk "ecltree")

  (url "hyperref")
  (href "hyperref")

  (citet "natbib")
  (citep "natbib")
  (citet* "natbib")
  (citep* "natbib")
  (citealt "natbib")
  (citealp "natbib")
  (citealt* "natbib")
  (citealp* "natbib")
  (citetext "natbib")
  (citeauthor "natbib")
  (citeauthor* "natbib")
  (citeyear "natbib")

  (index "makeidx")
  (printindex "makeidx")

  (inparaenum "paralist"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Deprecated routines for consulting the database
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (latex-resolve s)
  (if (string-starts? s "\\")
      (set! s (substring s 1 (string-length s))))
  (with arity (logic-ref latex-arity% (string->symbol s))
    (if (logic-in? (string->symbol s) latex-optional-arg%)
	(set! arity (- -1 arity)))
    (if (string-starts? s "end-")
	(begin
	  (set! s (string-append "begin-" (substring s 4 (string-length s))))
	  (set! arity 0)))
    (values (string->symbol s) arity)))

(tm-define (latex-arity tag)
  "Get the arity of a LaTeX @tag"
  (receive (s arity) (latex-resolve tag)
    (or arity 0)))

(tm-define (latex-type tag)
  "Get the type of a LaTeX @tag"
  (receive (s arity) (latex-resolve tag)
    (cond ((not arity) "undefined")
          ((logic-in? s latex-command%) "command")
          ((logic-in? s latex-length%) "length")
          ((logic-in? s latex-ignore%) "ignore")
          ((logic-in? s latex-as-pic%) "as-picture")
          ((logic-in? s latex-name%) "name")
          ((logic-in? s latex-counter%) "counter")
	  ((logic-in? s latex-modifier%) "modifier")
	  ((logic-in? s latex-control%) "control")
	  ((logic-in? s latex-operator%) "operator")
	  ((logic-in? s latex-list%) "list")
	  ((logic-in? s latex-math-environment%) "math-environment")
	  ((logic-in? s latex-environment%) "environment")
	  ((logic-in? s latex-texmacs%) "texmacs")
	  ((logic-in? s latex-symbol%) "symbol")
	  ((logic-in? s latex-big-symbol%) "big-symbol")
	  (else "undefined"))))
