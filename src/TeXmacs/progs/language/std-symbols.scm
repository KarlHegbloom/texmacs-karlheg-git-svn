
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : std-symbols.scm
;; DESCRIPTION : standard mathematical symbols
;; COPYRIGHT   : (C) 2010  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (texmacs-module (language std-symbols))

(define-module (language std-symbols)
  :use-module (texmacs-core))

(define-language std-symbols
  (:synopsis "default semantics for mathematical symbols")

  (define Assign-symbol
    (:type infix)
    (:penalty 3)
    (:spacing default default)
    "<assign>" "<plusassign>" "<minusassign>" "<astassign>" "<overassign>")
  
  (define Flux-symbol
    (:type infix)
    (:spacing default default)
    "<lflux>" "<gflux>")

  (define Models-symbol
    (:type infix)
    (:spacing default default)
    "<models>" "<vdash>" "<dashv>" "<vDash>" "<Vdash>" "<Vvdash>" "<VDash>"
    "<longvdash>" "<longdashv>" "<longvDash>"
    "<longVdash>" "<longVvdash>" "<longVDash>"
    "<nvdash>" "<ndashv>" "<nvDash>" "<nVdash>" "<nVvdash>" "<nVDash>")

  (define Quantifier-symbol
    (:type prefix)
    "<forall>" "<exists>" "<nexists>" "<Exists>" "<mathlambda>")

  (define Imply-nolim-symbol
    (:type infix)
    (:penalty 10)
    (:spacing default default)
    "<implies>" "<equivalent>" "<Leftarrow>" "<Rightarrow>" "<Leftrightarrow>"
    "<Longleftarrow>" "<Longrightarrow>" "<Longleftrightarrow>"
    "<Lleftarrow>" "<Rrightarrow>"
    "<nLeftarrow>" "<nRightarrow>" "<nLeftrightarrow>")

  (define Imply-lim-symbol
    (:type infix)
    (:penalty 10)
    (:spacing default default)
    (:limits always)
    "<Leftarrowlim>" "<Rightarrowlim>" "<Leftrightarrowlim>"
    "<Longleftarrowlim>" "<Longrightarrowlim>" "<Longleftrightarrowlim>")
  
  (define Imply-symbol
    Imply-nolim-symbol Imply-symbol)

  (define Or-symbol
    (:type infix)
    (:penalty 10)
    (:spacing default default)
    "<vee>" "<curlyvee>")

  (define And-symbol
    (:type infix)
    (:penalty 10)
    (:spacing default default)
    "<wedge>" "<curlywedge>")

  (define Not-symbol
    (:type prefix)
    (:penalty 10)
    "<neg>")

  (define Relation-nolim-symbol
    (:type infix)
    (:penalty 20)
    (:spacing default default)

    "=" "<ne>" "<neq>" "<longequal>" "<less>" "<gtr>" "<le>" "<leq>"
    "<prec>" "<preceq>" "<ll>" "<lleq>" "<subset>" "<subseteq>"
    "<sqsubset>" "<sqsubseteq>" "<in>" "<ni>" "<of>"
    "<ge>" "<geq>" "<succ>" "<succeq>"
    "<gg>" "<ggeq>" "<supset>" "<supseteq>" "<sqsupset>" "<sqsupseteq>"
    "<equiv>" "<nequiv>" "<sim>" "<simeq>" "<asymp>" "<approx>" "<cong>"
    "<subsetsim>" "<supsetsim>" "<doteq>" "<propto>" "<varpropto>"
    "<perp>" "<bowtie>" "<Join>" "<smile>" "<frown>" "<signchange>"
    "<parallel>" "<shortparallel>" "<nparallel>" "<nshortparallel>"
    "<shortmid>" "<nshortmid>" "<nmid>" "<divides>" "<ndivides>"

    "<approxeq>" "<backsim>" "<backsimeq>" "<Bumpeq>" "<bumpeq>" "<circeq>"
    "<curlyeqprec>" "<curlyeqsucc>" "<Doteq>" "<doteqdot>" "<eqcirc>"
    "<eqslantgtr>" "<eqslantless>" "<fallingdotseq>" "<geqq>" "<geqslant>"
    "<ggg>" "<gggtr>" "<gnapprox>" "<gneq>" "<gneqq>" "<gnsim>" "<gtrapprox>"
    "<gtrdot>" "<gtreqdot>" "<gtreqless>" "<gtreqqless>" "<gtrless>"
    "<gtrsim>" "<gvertneqq>" "<leqq>" "<leqslant>" "<lessapprox>"
    "<lessdot>" "<lesseqdot>" "<lesseqgtr>" "<lesseqqgtr>" "<lessgtr>"
    "<lesssim>" "<lll>" "<llless>" "<lnapprox>" "<lneq>" "<lneqq>"
    "<lnsim>" "<lvertneqq>" "<napprox>" "<ngeq>" "<ngeqq>" "<ngeqslant>"
    "<ngtr>" "<nleq>" "<nleqq>" "<nleqslant>" "<nless>" "<nprec>" "<npreceq>"
    "<nsim>" "<nsimeq>""<ncong>" "<nasymp>" "<nsubset>" "<nsupset>"
    "<nsqsubset>" "<nsqsupset>" "<nsqsubseteq>" "<nsqsupseteq>"
    "<nsubseteq>" "<nsucc>" "<nsucceq>"
    "<nsupseteq>" "<nsupseteqq>" "<precapprox>" "<preccurlyeq>"
    "<npreccurlyeq>" "<precnapprox>" "<precneqq>"
    "<precsim>" "<precnsim>" "<risingdoteq>" "<Subset>"
    "<subseteqq>" "<subsetneq>" "<subsetneqq>" "<succapprox>"
    "<succcurlyeq>" "<nsucccurlyeq>" "<succnapprox>" "<succneqq>"
    "<succsim>" "<succnsim>" "<Supset>" "<supseteqq>"
    "<supsetneq>" "<supsetneqq>"
    "<thickapprox>" "<thicksim>" "<varsubsetneq>" "<varsubsetneqq>"
    "<varsupsetneq>" "<varsupsetneqq>" "<llleq>" "<gggeq>"
    "<subsetplus>" "<supsetplus>"

    "<vartriangleleft>" "<vartriangleright>"
    "<triangleleft>" "<triangleright>"
    "<trianglelefteq>" "<trianglerighteq>" "<trianglelefteqslant>"
    "<trianglerighteqslant>" "<blacktriangleleft>" "<blacktriangleright>"
    "<ntriangleleft>" "<ntriangleright>"
    "<ntrianglelefteq>" "<ntrianglerighteq>"
    "<ntrianglelefteqslant>" "<ntrianglerighteqslant>"

    "<precprec>" "<precpreceq>" "<precprecprec>" "<precprecpreceq>"
    "<succsucc>" "<succsucceq>" "<succsuccsucc>" "<succsuccsucceq>"
    "<nprecprec>" "<nprecpreceq>" "<nprecprecprec>" "<nprecprecpreceq>"
    "<nsuccsucc>" "<nsuccsucceq>" "<nsuccsuccsucc>" "<nsuccsuccsucceq>"
    "<asympasymp>" "<nasympasymp>" "<simsim>" "<nsimsim>" "<nin>" "<nni>"
    "<notin>" "<notni>" "<precdot>" "<preceqdot>" "<dotsucc>" "<dotsucceq>")

  (define Relation-lim-symbol
    (:type infix)
    (:penalty 20)
    (:spacing default default)
    (:limits always)
    "<equallim>" "<longequallim>")

  (define Relation-symbol
    Relation-nolim-symbol Relation-lim-symbol)

  (define Arrow-nolim-symbol
    (:type infix)
    (:penalty 20)
    (:spacing default default)

    "<to>" "<into>" "<from>" "<transtype>" "<leftarrow>" "<rightarrow>"
    "<leftrightarrow>" "<mapsto>" "<mapsfrom>"
    "<hookleftarrow>" "<hookrightarrow>"
    "<longleftarrow>" "<longrightarrow>" "<longleftrightarrow>"
    "<longmapsto>" "<longmapsfrom>"
    "<longhookleftarrow>" "<longhookrightarrow>" "<leftharpoonup>"
    "<leftharpoondown>" "<rightleftharpoons>" "<rightharpoonup>"
    "<rightharpoondown>" "<leadsto>" "<nleadsto>"
    "<nearrow>" "<searrow>" "<swarrow>" "<nwarrow>"
    "<longtwoheadleftarrow>" "<longtwoheadrightarrow>"
    "<leftprec>" "<leftpreceq>" "<succright>" "<succeqright>"

    "<circlearrowleft>" "<circlearrowdown>"
    "<curvearrowleft>" "<curvearrowright>"
    "<downdownarrows>" "<downharpoonleft>" "<downharpoonright>"
    "<leftarrowtail>" "<leftleftarrows>" "<leftrightarrows>"
    "<leftrightharpoons>" "<rightleftharpoons>"
    "<looparrowleft>" "<looparrowright>" "<Lsh>" "<multimap>"
    "<nleftarrow>" "<nleftrightarrow>" "<nrightarrow>" "<restriction>"
    "<rightarrowtail>" "<rightleftarrows>" "<rightrightarrows>"
    "<leftsquigarrow>" "<rightsquigarrow>" "<leftrightsquigarrow>"
    "<Rsh>" "<twoheadleftarrow>" "<twoheadrightarrow>"
    "<upharpoonleft>" "<upharpoonright>" "<upuparrows>"
    "<leftrightmap>" "<pointer>")
  
  (define Arrow-lim-symbol
    (:type infix)
    (:penalty 20)
    (:spacing default default)
    (:limits always)
    "<leftarrowlim>" "<rightarrowlim>" "<leftrightarrowlim>"
    "<longleftarrowlim>" "<longrightarrowlim>" "<longleftrightarrowlim>"
    "<mapstolim>" "<longmapstolim>"
    "<leftsquigarrowlim>" "<rightsquigarrowlim>" "<leftrightsquigarrowlim>")

  (define Arrow-symbol
    Arrow-nolim-symbol Arrow-lim-symbol)

  (define Union-symbol
    (:type infix)
    (:penalty 30)
    (:spacing default default)
    "<cup>" "<Cup>" "<doublecup>" "<uplus>" "<sqcup>")

  (define Intersection-symbol
    (:type infix)
    (:penalty 30)
    (:spacing default default)
    "<cap>" "<Cap>" "<doublecap>" "<sqcap>")

  (define Exclude-symbol
    (:type infix)
    (:penalty 30)
    (:spacing default default)
    "<setminus>" "<smallsetminus>")

  (define Plus-visible-symbol
    (:type infix)
    (:penalty 30)
    (:spacing default default)
    "+" "<amalg>" "<oplus>" "<boxplus>"
    "<dotplus>" "<dotamalg>" "<dotoplus>")

  (define Plus-invisible-symbol
    (:type infix)
    (:penalty invalid)
    (:spacing none default)
    "<noplus>")

  (define Plus-symbol
    Plus-visible-symbol Plus-invisible-symbol)

  (define Plus-prefix-symbol
    (:type prefix)
    (:penalty invalid)
    (:spacing none none)
    "<upl>")

  (define Minus-symbol
    (:type infix)
    (:penalty 30)
    (:spacing default default)
    "-" "<pm>" "<mp>" "<minus>" "<ominus>" "<boxminus>" "<longminus>")

  (define Minus-prefix-symbol
    (:type prefix)
    (:penalty invalid)
    (:spacing none none)
    "<um>" "<upm>" "<ump>")

  (define Times-visible-symbol
    (:type infix)
    (:penalty 40)
    (:spacing default default)
    "<cdot>" "<times>" "<otimes>" "<circ>" "<odot>" "<boxdot>" "<boxtimes>"
    "<dottimes>" "<dototimes>" "<ltimes>" "<rtimes>" "<atimes>" "<btimes>"
    "<exterior>" "<join>" "<ast>" "<star>" "<oast>" "<asterisk>")

  (define Times-invisible-symbol
    (:type infix)
    (:penalty invalid)
    (:spacing half half)
    "*")

  (define Times-symbol
    Times-visible-symbol Times-invisible-symbol)

  (define Over-regular-symbol
    (:type infix)
    (:penalty 40)
    (:spacing default default)
    "<div>" "<oover>")

  (define Over-condensed-symbol
    (:type infix)
    (:penalty 40)
    "/")

  (define Over-symbol
    Over-regular-symbol Over-condensed-symbol)

  (define Power-symbol
    (:type infix)
    (:penalty 50)
    "^")

  (define Index-symbol
    (:type infix)
    (:penalty 50)
    "_")

  (define Big-separator-symbol
    "parallel" "interleave")

  (define Big-or-symbol
    "vee" "curlyvee")

  (define Big-and-symbol
    "wedge" "curlywedge")

  (define Big-union-symbol
    "cup" "sqcup" "amalg" "uplus" "box")

  (define Big-intersection-symbol
    "cap" "sqcap")

  (define Big-sum-symbol
    "int" "oint" "intlim" "ointlim" "sum" "oplus" "triangledown")

  (define Big-product-symbol
    "prod" "otimes" "odot" "triangleup")

  (define Big-operator-symbol
    Big-separator-symbol
    Big-or-symbol Big-and-symbol
    Big-union-symbol Big-intersection-symbol
    Big-sum-symbol Big-product-symbol)

  (define Big-nolim-symbol
    (:type prefix)
    (:penalty panic)
    (:spacing none big)
    "<big-int>" "<big-oint>")

  (define Big-lim-symbol
    (:type prefix)
    (:penalty panic)
    (:spacing none big)
    (:limits display)
    "<big-sum>" "<big-prod>" "<big-amalg>" "<big-intlim>" "<big-ointlim>"
    "<big-cap>" "<big-cup>" "<big-sqcap>" "<big-sqcup>"
    "<big-vee>" "<big-wedge>" "<big-curlyvee>" "<big-curlywedge>"
    "<big-odot>" "<big-otimes>" "<big-oplus>" "<big-uplus>"
    "<big-triangleup>" "<big-triangledown>"
    "<big-box>" "<big-parallel>" "<big-interleave>")

  (define Big-symbol
    Big-nolim-symbol Big-lim-symbol)

  (define Other-prefix-symbol
    (:type prefix)
    (:penalty invalid)
    (:spacing none none)
    "#" "<card>")

  (define Other-postfix-symbol
    (:type postfix)
    (:penalty panic)
    "!" "%" "<permil>" "<postup>" "<postdown>")

  (define Prime-symbol
    (:type symbol)
    (:penalty panic)
    "'" "`" "<dag>" "<ddag>" "<ast>")

  (define Ponctuation-visible-symbol
    (:type separator)
    (:penalty 0)
    (:spacing none default)
    "," ";" ":" "<point>" "<mid>")

  (define Ponctuation-invisible-symbol
    (:type separator)
    (:penalty invalid)
    (:spacing none none)
    "<nocomma>")

  (define Ponctuation-symbol
    Ponctuation-visible-symbol Ponctuation-invisible-symbol)

  (define Open-symbol
    (:type opening-bracket)
    "(" "[" "{"
    "<lvert>" "<lVert>"
    "<lfloor>" "<lceil>" "<langle>"
    "<llbracket>" "<llangle>"
    "<left-(>" "<left-[>" "<left-{>" "<left-less>"
    "<left-}>" "<left-]>" "<left-)>" "<left-gtr>"
    "<left-|>" "<left-||>" "<left-.>"
    "<left-lfloor>" "<left-lceil>" "<left-rfloor>" "<left-rceil>"
    "<left-langle>" "<left-rangle>")
  
  (define Middle-symbol
    (:type middle-bracket)
    "|" "<||>" "<nobracket>" "<mid-|>" "<mid-||>" "<mid-.>")

  (define Close-symbol
    (:type closing-bracket)
    "}" "]" ")"
    "<rvert>" "<rVert>"
    "<rfloor>" "<rceil>" "<rangle>"
    "<rrbracket>" "<rrangle>"
    "<right-(>" "<right-[>" "<right-{>" "<right-less>"
    "<right-}>" "<right-]>" "<right-)>" "<right-gtr>"
    "<right-|>" "<right-||>" "<right-.>"
    "<right-lfloor>" "<right-lceil>" "<right-rfloor>" "<right-rceil>"
    "<right-langle>" "<right-rangle>")

  (define Suspension-nolim-symbol
    (:type symbol)
    (:penalty invalid invalid)
    "<ldots>" "<cdots>" "<udots>" "<vdots>" "<ddots>" "<mdots>" "<colons>")
  
  (define Suspension-lim-symbol
    (:type symbol)
    (:penalty invalid invalid)
    (:limits always)
    "<cdotslim>")

  (define Suspension-symbol
    Suspension-nolim-symbol Suspension-lim-symbol)

  (define Letter-symbol
    (:type symbol)

    "<mathe>" "<mathi>" "<mathpi>"
    "<matheuler>" "<mathcatalan>" "<mathGamma>"
    "<aleph>" "<beth>" "<gimel>" "<daleth>"
    "<hbar>" "<hslash>" "<imath>" "<jmath>"
    "<ell>" "<b-ell>" "<wp>" "<digamma>"

    "<alpha>" "<beta>" "<gamma>" "<delta>" "<epsilon>"
    "<varepsilon>" "<zeta>" "<eta>" "<theta>" "<vartheta>"
    "<iota>" "<kappa>" "<varkappa>" "<lambda>" "<mu>" "<nu>"
    "<xi>" "<omicron>" "<pi>" "<varpi>" "<rho>" "<varrho>"
    "<sigma>" "<varsigma>" "<tau>" "<upsilon>"
    "<phi>" "<varphi>" "<psi>" "<chi>" "<omega>"
    "<backepsilon>" "<mho>"

    "<Alpha>" "<Beta>" "<Gamma>" "<Delta>" "<Epsilon>" "<Zeta>"
    "<Eta>" "<Theta>" "<Iota>" "<Kappa>" "<Lambda>" "<Mu>" "<Nu>"
    "<Xi>" "<Omicron>" "<Pi>" "<Rho>" "<Sigma>" "<Tau>"
    "<Upsilon>" "<Phi>" "<Psi>" "<Chi>" "<Omega>"
    "<Backepsilon>" "<Mho>"

    "<b-alpha>" "<b-beta>" "<b-gamma>" "<b-delta>" "<b-epsilon>"
    "<b-varepsilon>" "<b-zeta>" "<b-eta>" "<b-theta>" "<b-vartheta>"
    "<b-iota>" "<b-kappa>" "<b-varkappa>" "<b-lambda>" "<b-mu>" "<b-nu>"
    "<b-xi>" "<b-omicron>" "<b-pi>" "<b-varpi>" "<b-rho>" "<b-varrho>"
    "<b-sigma>" "<b-varsigma>" "<b-tau>" "<b-upsilon>"
    "<b-phi>" "<b-varphi>" "<b-psi>" "<b-chi>" "<b-omega>"
    "<b-backepsilon>" "<b-mho>"

    "<b-Alpha>" "<b-Beta>" "<b-Gamma>" "<b-Delta>" "<b-Epsilon>"
    "<b-Zeta>" "<b-Eta>" "<b-Theta>" "<b-Iota>" "<b-Kappa>"
    "<b-Lambda>" "<b-Mu>" "<b-Nu>" "<b-Xi>" "<b-Omicron>"
    "<b-Pi>" "<b-Rho>" "<b-Sigma>" "<b-Tau>" "<b-Upsilon>"
    "<b-Phi>" "<b-Psi>" "<b-Chi>" "<b-Omega>"
    "<b-Backepsilon>" "<b-Mho>"

    "<b-a>" "<b-b>" "<b-c>" "<b-d>" "<b-e>" "<b-f>" "<b-g>"
    "<b-h>" "<b-i>" "<b-j>" "<b-k>" "<b-l>" "<b-m>" "<b-n>"
    "<b-o>" "<b-p>" "<b-q>" "<b-r>" "<b-s>" "<b-t>" "<b-u>"
    "<b-v>" "<b-w>" "<b-x>" "<b-y>" "<b-z>"

    "<b-A>" "<b-B>" "<b-C>" "<b-D>" "<b-E>" "<b-F>" "<b-G>"
    "<b-H>" "<b-I>" "<b-J>" "<b-K>" "<b-L>" "<b-M>" "<b-N>"
    "<b-O>" "<b-P>" "<b-Q>" "<b-R>" "<b-S>" "<b-T>" "<b-U>"
    "<b-V>" "<b-W>" "<b-X>" "<b-Y>" "<b-Z>"

    "<b-up-a>" "<b-up-b>" "<b-up-c>" "<b-up-d>" "<b-up-e>" "<b-up-f>"
    "<b-up-g>" "<b-up-h>" "<b-up-i>" "<b-up-j>" "<b-up-k>" "<b-up-l>"
    "<b-up-m>" "<b-up-n>" "<b-up-o>" "<b-up-p>" "<b-up-q>" "<b-up-r>"
    "<b-up-s>" "<b-up-t>" "<b-up-u>" "<b-up-v>" "<b-up-w>" "<b-up-x>"
    "<b-up-y>" "<b-up-z>"

    "<b-up-A>" "<b-up-B>" "<b-up-C>" "<b-up-D>" "<b-up-E>" "<b-up-F>"
    "<b-up-G>" "<b-up-H>" "<b-up-I>" "<b-up-J>" "<b-up-K>" "<b-up-L>"
    "<b-up-M>" "<b-up-N>" "<b-up-O>" "<b-up-P>" "<b-up-Q>" "<b-up-R>"
    "<b-up-S>" "<b-up-T>" "<b-up-U>" "<b-up-V>" "<b-up-W>" "<b-up-X>"
    "<b-up-Y>" "<b-up-Z>"

    "<b-0>" "<b-1>" "<b-2>" "<b-3>" "<b-4>"
    "<b-5>" "<b-6>" "<b-7>" "<b-8>" "<b-9>"
    
    "<cal-a>" "<cal-b>" "<cal-c>" "<cal-d>" "<cal-e>" "<cal-f>" "<cal-g>"
    "<cal-h>" "<cal-i>" "<cal-j>" "<cal-k>" "<cal-l>" "<cal-m>" "<cal-n>"
    "<cal-o>" "<cal-p>" "<cal-q>" "<cal-r>" "<cal-s>" "<cal-t>" "<cal-u>"
    "<cal-v>" "<cal-w>" "<cal-x>" "<cal-y>" "<cal-z>"
    
    "<cal-A>" "<cal-B>" "<cal-C>" "<cal-D>" "<cal-E>" "<cal-F>" "<cal-G>"
    "<cal-H>" "<cal-I>" "<cal-J>" "<cal-K>" "<cal-L>" "<cal-M>" "<cal-N>"
    "<cal-O>" "<cal-P>" "<cal-Q>" "<cal-R>" "<cal-S>" "<cal-T>" "<cal-U>"
    "<cal-V>" "<cal-W>" "<cal-X>" "<cal-Y>" "<cal-Z>"
    
    "<b-cal-a>" "<b-cal-b>" "<b-cal-c>" "<b-cal-d>" "<b-cal-e>" "<b-cal-f>"
    "<b-cal-g>" "<b-cal-h>" "<b-cal-i>" "<b-cal-j>" "<b-cal-k>" "<b-cal-l>"
    "<b-cal-m>" "<b-cal-n>" "<b-cal-o>" "<b-cal-p>" "<b-cal-q>" "<b-cal-r>"
    "<b-cal-s>" "<b-cal-t>" "<b-cal-u>" "<b-cal-v>" "<b-cal-w>" "<b-cal-x>"
    "<b-cal-y>" "<b-cal-z>"

    "<b-cal-A>" "<b-cal-B>" "<b-cal-C>" "<b-cal-D>" "<b-cal-E>" "<b-cal-F>"
    "<b-cal-G>" "<b-cal-H>" "<b-cal-I>" "<b-cal-J>" "<b-cal-K>" "<b-cal-L>"
    "<b-cal-M>" "<b-cal-N>" "<b-cal-O>" "<b-cal-P>" "<b-cal-Q>" "<b-cal-R>"
    "<b-cal-S>" "<b-cal-T>" "<b-cal-U>" "<b-cal-V>" "<b-cal-W>" "<b-cal-X>"
    "<b-cal-Y>" "<b-cal-Z>"

    "<frak-a>" "<frak-b>" "<frak-c>" "<frak-d>" "<frak-e>" "<frak-f>"
    "<frak-g>" "<frak-h>" "<frak-i>" "<frak-j>" "<frak-k>" "<frak-l>"
    "<frak-m>" "<frak-n>" "<frak-o>" "<frak-p>" "<frak-q>" "<frak-r>"
    "<frak-s>" "<frak-t>" "<frak-u>" "<frak-v>" "<frak-w>" "<frak-x>"
    "<frak-y>" "<frak-z>"

    "<frak-A>" "<frak-B>" "<frak-C>" "<frak-D>" "<frak-E>" "<frak-F>"
    "<frak-G>" "<frak-H>" "<frak-I>" "<frak-J>" "<frak-K>" "<frak-L>"
    "<frak-M>" "<frak-N>" "<frak-O>" "<frak-P>" "<frak-Q>" "<frak-R>"
    "<frak-S>" "<frak-T>" "<frak-U>" "<frak-V>" "<frak-W>" "<frak-X>"
    "<frak-Y>" "<frak-Z>"

    "<bbb-a>" "<bbb-b>" "<bbb-c>" "<bbb-d>" "<bbb-e>" "<bbb-f>" "<bbb-g>"
    "<bbb-h>" "<bbb-i>" "<bbb-j>" "<bbb-k>" "<bbb-l>" "<bbb-m>" "<bbb-n>"
    "<bbb-o>" "<bbb-p>" "<bbb-q>" "<bbb-r>" "<bbb-s>" "<bbb-t>" "<bbb-u>"
    "<bbb-v>" "<bbb-w>" "<bbb-x>" "<bbb-y>" "<bbb-z>"

    "<bbb-A>" "<bbb-B>" "<bbb-C>" "<bbb-D>" "<bbb-E>" "<bbb-F>" "<bbb-G>"
    "<bbb-H>" "<bbb-I>" "<bbb-J>" "<bbb-K>" "<bbb-L>" "<bbb-M>" "<bbb-N>"
    "<bbb-O>" "<bbb-P>" "<bbb-Q>" "<bbb-R>" "<bbb-S>" "<bbb-T>" "<bbb-U>"
    "<bbb-V>" "<bbb-W>" "<bbb-X>" "<bbb-Y>" "<bbb-Z>"

    "<bbb-0>" "<bbb-1>" "<bbb-2>" "<bbb-3>" "<bbb-4>"
    "<bbb-5>" "<bbb-6>" "<bbb-7>" "<bbb-8>" "<bbb-9>")
  
  (define Miscellaneous-symbol
    (:type symbol)

    "<ldot>" "<udot>" "<comma>" "<cdummy>" "<nosymbol>"

    "<uparrow>" "<Uparrow>" "<downarrow>" "<Downarrow>"
    "<updownarrow>" "<Updownarrow>" "<mapsup>" "<mapsdown>"
    "<hookuparrow>" "<hookdownarrow>" "<longuparrow>" "<Longuparrow>"
    "<longdownarrow>" "<Longdownarrow>" "<longupdownarrow>" "<Longupdownarrow>"
    "<longmapsup>" "<longmapsdown>" "<longhookuparrow>" "<longhookdownarrow>"

    "<prime>" "<emptyset>"
    "<surd>" "<top>" "<bot>" "<angle>"
    "<flat>" "<natural>" "<sharp>" "<backslash>"
    "<infty>" "<infty>" "<Box>" "<Diamont>"
    "<triangle>" "<clubsuit>" "<diamondsuit>" "<heartsuit>"
    "<spadesuit>" "<diamond>" "<box>" "<bullet>"
    "<eigthnote>" "<quarternote>" "<halfnote>" "<fullnote>" "<twonotes>"
    "<sun>" "<leftmoon>" "<rightmoon>" "<earth>" "<male>" "<female>"
    "<kreuz>" "<recorder>" "<phone>" "<checked>" "<bell>"

    "<backprime>" "<barwedge>" "<because>"
    "<between>" "<bigstar>" "<blacklozenge>"
    "<blacksquare>" "<blacktriangle>" "<blacktriangledown>"
    "<centerdot>" "<checkmark>" "<circledast>" "<circledcirc>"
    "<circleddash>" "<divideontimes>" "<doublebarwedge>"
    "<intercal>" "<leftthreetimes>" "<llcorner>"
    "<lozenge>" "<lrcorner>" "<maltese>" "<measuredangle>"
    "<pitchfork>" "<rightthreetimes>"
    "<smallfrown>" "<smallsmile>" "<sphericalangle>"
    "<square>" "<therefore>" "<thorn>" "<triangledown>"
    "<triangleq>" "<ulcorner>" "<urcorner>" "<varkappa>"
    "<varnothing>" "<vartriangle>" "<veebar>" "<yen>")

  (define Spacing-visible-symbol
    (:type symbol)
    (:spacing default none)
    " ")

  (define Spacing-wide-symbol
    (:type symbol)
    (:spacing big none)
    "<space>")

  (define Spacing-invisible-symbol
    (:type symbol)
    (:penalty invalid)
    (:spacing none none)
    "<nospace>")

  (define Spacing-symbol
    Spacing-visible-symbol Spacing-wide-symbol Spacing-invisible-symbol)

  (define Unary-operator-glyph-symbol
    (:type unary)
    (:penalty invalid)
    (:spacing none none)
    "<mathd>" "<mathD>" "<mathLaplace>" "<partial>" "<nabla>"
    "<Re>" "<Im>" "<complement>"
    "<sum>" "<prod>" "<int>" "<oint>")

  (define Unary-operator-textual-symbol
    (:type unary)
    (:penalty invalid)
    (:spacing none default)
    "arccos" "arcsin" "arctan" "cos" "cosh" "cot" "coth" "csc"
    "deg" "det" "dim" "exp" "gcd" "hom" "ker" "Pr"
    "lg" "ln" "log" "sec" "sin" "sinh" "tan" "tanh")

  (define Unary-operator-symbol
    Unary-operator-glyph-symbol
    Unary-operator-textual-symbol)

  (define Binary-operator-symbol
    (:type binary)
    (:penalty invalid)
    (:spacing none default)
    "div" "mod")

  (define N-ary-operator-symbol
    (:type n-ary)
    (:penalty invalid)
    (:spacing none default)
    (:limits display)
    "inf" "lim" "liminf" "limsup" "max" "min" "sup")

  (define Prefix-symbol
    Not-symbol
    (:<big Big-operator-symbol :>)
    Minus-prefix-symbol
    Plus-prefix-symbol
    Other-prefix-symbol)

  (define Postfix-symbol
    Other-postfix-symbol)

  (define Infix-symbol
    Assign-symbol
    Models-symbol
    Imply-symbol
    Or-symbol
    And-symbol
    Relation-symbol
    Arrow-symbol
    Union-symbol
    Intersection-symbol
    Exclude-symbol
    Plus-symbol
    Minus-symbol
    Times-symbol
    Over-symbol
    Power-symbol)

  (define Reserved-symbol
    :<frac :<sqrt :<wide :<table :<row
    :<around :<around* :<big-around :<left :<mid :<right :<big
    :<lsub :<lsup :<rsub :<rsup :<lprime :<rprime
    :<Prefix :<Postfix))
