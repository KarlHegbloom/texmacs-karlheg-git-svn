
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : tmtex-preamble.scm
;; DESCRIPTION : automatic generation of TeXmacs specific preamble
;; COPYRIGHT   : (C) 2002  Joris van der Hoeven
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; TODO: find a way to construct drd-tables for each style and
;;       each language using a suitable DRD mechanism
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (convert latex tmtex-preamble)
  (:use (drd latex latex-drd) (convert latex texout))
  (:export tmtex-preamble-build))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Definition of all extra commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tmtex-preamble-def-sub style lan)
  (define (newitemize s item)
    (string-append "\\newenvironment{" s "}\n"
		   "  {\\begin{itemize}"
		   "\\renewcommand{\\labelitemi}{" item "}"
		   "\\renewcommand{\\labelitemii}{" item "}"
		   "\\renewcommand{\\labelitemiii}{" item "}"
		   "\\renewcommand{\\labelitemiv}{" item "}"
		   "}{\\end{itemize}}"))
  (define (newenumerate s item)
    (string-append "\\newenvironment{" s "}"
		   "{\\begin{enumerate}[" item "]}{\\end{enumerate}}"))
  (define (newtheorem s text)
    (string-append "\\newtheorem{" s "}{" (translate text "english" lan) "}"))
  (define (newremark s text)
    (string-append "\\newtheorem{var" s "}{"
		   (translate text "english" lan) "}\n"
		   "\\newenvironment{" s "}{\\begin{var" s
		   "}\\em}{\\em\\end{var" s "}}"))
  (define (newexercise s text)
    (string-append "\\newcounter{" s "nr}\n"
		   "\\setcounter{" s "nr}{0}\n"
		   "\\newenvironment{" s "}{\\medskip\n\n"
		   "  \\refstepcounter{" s "nr}\\small\n"
		   "  {\\bf\\noindent " (translate text "english" lan)
		   "~\\arabic{" s "nr}\\ }}{\\normalsize}"))
  (define (newproof s text)
    (string-append "\\newenvironment{" s "}{\n"
		   "  \\noindent\\textbf{"
		   (translate text "english" lan) "}\\ }{\\hspace*{\\fill}\n"
		   "  \\begin{math}\\Box\\end{math}\\medskip}\n"
		   "\\newenvironment{" s "*}[1]{\n"
		   "  \\noindent\\textbf{#1\\ }}{\\hspace*{\\fill}\n"
		   "  \\begin{math}\\Box\\end{math}\\medskip}"))
  (define (newtmfloat)
    (string-append
      "\\newcommand{\\tmfloatcontents}{}\n"
      "\\newlength{\\tmfloatwidth}\n"
      "\\newcommand{\\tmfloat}[5]{\n"
      "  \\renewcommand{\\tmfloatcontents}{#4}\n"
      "  \\setlength{\\tmfloatwidth}{\\widthof{\\tmfloatcontents}+1in}\n"
      "  \\ifthenelse{\\equal{#2}{small}}\n"
      "    {\\ifthenelse{\\lengthtest{\\tmfloatwidth > \\linewidth}}\n"
      "      {\\setlength{\\tmfloatwidth}{\\linewidth}}{}}\n"
      "    {\\setlength{\\tmfloatwidth}{\\linewidth}}"
      "  \\begin{minipage}[#1]{\\tmfloatwidth}\n"
      "    \\begin{center}\n"
      "      \\tmfloatcontents\n"
      "      \\captionof{#3}{#5}\n"
      "    \\end{center}\n"
      "  \\end{minipage}}"))
  (define (par-mods)
    (string-append
      "\\newenvironment{tmparmod}[3]{%\n"
      " \\begin{list}{}{%\n"
      " \\setlength{\\topsep}{0pt}%\n"
      " \\setlength{\\leftmargin}{#1}%\n"
      " \\setlength{\\rightmargin}{#2}%\n"
      " \\setlength{\\parindent}{#3}%\n"
      " \\setlength{\\listparindent}{\\parindent}%\n"
      " \\setlength{\\itemindent}{\\parindent}%\n"
      " \\setlength{\\parsep}{\\parskip}%\n"
      " }%\n"
      "\\item[]}{\\end{list}}\n"))
  (define (color)
    (string-append
      "\\definecolor{grey}{rgb}{0.75,0.75,0.75}\n"
      "\\definecolor{orange}{rgb}{1.0,0.5,0.5}\n"
      "\\definecolor{brown}{rgb}{0.5,0.25,0.0}\n"
      "\\definecolor{pink}{rgb}{1.0,0.5,0.5}"))
  `(;; itemize and enumerate environments
    (itemizeminus ,(newitemize "itemizeminus" "$-$"))
    (itemizedot ,(newitemize "itemizedot" "$\\bullet$"))
    (itemizearrow ,(newitemize "itemizearrow" "$\\rightarrow$"))
    (enumeratenumeric ,(newenumerate "enumeratenumeric" "1."))
    (enumerateroman ,(newenumerate "enumerateroman" "i."))
    (enumerateromancap ,(newenumerate "enumerateromancap" "I."))
    (enumeratealpha ,(newenumerate "enumeratealpha" "a{\\textup{)}}"))
    (enumeratealphacap ,(newenumerate "enumeratealphacap" "A."))

    ;; theorems
    (theorem ,(newtheorem "theorem" "Theorem"))
    (proposition ,(newtheorem "proposition" "Proposition"))
    (lemma ,(newtheorem "lemma" "Lemma"))
    (corollary ,(newtheorem "corollary" "Corollary"))
    (axiom ,(newtheorem "axiom" "Axiom"))
    (definition ,(newtheorem "definition" "Definition"))
    (remark ,(newremark "remark" "Remark"))
    (note ,(newremark "note" "Note"))
    (example ,(newremark "example" "Example"))
    (convention ,(newremark "convention" "Convention"))
    (warning ,(newremark "warning" "Warning"))
    (exercise ,(newexercise "exercise" "Exercise"))
    (problem ,(newexercise "problem" "Problem"))
    (proof ,(newproof "proof" "Proof"))
    (tmfloat ,(newtmfloat))
    (dueto 1 "\\textup{\\textbf{(#1) }}")

    ;; TeXmacs structural markup
    (TeXmacs 0 "T\\kern-.1667em\\lower.5ex\\hbox{E}\\kern-.125emX\\kern-.1em\\lower.5ex\\hbox{\\textsc{m\\kern-.05ema\\kern-.125emc\\kern-.05ems}}")
    (madebyTeXmacs 0
     ,(string-append
       "\\footnote{"
       (translate "This text has been produced using" "english" lan)
       " GNU T\\kern-.1667em\\lower.5ex\\hbox{E}\\kern-.125emX\\kern-.1em\\lower.5ex\\hbox{\\textsc{m\\kern-.05ema\\kern-.125emc\\kern-.05ems}} ("
       (translate "see" "english" lan)
       "{\\tt http://www.texmacs.org}).}"))
    (tmhlink 2 "{\\color{blue} #1}")
    (tmaction 2 "{\\color{blue} #1}")
    (tmmathbf 1 "\\boldsymbol{#1}")
    (tmop 1 "\\operatorname{#1}")
    (tmbsl 0 "$\\backslash$")
    (tmstrong 1 "\\textbf{#1}")
    (tmem 1 "{\\em #1\\/}")
    (tmtt 1 "\\texttt{#1}")
    (tmname 1 "\\textsc{#1}")
    (tmsamp 1 "\\textsf{#1}")
    (tmabbr 1 "#1")
    (tmdfn 1 "\\textbf{#1}")
    (tmkbd 1 "\\texttt{#1}")
    (tmvar 1 "\\texttt{#1}")
    (tmacronym 1 "\\textsc{#1}")
    (tmperson 1 "\\textsc{#1}")
    (tmdummy 0 "$\\mbox{}$")
    (tmscript 1 "\\text{\\scriptsize $#1$}")
    (tmparmod ,(par-mods))
    (color ,(color))

    ;; other extra markup
    (scheme 0 "{\\sc Scheme}")
    (tmdef 1 "#1")
    (tmref 3 "#1")
    (tmat 0 "\\symbol{\"40}")
    (tmunsc 0 "\\_")
    (emdash 0 "---")
    (pari 0 "{\\sc Pari}")
    (op 1 "#1")
    (keywords 1
      ,(string-append
	"{{\\bf "
	(translate "Keywords: " "english" lan)
	"}#1}"))
    (AMSclass 1
      ,(string-append
	"{{\\bf "
	(translate "A.M.S. subject classification: " "english" lan)
	"}#1}"))

    ;; arrows with limits
    (leftarrowlim 0 "\\mathop{\\leftarrow}\\limits")
    (rightarrowlim 0 "\\mathop{\\rightarrow}\\limits")
    (leftrightarrowlim 0 "\\mathop{\\leftrightarrow}\\limits")
    (mapstolim 0 "\\mathop{\\mapsto}\\limits")
    (longleftarrowlim 0 "\\mathop{\\longleftarrow}\\limits")
    (longrightarrowlim 0 "\\mathop{\\longrightarrow}\\limits")
    (longleftrightarrowlim 0 "\\mathop{\\longleftrightarrow}\\limits")
    (longmapstolim 0 "\\mathop{\\longmapsto}\\limits")
    (equallim 0 "\\mathop{=}\\limits")
    (longequallim 0 "\\mathop{\\longequal}\\limits")
    (Leftarrowlim 0 "\\mathop{\\leftarrow}\\limits")
    (Rightarrowlim 0 "\\mathop{\\rightarrow}\\limits")
    (Leftrightarrowlim 0 "\\mathop{\\leftrightarrow}\\limits")
    (Longleftarrowlim 0 "\\mathop{\\longleftarrow}\\limits")
    (Longrightarrowlim 0 "\\mathop{\\longrightarrow}\\limits")
    (Longleftrightarrowlim 0 "\\mathop{\\longleftrightarrow}\\limits")
   
    ;; asymptotic relations by Joris
    (nasymp 0 "\\not\\asymp")
    (asympasymp 0 "\\asymp\\!\\!\\!-")
    (nasympasymp 0 "\\not\\asymp\\!\\!\\!-")
    (simsim 0 "\\approx\\!\\!\\!-")
    (nsimsim 0 "\\not\\approx\\!\\!\\!-")
    (npreccurlyeq 0 "\\not\\preccurlyeq")
    (precprec 0 "\\prec\\!\\!\\!\\prec")
    (precpreceq 0 "\\preceq\\!\\!\\!\\preceq")
    (precprecprec 0 "\\prec\\!\\!\\!\\prec\\!\\!\\!\\prec")
    (precprecpreceq 0 "\\preceq\\!\\!\\!\\preceq\\!\\!\\!\\preceq")
    (succsucc 0 "\\succ\\!\\!\\!\\succ")
    (succsucceq 0 "\\succeq\\!\\!\\!\\succeq")
    (succsuccsucc 0 "\\succ\\!\\!\\!\\succ\\!\\!\\!\\succ")
    (succsuccsucceq 0 "\\succeq\\!\\!\\!\\succeq\\!\\!\\!\\succeq")

    ;; other extra symbols
    (bigintwl 0 "\\int")
    (bigointwl 0 "\\oint")
    (bignone 0 "")
    (asterisk 0 "*")
    (nonesep 0 "")
    (nin 0 "\\not\\in")
    (nequiv 0 "\\not\\equiv")
    (dotamalg 0 "\\mathaccent95{\\amalg}")
    (dottimes 0 "\\mathaccent95{\\times}")
    (dotoplus 0 "\\mathaccent95{\\oplus}")
    (dototimes 0 "\\mathaccent95{\\otimes}")
    (longequal 0 "{=\\!\\!=}")
    (longhookrightarrow 0 "{\\lhook\\joinrel\\relbar\\joinrel\\rightarrow}")
    (longhookleftarrow 0 "{\\leftarrow\\joinrel\\relbar\\joinrel\\rhook}")
    (triangleup 0 "\\triangle")
    (precdot 0 "{\\prec\\hspace{-0.6em}\\cdot}\\;\\,")
    (preceqdot 0 "{\\preccurlyeq\\hspace{-0.6em}\\cdot}\\;\\,")
    (Iota 0 "\\mathrm{I}")
    (Tau 0 "\\mathrm{T}")
    (join 0 "\\Join")
    (um 0 "-")
    (assign 0 ":=")
    (plusassign 0 "+\\!\\!=")
    (minusassign 0 "-\\!\\!=")
    (timesassign 0 "\times\\!\\!=")
    (overassign 0 "/\\!\\!=")
    (udots 0 "{\\mbox{\\rotatebox[origin=c]{90}{$\\ddots$}}}")
    (Backepsilon 0 "{\\mbox{\\rotatebox[origin=c]{180}{E}}}")
    (mho "\\renewcommand{\\mho}{\\mbox{\\rotatebox[origin=c]{180}{$\\omega$}}}")
    (Mho 0 "{\\mbox{\\rotatebox[origin=c]{180}{$\\Omega$}}}")
    (mathd 0 "\\mathrm{d}")
    (mathe 0 "\\mathrm{e}")
    (mathi 0 "\\mathrm{i}")
    (mathpi 0 "\\pi")
    (lleq 0 "\\leq\\negmedspace\\negmedspace\\leq")
    (llleq 0 "\\leq\\negmedspace\\negmedspace\\leq\\negmedspace\\negmedspace\\leq")
    (ggeq 0 "\\geq\\negmedspace\\negmedspace\\geq")
    (gggeq 0 "\\geq\\negmedspace\\negmedspace\\geq\\negmedspace\\negmedspace\\geq")

    ;; deprecated
    (key 1 "\\fbox{\\rule[-2pt]{0pt}{9pt}#1}")
    (skey 1 "\\fbox{\\rule[-2pt]{0pt}{9pt}shift-#1}")
    (ckey 1 "\\fbox{\\rule[-2pt]{0pt}{9pt}ctrl-#1}")
    (akey 1 "\\fbox{\\rule[-2pt]{0pt}{9pt}alt-#1}")
    (mkey 1 "\\fbox{\\rule[-2pt]{0pt}{9pt}meta-#1}")
    (hkey 1 "\\fbox{\\rule[-2pt]{0pt}{9pt}hyper-#1}")
    (eqnumber 0 ,(string-append "\\hfill(\\theequation"
				"\\addtocounter{equation}{-1}"
				"\\refstepcounter{equation}"
				"\\addtocounter{equation}{1})"))
    (leqnumber 0 ,(string-append "(\\theequation"
				 "\\addtocounter{equation}{-1}"
				 "\\refstepcounter{equation}"
				 "\\addtocounter{equation}{1})\\hfill"))
    (reqnumber 0 ,(string-append "\\hfill(\\theequation"
				 "\\addtocounter{equation}{-1}"
				 "\\refstepcounter{equation}"
				 "\\addtocounter{equation}{1})"))))

(define (tmtex-preamble-def style lan)
  (define (tmsection s inside)
    (string-append "\\newcommand{\\" s "}[1]{"
		   "\\medskip\\bigskip\n\n"
		   "\\noindent\\textbf{" inside "}\\vspace{-3ex}\n\n"
		   "\\noindent}"))
  (define (tmparagraph s inside)
    (string-append "\\newcommand{\\" s "}[1]{\\smallskip\n\n"
		   "\\noindent\\textbf{" inside "} }"))

  (let ((l (tmtex-preamble-def-sub style lan)))
    (define tex-preamble-letter-article-extra
      `((chapter ,(tmsection "chapter"
			     "\\begin{center}\\huge #1\\end{center}"))))
    (define tex-preamble-letter-extra
      `((appendix "\\newcommand{\\appendix}{}")
	(section ,(tmsection "section" "\\LARGE #1"))
	(subsection ,(tmsection "subsection" "\\Large #1"))
	(subsubsection ,(tmsection "subsubsection" "\\large #1"))
	(paragraph ,(tmparagraph "paragraph" "#1"))
	(subparagraph ,(tmparagraph "subparagraph" "#1"))))

    (if (in? style '("letter" "article"))
	(set! l (append tex-preamble-letter-article-extra l)))
    (if (== style "letter")
	(set! l (append tex-preamble-letter-extra l)))
    l))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Computation of the dictionary
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tmtex-preamble-rewrite l)
  (if (= (length l) 3)
      (let ((s (car l)) (n (cadr l)) (r (caddr l)))
	(list s (string-append "\\newcommand{\\" (symbol->string s) "}"
			       (if (= n 0) ""
				   (string-append "[" (number->string n) "]"))
			       "{" r "}")))
      l))

(define (tmtex-preamble-table style lan)
  (let* ((l (tmtex-preamble-def style lan))
	 (r (map-in-order tmtex-preamble-rewrite l))
	 (t (make-ahash-table)))
    (fill-dictionary t r)
    t))

(define (tmtex-preamble-show-all style lan)
  (let* ((l (tmtex-preamble-def style lan))
	 (r (map-in-order tmtex-preamble-rewrite l)))
    (for-each (lambda (l) (display* (cadr l) "\n")) r)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Language specific stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(drd-table tmtex-preamble-language-def%
  ("czech" "\\usepackage[czech]{babel}")
  ("danish" "\\usepackage[danish]{babel}")
  ("dutch" "\\usepackage[dutch]{babel}")
  ("finnish" "\\usepackage[finnish]{babel}")
  ("french" "\\usepackage[french]{babel}")
  ("german" "\\usepackage[german]{babel}")
  ("hungarian" "\\usepackage[hungarian]{babel}")
  ("italian" "\\usepackage[italian]{babel}")
  ("polish" "\\usepackage[polish]{babel}")
  ("portuguese" "\\usepackage[portuges]{babel}")
  ("romanian" "\\usepackage[romanian]{babel}")
  ("russian" "\\usepackage[cp1251]{inputenc}\n\\usepackage[russian]{babel}")
  ("slovene" "\\usepackage[slovene]{babel}")
  ("spanish" "\\usepackage[spanish]{babel}")
  ("swedish" "\\usepackage[swedish]{babel}")
  ("ukrainian"
   "\\usepackage[cp1251]{inputenc}\n\\usepackage[ukrainian]{babel}"))

(define tmtex-preamble-iso-latin
  "\\catcode`\\�=\\active \\def�{\\`a} \\catcode`\\�=\\active \\def�{\\`A}
\\catcode`\\�=\\active \\def�{\\'a} \\catcode`\\�=\\active \\def�{\\'A}
\\catcode`\\�=\\active \\def�{\\\"a} \\catcode`\\�=\\active \\def�{\\\"A}
\\catcode`\\�=\\active \\def�{\\^a} \\catcode`\\�=\\active \\def�{\\^A}
\\catcode`\\�=\\active \\def�{{\\aa}} \\catcode`\\�=\\active \\def�{{\\AA}}
\\catcode`\\�=\\active \\def�{\\c{c}} \\catcode`\\�=\\active \\def�{\\c{C}}
\\catcode`\\�=\\active \\def�{\\`e} \\catcode`\\�=\\active \\def�{\\`E}
\\catcode`\\�=\\active \\def�{\\'e} \\catcode`\\�=\\active \\def�{\\'E}
\\catcode`\\�=\\active \\def�{\\\"e} \\catcode`\\�=\\active \\def�{\\\"E}
\\catcode`\\�=\\active \\def�{\\^e} \\catcode`\\�=\\active \\def�{\\^E}
\\catcode`\\�=\\active \\def�{\\`{\\i}} \\catcode`\\�=\\active \\def�{\\`{\\I}}
\\catcode`\\�=\\active \\def�{\\'{\\i}} \\catcode`\\�=\\active \\def�{\\'{\\I}}
\\catcode`\\�=\\active \\def�{\\\"{\\i}} \\catcode`\\�=\\active \\def�{\\\"{\\I}}
\\catcode`\\�=\\active \\def�{\\^{\\i}} \\catcode`\\�=\\active \\def�{\\^{\\I}}
\\catcode`\\�=\\active \\def�{\\`o} \\catcode`\\�=\\active \\def�{\\`O}
\\catcode`\\�=\\active \\def�{\\'o} \\catcode`\\�=\\active \\def�{\\'O}
\\catcode`\\�=\\active \\def�{\\\"o} \\catcode`\\�=\\active \\def�{\\\"O}
\\catcode`\\�=\\active \\def�{\\^o} \\catcode`\\�=\\active \\def�{\\^O}
\\catcode`\\�=\\active \\def�{\\`u} \\catcode`\\�=\\active \\def�{\\`U}
\\catcode`\\�=\\active \\def�{\\'u} \\catcode`\\�=\\active \\def�{\\'U}
\\catcode`\\�=\\active \\def�{\\\"u} \\catcode`\\�=\\active \\def�{\\\"U}
\\catcode`\\�=\\active \\def�{\\^u} \\catcode`\\�=\\active \\def�{\\^U}
\\catcode`\\�=\\active \\def�{\\'y} \\catcode`\\�=\\active \\def�{\\'Y}
\\catcode`\\�=\\active \\def�{\\\"y} \\catcode`\\�=\\active \\def�{\\\"Y}
\\catcode`\\�=\\active \\def�{!`}
\\catcode`\\�=\\active \\def�{?`}
\\catcode`\\�=\\active \\def�{{\\ss}}
")

(define tmtex-preamble-cyrillic
  "\\catcode`\\�=\\active \\def�{\\CYRA} \\catcode`\\�=\\active \\def�{\\cyra}
\\catcode`\\�=\\active \\def�{\\CYRB} \\catcode`\\�=\\active \\def�{\\cyrb}
\\catcode`\\�=\\active \\def�{\\CYRV} \\catcode`\\�=\\active \\def�{\\cyrv}
\\catcode`\\�=\\active \\def�{\\CYRG} \\catcode`\\�=\\active \\def�{\\cyrg}
\\catcode`\\�=\\active \\def�{\\CYRD} \\catcode`\\�=\\active \\def�{\\cyrd}
\\catcode`\\�=\\active \\def�{\\CYRE} \\catcode`\\�=\\active \\def�{\\cyre}
\\catcode`\\�=\\active \\def�{\\CYRZH} \\catcode`\\�=\\active \\def�{\\cyrzh}
\\catcode`\\�=\\active \\def�{\\CYRZ} \\catcode`\\�=\\active \\def�{\\cyrz}
\\catcode`\\�=\\active \\def�{\\CYRI} \\catcode`\\�=\\active \\def�{\\cyri}
\\catcode`\\�=\\active \\def�{\\CYRISHRT} \\catcode`\\�=\\active \\def�{\\cyrishrt}
\\catcode`\\�=\\active \\def�{\\CYRK} \\catcode`\\�=\\active \\def�{\\cyrk}
\\catcode`\\�=\\active \\def�{\\CYRL} \\catcode`\\�=\\active \\def�{\\cyrl}
\\catcode`\\�=\\active \\def�{\\CYRM} \\catcode`\\�=\\active \\def�{\\cyrm}
\\catcode`\\�=\\active \\def�{\\CYRN} \\catcode`\\�=\\active \\def�{\\cyrn}
\\catcode`\\�=\\active \\def�{\\CYRO} \\catcode`\\�=\\active \\def�{\\cyro}
\\catcode`\\�=\\active \\def�{\\CYRP} \\catcode`\\�=\\active \\def�{\\cyrp}
\\catcode`\\�=\\active \\def�{\\CYRR} \\catcode`\\�=\\active \\def�{\\cyrr}
\\catcode`\\�=\\active \\def�{\\CYRS} \\catcode`\\�=\\active \\def�{\\cyrs}
\\catcode`\\�=\\active \\def�{\\CYRT} \\catcode`\\�=\\active \\def�{\\cyrt}
\\catcode`\\�=\\active \\def�{\\CYRU} \\catcode`\\�=\\active \\def�{\\cyru}
\\catcode`\\�=\\active \\def�{\\CYRF} \\catcode`\\�=\\active \\def�{\\cyrf}
\\catcode`\\�=\\active \\def�{\\CYRH} \\catcode`\\�=\\active \\def�{\\cyrh}
\\catcode`\\�=\\active \\def�{\\CYRC} \\catcode`\\�=\\active \\def�{\\cyrc}
\\catcode`\\�=\\active \\def�{\\CYRCH} \\catcode`\\�=\\active \\def�{\\cyrch}
\\catcode`\\�=\\active \\def�{\\CYRSH} \\catcode`\\�=\\active \\def�{\\cyrsh}
\\catcode`\\�=\\active \\def�{\\CYRSHCH} \\catcode`\\�=\\active \\def�{\\cyrshch}
\\catcode`\\�=\\active \\def�{\\CYRHRDSN} \\catcode`\\�=\\active \\def�{\\cyrhrdsn}
\\catcode`\\�=\\active \\def�{\\CYRERY} \\catcode`\\�=\\active \\def�{\\cyrery}
\\catcode`\\�=\\active \\def�{\\CYRSFTSN} \\catcode`\\�=\\active \\def�{\\cyrsftsn}
\\catcode`\\�=\\active \\def�{\\CYREREV} \\catcode`\\�=\\active \\def�{\\cyrerev}
\\catcode`\\�=\\active \\def�{\\CYRYU} \\catcode`\\�=\\active \\def�{\\cyryu}
\\catcode`\\�=\\active \\def�{\\CYRYA} \\catcode`\\�=\\active \\def�{\\cyrya}
\\catcode`\\�=\\active \\def�{\\CYRYO} \\catcode`\\�=\\active \\def�{\\cyryo}
")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Building the preamble
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define tmtex-preamble-dic (make-ahash-table))
(define tmtex-preamble-done (make-ahash-table))
(define tmtex-preamble-uses (make-ahash-table))
(define tmtex-preamble-result "")
(define tmtex-preamble-hichar-flag #f)

(define (tmtex-preamble-test-insert s)
  (if (and (ahash-ref tmtex-preamble-dic s)
	   (not (ahash-ref tmtex-preamble-done s)))
      (begin
	(ahash-set! tmtex-preamble-done s #t)
	(set! tmtex-preamble-result
	      (string-append tmtex-preamble-result
			     (ahash-ref tmtex-preamble-dic s)
			     "\n"))))
  (with packlist (drd-ref-list latex-needs% s)
    (if packlist
	(for-each 
	  (lambda (pack)
	    (if (not (ahash-ref tmtex-preamble-uses pack))
		(ahash-set! tmtex-preamble-uses pack #t)))
          packlist))))

(define (tmtex-preamble-test-hichar c)
  (if (>= (char->integer c) 128)
      (set! tmtex-preamble-hichar-flag #t)))

(define (tmtex-preamble-build-sub l)
  (if (and (list? l) (not (null? l)))
      (let ((x (car l)))
	(if (symbol? x) (tmtex-preamble-test-insert x))
	(if (and (list? x) (>= (length l) 2) (== (car x) '!begin))
	    (tmtex-preamble-test-insert (string->symbol (cadr x))))
	(if (and (in? x '(!sub !sup)) (texout-contains-table? (cadr l)))
	    (tmtex-preamble-test-insert 'tmscript))
	(for-each tmtex-preamble-build-sub (cdr l)))
      (if (string? l)
	  (for-each tmtex-preamble-test-hichar (string->list l)))))

(define (tmtex-preamble-make-package-list l)
  (cond ((null? l) "")
        ((null? (cdr l)) (force-string (car l)))
        (else (string-append (force-string (car l)) ","
          (tmtex-preamble-make-package-list (cdr l))))))

(define (tmtex-preamble-build text style lan)
  (set! tmtex-preamble-dic (tmtex-preamble-table style lan))
  (set! tmtex-preamble-done (make-ahash-table))
  (set! tmtex-preamble-uses (make-ahash-table))
  (set! tmtex-preamble-result "")
  (set! tmtex-preamble-hichar-flag #f)
  (if (drd-ref tmtex-preamble-language% lan)
      (set! tmtex-preamble-result
	    (string-append (drd-ref tmtex-preamble-language% lan) "\n")))
  (tmtex-preamble-build-sub text)
  (if tmtex-preamble-hichar-flag
      (let ((extra (if (in? lan '("russian" "ukrainian"))
		       tmtex-preamble-cyrillic
		       tmtex-preamble-iso-latin)))
	(set! tmtex-preamble-result
	      (string-append extra tmtex-preamble-result))))
  (values
    (tmtex-preamble-make-package-list 
      (sort
	(map car (ahash-table->list tmtex-preamble-uses))
	(lambda (l r)
	  (let* ((tl (drd-ref latex-package-priority% l))
		 (tr (drd-ref latex-package-priority% r))
		 (vl (if tl tl 999999))
		 (vr (if tr tr 999999)))
		(< vl vr)))))
    tmtex-preamble-result))
