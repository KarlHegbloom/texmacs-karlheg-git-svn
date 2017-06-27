;;; coding: raw -*- coding-system: raw-text -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : text-kbd.scm
;; DESCRIPTION : keystrokes in text mode
;; COPYRIGHT   : (C) 1999  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-module (text text-kbd)
  :use-module (generic generic-kbd)
  :use-module (utils edit auto-close)
  :use-module (text text-edit))


;;; (To texmacs-dev: correct me if I'm wrong in the following comment please. I
;;; am offline today as I write this. What I'm wondering is what coding will
;;; Guile 2.2 need to read this in as? What will it see inside the quotation
;;; marks?

;;; The right hand side (rhs) values below that will display in Emacs as \nnn
;;; when the coding system is set to raw-text are TeX Cork T1 character to
;;; glyph encoding. That is, the number behind the \ is the T1 character that
;;; maps to the glyph that is displayed when that character is inserted into
;;; the document.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Special symbols in text mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(kbd-map
  (:mode in-text?)
  ("\"" (insert-quote))
  ("\" var" "\"")
  ("- var" (make 'nbhyph))
  ("<" "<less>")
  (">" "<gtr>")
  ("< var" "")
  ("> var" "")
  ("`" "<#2018>")
  ("'" (insert-apostrophe #f))
  ("` var" "`")
  ("' var" (insert-apostrophe #t))

  ("< <" "")
  ("> >" "")
  ("' '" "")
  ("` `" "")
  (", ," "")
  ("- -" "")
  ("- - -" "")
  ("< < var" "<less><less>")
  ("> > var" "<gtr><gtr>")
  ("' ' var" "''")
  ("` ` var" "``")
  ("- - var" "--")
  ("- - - var" "---")

  ("space var" (make 'nbsp))
  ("space var var" (make-space "1em"))
  ("_" "_")
  ("_ var" (make-script #f #t))
  ("^" "^")
  ("^ var" (make-script #t #t))
  ("accent:deadhat var" (make-script #t #t))
  ("sz" "�")

  ("text:symbol s" "�")
  ("text:symbol S" "�")
  ("text:symbol a" "�")
  ("text:symbol a e" "�")
  ("text:symbol o" "�")
  ("text:symbol o e" "�")
  ("text:symbol A" "�")
  ("text:symbol A E" "�")
  ("text:symbol O" "�")
  ("text:symbol O E" "�")
  ("text:symbol !" "�")
  ("text:symbol ?" "�")
  ("text:symbol p" "�")
  ("text:symbol P" "�")
  ("text:symbol m" (make 'masculine))
  ("text:symbol M" (make 'varmasculine))
  ("text:symbol f" (make 'ordfeminine))
  ("text:symbol F" (make 'varordfeminine))

  ("accent:tilde" "~")
  ("accent:tilde space" "~")
  ("accent:tilde A" "�")
  ("accent:tilde N" "�")
  ("accent:tilde O" "�")
  ("accent:tilde a" "�")
  ("accent:tilde n" "�")
  ("accent:tilde o" "�")

  ("accent:hat" "^")
  ("accent:hat space" "^")
  ("accent:hat A" "�")
  ("accent:hat E" "�")
  ("accent:hat I" "�")
  ("accent:hat O" "�")
  ("accent:hat U" "�")
  ("accent:hat a" "�")
  ("accent:hat e" "�")
  ("accent:hat i" "�")
  ("accent:hat o" "�")
  ("accent:hat u" "�")
  ("accent:deadhat" "^")
  ("accent:deadhat space" "^")
  ("accent:deadhat A" "�")
  ("accent:deadhat E" "�")
  ("accent:deadhat I" "�")
  ("accent:deadhat O" "�")
  ("accent:deadhat U" "�")
  ("accent:deadhat a" "�")
  ("accent:deadhat e" "�")
  ("accent:deadhat i" "�")
  ("accent:deadhat o" "�")
  ("accent:deadhat u" "�")

  ("accent:umlaut" "")
  ("accent:umlaut space" "")
  ("accent:umlaut A" "�")
  ("accent:umlaut E" "�")
  ("accent:umlaut I" "�")
  ("accent:umlaut O" "�")
  ("accent:umlaut U" "�")
  ("accent:umlaut Y" "�")
  ("accent:umlaut a" "�")
  ("accent:umlaut e" "�")
  ("accent:umlaut i" "�")
  ("accent:umlaut o" "�")
  ("accent:umlaut u" "�")
  ("accent:umlaut y" "�")

  ("accent:acute" "'")
  ("accent:acute space" "'")
  ("accent:acute A" "�")
  ("accent:acute C" "�")
  ("accent:acute E" "�")
  ("accent:acute I" "�")
  ("accent:acute L" "�")
  ("accent:acute N" "�")
  ("accent:acute O" "�")
  ("accent:acute R" "�")
  ("accent:acute S" "�")
  ("accent:acute U" "�")
  ("accent:acute Y" "�")
  ("accent:acute Z" "�")
  ("accent:acute a" "�")
  ("accent:acute c" "�")
  ("accent:acute e" "�")
  ("accent:acute i" "�")
  ("accent:acute l" "�")
  ("accent:acute n" "�")
  ("accent:acute o" "�")
  ("accent:acute r" "�")
  ("accent:acute s" "�")
  ("accent:acute u" "�")
  ("accent:acute y" "�")
  ("accent:acute z" "�")

  ("accent:grave" "`")
  ("accent:grave space" "`")
  ("accent:grave A" "�")
  ("accent:grave E" "�")
  ("accent:grave I" "�")
  ("accent:grave O" "�")
  ("accent:grave U" "�")
  ("accent:grave a" "�")
  ("accent:grave e" "�")
  ("accent:grave i" "�")
  ("accent:grave o" "�")
  ("accent:grave u" "�")

  ("accent:cedilla" "")
  ("accent:cedilla space" "")
  ("accent:cedilla C" "�")
  ("accent:cedilla S" "�")
  ("accent:cedilla T" "�")
  ("accent:cedilla c" "�")
  ("accent:cedilla s" "�")
  ("accent:cedilla t" "�")

  ("accent:breve" "")
  ("accent:breve space" "")
  ("accent:breve A" "�")
  ("accent:breve G" "�")
  ("accent:breve a" "�")
  ("accent:breve g" "�")

  ("accent:check" "")
  ("accent:check space" "")
  ("accent:check C" "�")
  ("accent:check D" "�")
  ("accent:check E" "�")
  ("accent:check L" "�")
  ("accent:check N" "�")
  ("accent:check R" "�")
  ("accent:check S" "�")
  ("accent:check T" "�")
  ("accent:check U" "�")
  ("accent:check Z" "�")
  ("accent:check c" "�")
  ("accent:check d" "�")
  ("accent:check e" "�")
  ("accent:check l" "�")
  ("accent:check n" "�")
  ("accent:check r" "�")
  ("accent:check s" "�")
  ("accent:check t" "�")
  ("accent:check u" "�")
  ("accent:check z" "�")

  ("accent:doubleacute" "")
  ("accent:doubleacute space" "")
  ("accent:doubleacute O" "�")
  ("accent:doubleacute U" "�")
  ("accent:doubleacute o" "�")
  ("accent:doubleacute u" "�")

  ("accent:abovering" "")
  ("accent:abovering space" "")
  ("accent:abovering A" "�")
  ("accent:abovering U" "�")
  ("accent:abovering a" "�")
  ("accent:abovering u" "�")

  ("accent:abovedot" "
")
  ("accent:abovedot space" "
")
  ("accent:abovedot Z" "�")
  ("accent:abovedot I" "�")
  ("accent:abovedot z" "�")

  ("accent:ogonek" "")
  ("accent:ogonek space" "")
  ("accent:ogonek a" "�")
  ("accent:ogonek A" "�")
  ("accent:ogonek e" "�")
  ("accent:ogonek E" "�")

  ("exclamdown" "�")
  ("cent" (make 'cent))
  ("sterling" "�")
  ("currency" (make 'currency))
  ("yen" (make 'yen))
  ("section" "�")
  ("copyright" (make 'copyright))
  ("copyright var" (make 'copyleft))
  ("guillemotleft" "")
  ("registered" (make 'registered))
  ("degree" (make 'degreesign))
  ("twosuperior" (make 'twosuperior))
  ("threesuperior" (make 'threesuperior))
  ("paragraph" (make 'paragraphsign))
  ("onesuperior" (make 'onesuperior))
  ("guillemotright" "")
  ("onequarter" (make 'onequarter))
  ("onehalf" (make 'onehalf))
  ("threequarters" (make 'threequarters))
  ("questiondown" "�")
  ("euro" (make 'euro))
  ("masculine" (make 'masculine))
  ("ordfeminine" (make 'ordfeminine))
  ("masculine var" (make 'varmasculine))
  ("ordfeminine var" (make 'varordfeminine)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Language dependent shortcuts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(kbd-map
  (:mode in-cyrillic?)
  ("modeswitch"
   (make-with "language" "english")
   (make-with "font" "roman")))

; This breaks � for german keyboards on all systems.
; Maybe it was an old fix for bug #2092 ? It now seems incorrect.
;(kbd-map
;  (:mode in-german?)
;  ("�" "�")
;  ("�" "�"))

(kbd-map
  (:mode in-hungarian?)
  ("text:symbol O" "�")
  ("text:symbol U" "�")
  ("text:symbol o" "�")
  ("text:symbol u" "�")
  ("text:symbol O var" "�")
  ("text:symbol o var" "�"))

(kbd-map
  (:mode in-spanish?)
  ("�" "�")
  ("�" "�")
  ("! var" "�")
  ("? var" "�")
  ("! `" "�")
  ("? `" "�")
  ("! accent:grave" "�")
  ("? accent:grave" "�"))

(kbd-map
  (:mode in-polish?)
  ("text:symbol a" "�")
  ("text:symbol A" "�")
  ("text:symbol c" "�")
  ("text:symbol C" "�")
  ("text:symbol e" "�")
  ("text:symbol E" "�")
  ("text:symbol l" "�")
  ("text:symbol L" "�")
  ("text:symbol n" "�")
  ("text:symbol N" "�")
  ("text:symbol o" "�")
  ("text:symbol O" "�")
  ("text:symbol s" "�")
  ("text:symbol S" "�")
  ("text:symbol x" "�")
  ("text:symbol X" "�")
  ("text:symbol z" "�")
  ("text:symbol Z" "�")
  ("text:symbol a var" "�")
  ("text:symbol A var" "�")
  ("text:symbol o var" "�")
  ("text:symbol O var" "�")
  ("text:symbol s var" "�")
  ("text:symbol S var" "�")
  ("text:symbol z var" "�")
  ("text:symbol Z var" "�"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Greek symbols
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(kbd-map
  ("alpha" "<#3B1>")
  ("beta" "<#3B2>")
  ("gamma" "<#3B3>")
  ("delta" "<#3B4>")
  ("epsilon" "<#3F5>")
  ("varepsilon" "<#3B5>")
  ("zeta" "<#3B6>")
  ("eta" "<#3B7>")
  ("theta" "<#3B8>")
  ("vartheta" "<#3D1>")
  ("iota" "<#3B9>")
  ("kappa" "<#3BA>")
  ("varkappa" "<#3F0>")
  ("lambda" "<#3BB>")
  ("mu" "<#3BC>")
  ("nu" "<#3BD>")
  ("xi" "<#3BE>")
  ("omicron" "<#3BF>")
  ("pi" "<#3C0>")
  ("varpi" "<#3D6>")
  ("rho" "<#3C1>")
  ("varrho" "<#3F1>")
  ("sigma" "<#3C3>")
  ("varsigma" "<#3C2>")
  ("tau" "<#3C4>")
  ("upsilon" "<#3C5>")
  ("phi" "<#3C6>")
  ("varphi" "<#3D5>")
  ("chi" "<#3C7>")
  ("psi" "<#3C8>")
  ("omega" "<#3C9>")
  ("Alpha" "<#391>")
  ("Beta" "<#392>")
  ("Gamma" "<#393>")
  ("Delta" "<#394>")
  ("Epsilon" "<#395>")
  ("Zeta" "<#396>")
  ("Eta" "<#397>")
  ("Theta" "<#398>")
  ("Iota" "<#399>")
  ("Kappa" "<#39A>")
  ("Lambda" "<#39B>")
  ("Mu" "<#39C>")
  ("Nu" "<#39D>")
  ("Xi" "<#39E>")
  ("Omicron" "<#39F>")
  ("Pi" "<#3A0>")
  ("Rho" "<#3A1>")
  ("Sigma" "<#3A3>")
  ("Tau" "<#3A4>")
  ("Upsilon" "<#3A5>")
  ("Phi" "<#3A6>")
  ("Chi" "<#3A7>")
  ("Psi" "<#3A8>")
  ("Omega" "<#3A9>"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Overwrite shortcuts which are inadequate in certain contexts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(kbd-map
  (:mode in-variants-disabled?)
  ("- var" (begin (insert "-") (kbd-tab)))
  ("space var" (begin (insert " ") (kbd-tab)))
  ("space var var" (begin (insert " ") (kbd-tab) (kbd-tab))))

(kbd-map
  (:mode in-verbatim?)
  ("space var" (insert-tabstop))
  ("space var var" (begin (insert-tabstop) (insert-tabstop)))
  ("$" (insert "$"))
  ("$ var" (make 'math))
  ("\\" "\\")
  ("\\ var" (make 'hybrid))
  ("\"" "\"")
  ("`" "`")
  ("` var" "<#2018>")
  ("'" "'")
  ("' var" "<#2019>")
  ("< <" "<less><less>")
  ("> >" "<gtr><gtr>")
  ("' '" "''")
  ("` `" "``")
  ("- -" "--")
  ("- - -" "---")
  ("< < var" "")
  ("> > var" "")
  ("' ' var" "")
  ("` ` var" "")
  (", , var" "")
  ("- - var" "")
  ("- - - var" ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Changing the text format
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(kbd-map
  (:mode in-text?)
  ("font ^" (make-script #t #t))
  ("font hat" (make-script #t #t))
  ("font _" (make-script #f #t))
  ("font s" (make-with "font-family" "ss"))
  ("font t" (make-with "font-family" "tt"))
  ("font b" (make-with "font-series" "bold"))
  ("font m" (make-with "font-series" "medium"))
  ("font r" (make-with "font-shape" "right"))
  ("font i" (make-with "font-shape" "italic"))
  ("font l" (make-with "font-shape" "slanted"))
  ("font o" (make 'overline))
  ("font p" (make-with "font-shape" "small-caps"))
  ("font u" (make 'underline)))

(kbd-map
  (:profile macos)
  (:mode in-text?)
  ("macos {" (make-line-with "par-mode" "left"))
  ("macos |" (make-line-with "par-mode" "center"))
  ("macos }" (make-line-with "par-mode" "right"))
  ("macos C-{" (make-line-with "par-mode" "justify")))

(kbd-map
  (:profile windows)
  (:mode in-text?)
  ("windows 1" (make-line-with "par-line-sep" "0fn"))
  ("windows 2" (make-line-with "par-line-sep" "1fn"))
  ("windows 5" (make-line-with "par-line-sep" "0.5fn"))
  ("windows l" (make-line-with "par-mode" "left"))
  ("windows e" (make-line-with "par-mode" "center"))
  ("windows r" (make-line-with "par-mode" "right"))
  ("windows j" (make-line-with "par-mode" "justify"))
  ("windows t" (make 'indent)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Standard markup in text mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(kbd-map
  (:mode in-std-text?)
  ("text $" (make-equation*))
  ("text &" (make-eqnarray*))

  ("text a" (make 'abbr))
  ("text d" (make-tmlist 'description))
  ("text e" (make-tmlist 'enumerate))
  ("text i" (make-tmlist 'itemize))
  ("text m" (make 'em))
  ("text n" (make 'name))
  ("text p" (make 'samp))
  ("text s" (make 'strong))
  ("text v" (make 'verbatim))
  ("text ;" (make-item))
  ("text 0" (make-section 'chapter))
  ("text 1" (make-section 'section))
  ("text 2" (make-section 'subsection))
  ("text 3" (make-section 'subsubsection))
  ("text 4" (make-section 'paragraph))
  ("text 5" (make-section 'subparagraph))

  ("F5" (make 'em))
  ("F6" (make 'strong))
  ("F7" (make 'verbatim))
  ("F8" (make 'samp))
  ("S-F6" (make 'name)))
