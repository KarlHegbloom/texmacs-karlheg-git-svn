
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : format-math-menu.scm
;; DESCRIPTION : menus for altering text properties in math mode
;; COPYRIGHT   : (C) 1999  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (math format-math-menu)
  (:use (generic format-edit)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; What kind of math font are we using?
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (real-math-font? fn)
  (or (== fn "roman") (== fn "concrete")))

(tm-define (real-math-family? fn)
  (or (== fn "mr") (== fn "ms") (== fn "mt")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The Font menu in math mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind math-font-menu
  (-> "Name"
      (if (url-exists-in-tex? "rpsyr.tfm")
	  ("Adobe" (make-with "math-font" "adobe")))
      (if (font-exists-in-tt? "Apple Symbols")
	  ("Apple symbols" (make-with "math-font" "math-apple")))
      (if (font-exists-in-tt? "Asana-Math")
	  ("Asana" (make-with "math-font" "math-asana")))
      (if (url-exists-in-tex? "ccr10.mf")
	  ("Concrete" (make-with "math-font" "concrete")))
      (if (font-exists-in-tt? "DejaVuSerif")
	  ("Dejavu" (make-with "math-font" "math-dejavu"))) 
      (if (font-exists-in-tt? "LucidaGrande")
	  ("Lucida" (make-with "math-font" "math-lucida")))
      (if (url-exists-in-tex? "eurm10.mf")
	  ("New Roman" (make-with "math-font" "ENR")))
      (if (font-exists-in-tt? "texgyrepagella-math")
	  ("Pagella" (make-with "math-font" "math-pagella")))
      ("Roman" (make-with "math-font" "roman"))
      (if (font-exists-in-tt? "STIX-Regular")
	  ("Stix" (make-with "math-font" "math-stix")))
      (if (font-exists-in-tt? "texgyretermes-math")
	  ("Termes" (make-with "math-font" "math-termes")))
      ---
      (if (url-exists-in-tex? "cdr10.mf")
	  ("Duerer" (make-with "math-font" "Duerer")))
      (if (url-exists-in-tex? "eufm10.mf")
	  ("Euler" (make-with "math-font" "Euler")))
      (-> "Calligraphic"
	  ("Default" (make-with "math-font" "cal"))
	  (if (url-exists-in-tex? "euxm10.mf")
	      ("Euler" (make-with "math-font" "cal**")))
	  (if (url-exists-in-tex? "rsfs10.mf")
	      ("Ralph Smith's" (make-with "math-font" "cal*"))))
      (-> "Blackboard bold"
	  ("Default" (make-with "math-font" "Bbb*"))
	  (if (url-exists-in-tex? "msbm10.mf")
	      ("A.M.S." (make-with "math-font" "Bbb")))
	  (if (url-exists-in-tex? "bbold10.mf")
	      ("Blackboard bold" (make-with "math-font" "Bbb**")))
	  (if (url-exists-in-tex? "ocmr10.mf")
	      ("Outlined roman" (make-with "math-font" "Bbb***")))
	  (if (url-exists-in-tex? "dsrom10.mf")
	      ("Double stroke" (make-with "math-font" "Bbb****")))))
  (if (real-math-font? (get-env "math-font"))
      (-> "Variant"
	  ("Roman" (make-with "math-font-family" "mr"))
	  ("Typewriter" (make-with "math-font-family" "mt"))
	  ("Sans serif" (make-with "math-font-family" "ms"))
	  ;;---
	  ;;(-> "Text font"
	  ;;    ("Roman" (make-with "math-font-family" "trm"))
	  ;;    ("Typewriter" (make-with "math-font-family" "ttt"))
	  ;;    ("Sans serif" (make-with "math-font-family" "tss"))
	  ;;    ("Bold" (make-with "math-font-family" "bf"))
	  ;;    ("Right" (make-with "math-font-family" "up"))
	  ;;    ("Slanted" (make-with "math-font-family" "sl"))
	  ;;    ("Italic" (make-with "math-font-family" "it")))
          )
      (if (real-math-family? (get-env "math-font-family"))
	  (-> "Series"
	      ("Light" (make-with "math-font-series" "light"))
	      ("Medium" (make-with "math-font-series" "medium"))
	      ("Bold" (make-with "math-font-series" "bold"))))
      (-> "Shape"
	  ("Normal" (make-with "math-font-shape" "normal"))
	  ("Upight" (make-with "math-font-shape" "right"))))
  (if (not (real-math-font? (get-env "math-font")))
      (-> "Variant"
	  ("Roman" (make-with "math-font-family" "mr"))
	  ("Typewriter" (make-with "math-font-family" "mt"))
	  ("Sans serif" (make-with "math-font-family" "ms")))
      (-> "Series"
	  ("Medium" (make-with "math-font-series" "medium"))
	  ("Bold" (make-with "math-font-series" "bold")))
      (-> "Shape"
	  ("Default" (make-with "math-font-shape" "normal"))
	  ("Right" (make-with "math-font-shape" "right"))
	  ("Slanted" (make-with "math-font-shape" "slanted"))
	  ("Italic" (make-with "math-font-shape" "italic"))))
  (-> "Size" (link font-size-menu)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Special mathematical text properties
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind math-special-format-menu
  (-> "Index level"
      ("Normal" (make-with "math-level" "0"))
      ("Script size" (make-with "math-level" "1"))
      ("Script script size" (make-with "math-level" "2")))
  (-> "Display style"
      ("On" (make-with "math-display" "true"))
      ("Off" (make-with "math-display" "false")))
  (-> "Condensed"
      ("On" (make-with "math-condensed" "true"))
      ("Off" (make-with "math-condensed" "false"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The main Format menu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind math-format-menu
  (if (new-fonts?)
      ("Font" (interactive open-font-selector)))
  (if (not (new-fonts?))
      (group "Font")
      (link math-font-menu))
  (if (simple-menus?)
      (-> "Color" (link color-menu)))
  (if (detailed-menus?)
      ---
      (group "Text")
      (-> "Color" (link color-menu))
      (if (== (get-preference "experimental alpha") "on")
          (-> "Opacity" (link opacity-menu)))
      (-> "Scripts" (link local-supported-scripts-menu))
      (-> "Space" (link horizontal-space-menu))
      (-> "Transform" (link transform-menu))
      (-> "Specific" (link specific-menu)))
  ---
  (group "Special")
  (link math-special-format-menu))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Icons for modifying mathematical text properties
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind math-format-icons
  /
  (=> (balloon (icon "tm_color.xpm") "Select a foreground color")
      (link color-menu))
  (=> (balloon (icon "tm_math_style.xpm")
               "Change the style of mathematical formulas")
      (group "Style")
      ("Small inline" (make-with "math-display" "false"))
      ("Large displayed" (make-with "math-display" "true"))
      ---
      (group "Spacing")
      ("Normal" (make-with "math-condensed" "false"))
      ("Condensed" (make-with "math-condensed" "true"))
      ---
      (group "Size")
      ("Normal" (make-with "math-level" "0"))
      ("Script size" (make-with "math-level" "1"))
      ("Script script size" (make-with "math-level" "2"))))
