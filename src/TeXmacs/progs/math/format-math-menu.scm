
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
      ("Roman" (make-with "math-font" "roman"))
      (if (url-exists-in-tex? "ccr10.mf")
	  ("Concrete" (make-with "math-font" "concrete")))
      (if (url-exists-in-tex? "eurm10.mf")
	  ("New Roman" (make-with "math-font" "ENR")))
      ---
      (if (url-exists-in-tex? "rpsyr.tfm")
	  ("Adobe" (make-with "math-font" "adobe")))
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
	  ---
	  (-> "Text font"
	      ("Roman" (make-with "math-font-family" "trm"))
	      ("Typewriter" (make-with "math-font-family" "ttt"))
	      ("Sans serif" (make-with "math-font-family" "tss"))
	      ("Bold" (make-with "math-font-family" "bf"))
	      ("Right" (make-with "math-font-family" "up"))
	      ("Slanted" (make-with "math-font-family" "sl"))
	      ("Italic" (make-with "math-font-family" "it"))))
      (if (real-math-family? (get-env "math-font-family"))
	  (-> "Series"
	      ("Light" (make-with "math-font-series" "light"))
	      ("Medium" (make-with "math-font-series" "medium"))
	      ("Bold" (make-with "math-font-series" "bold")))))
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
  (group "Font")
  (link math-font-menu)
  (if (simple-menus?)
      (-> "Color" (link color-menu)))
  ---
  (group "Content")
  (if (detailed-menus?)
      (-> "Color" (link color-menu))
      (-> "Scripts" (link local-supported-scripts-menu)))
  (link math-special-format-menu))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Icons for modifying mathematical text properties
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind math-format-icons
  (=> (balloon (icon "tm_greek.xpm") "Insert a greek character")
      (tile 8 (link lower-greek-menu))
      ---
      (tile 8 (link upper-greek-menu)))
  (=> (balloon (icon "tm_mathbold.xpm")
	       "Insert a bold character")
      (tile 15 (link bold-num-menu))
      ---
      (tile 13 (link bold-alpha-menu))
      ---
      (tile 15 (link bold-greek-menu))
      ---
      ("use a bold font" (make-with "math-font-series" "bold")))
  (=> (balloon (icon "tm_cal.xpm")
	       "Insert a calligraphic character")
      (tile 13 (link cal-menu))
      ---
      ("use a calligraphic font" (make-with "math-font" "cal")))
  (=> (balloon (icon "tm_frak.xpm")
	       "Insert a fraktur character")
      (tile 13 (link frak-menu))
      ---
      ("use the fraktur font" (make-with "math-font" "Euler")))
  (=> (balloon (icon "tm_bbb.xpm")
	       "Insert a blackboard bold character")
      (tile 13 (link bbb-menu))
      ---
      ("use the blackboard bold font" (make-with "math-font" "Bbb*")))
  (if (not (in-graphics?))
      (=> (balloon (icon "tm_color.xpm") "Select a foreground color")
	  (link color-menu))))
