
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : menu-help.scm
;; DESCRIPTION : the help menu
;; COPYRIGHT   : (C) 1999  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (menus menu-help)
  (:use (texmacs texmacs tm-help)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The Help menu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind help-menu
  (when (url-exists-in-help? "about/welcome/welcome.en.tm")
	("Welcome" (load-help-buffer "about/welcome/welcome")))
  ---
  (when (url-exists-in-help? "main/config/man-configuration.en.tm")
	(-> "Configuration"
	    ("Browse" (load-help-buffer "main/config/man-configuration"))
	    ---
	    ("Introduction"
	     (load-help-article "main/config/man-intro"))
	    ("Keyboard modifiers"
	     (load-help-article "main/config/man-config-kbd-modkeys"))
	    ("Users of Cyrillic languages"
	     (load-help-article "main/config/man-russian"))))
  (when (url-exists-in-help? "main/man-manual.en.tm")
	(-> "Manual"
	    ("Browse" (load-help-buffer "main/man-manual"))
	    ---
	    ("Getting started"
	     (load-help-article "main/start/man-getting-started"))
	    ("Typing simple texts"
	     (load-help-article "main/text/man-text"))
	    ("Mathematical formulas"
	     (load-help-article "main/math/man-math"))
	    ("Tabular material"
	     (load-help-article "main/table/man-table"))
	    ("Automatically generated content"
	     (load-help-article "main/links/man-links"))
	    ("Editing tools"
	     (load-help-article "main/editing/man-editing-tools"))
	    ("Advanced layout features"
	     (load-help-article "main/layout/man-layout"))
	    ("TeXmacs plugins"
	     (load-help-article "devel/plugin/plugins"))
	    ("TeXmacs as an interface"
	     (load-help-article "main/interface/man-itf"))
	    ("Writing your own style files"
	     (load-help-article "devel/style/style"))
	    ("Customizing TeXmacs"
	     (load-help-article "main/scheme/man-scheme"))
	    ("Compatibility with other formats"
	     (load-help-article "main/convert/man-convert"))))
  (when (url-exists-in-help? "main/man-manual.en.tm")
	(-> "Reference guide"
	    ("The TeXmacs format"
	     (load-help-article "devel/format/basics/basics"))
	    ("TeXmacs primitives"
	     (load-help-article "devel/format/regular/regular"))
	    ("Stylesheet language"
	     (load-help-article "devel/format/stylesheet/stylesheet"))
	    ("Standard TeXmacs styles"
	     (load-help-article "main/styles/styles"))))
  (when (url-exists-in-help? "tutorial/tut-tutorial.en.tm")
	(-> "Tutorial"
	    ("Browse" (load-help-buffer "tutorial/tut-tutorial"))
	    ---
	    ("First contact"
	     (load-help-article "tutorial/start/tut-start"))
	    ("Typing a simple text"
	     (load-help-article "tutorial/start/tut-simple-text"))))
; (when (url-exists-in-help? "devel/style/style.en.tm")
;	(-> "Styles"
;	    ("Browse" (load-help-buffer "devel/style/style"))
;	    ---
;	    ("Introduction"
;	     (load-help-article "devel/style/style-intro"))
;	    ("The standard TeXmacs styles"
;	     (load-help-article "devel/style/standard/standard"))
;	    ("Writing your own style files"
;	     (load-help-article "devel/style/design/design"))
;	    ("Customizing the TeXmacs styles"
;	     (load-help-article "devel/style/customize/customize"))))
  (when (url-exists-in-help? "about/about.en.tm")
	(-> "About"
	    ("Browse" (load-help-buffer "about/about"))
	    ---
	    ("Summary"
	     (load-help-article "about/about-summary"))
	    ("License"
	     (load-help-buffer "$TEXMACS_PATH/LICENSE"))
	    ("Philosophy"
	     (load-help-article "about/philosophy/philosophy"))
	    ("The TeXmacs authors"
	     (load-help-article "about/authors/authors"))
	    ---
	    (when (url-exists-in-help? "about/changes/changes-recent")
		  ("What is new"
		   (load-help-article "about/changes/changes-recent")))
	    ("Major changes"
	     (load-help-article "about/changes/changes-main"))
	    ("Change log"
	     (load-help-article "about/changes/change-log"))
	    ---
	    ("Original welcome message"
	     (load-help-article "about/welcome/first"))))
  ---
  (when (url-exists-in-help? "about/contribute/contribute.en.tm")
	(-> "Help us"
	    ("Browse" (load-help-buffer "about/contribute/contribute"))
	    ---
	    ("Use TeXmacs"
	     (load-help-article "about/contribute/using/using"))
	    ("Making donations"
	     (load-help-article "about/contribute/material/donations"))
	    ("Documentation"
	     (load-help-article
	      "about/contribute/documentation/documentation"))
	    ("Internationalization"
	     (load-help-article "about/contribute/translate/translate"))
	    ("Writing data converters"
	     (load-help-article "about/contribute/converters/converters"))
	    ("Porting TeXmacs to other platforms"
	     (load-help-article "about/contribute/porting/porting"))
	    ("Interfacing TeXmacs with other systems"
	     (load-help-article "about/contribute/interfaces/interfaces"))
	    ("Become a TeXmacs developer"
	     (load-help-article "about/contribute/develop/develop"))))
  (when (url-exists-in-help? "about/projects/projects.en.tm")
	(-> "Projects"
	    ("Browse" (load-help-buffer "about/projects/projects"))
	    ---
	    ("Improving the current implementation"
	     (load-help-buffer "about/projects/improvements"))
	    ("Plans for the future"
	     (load-help-buffer "about/projects/future"))))
; (when (url-exists-in-help? "devel/format/format.en.tm")
;	(-> "Document format"
;	    ("Browse" (load-help-buffer "devel/format/format"))
;	    ---
;	    ("Documents are trees"
;	     (load-help-article "devel/format/trees"))
;	    ("The leaves of TeXmacs trees"
;	     (load-help-article "devel/format/leaves"))
;	    ("The primitive TeXmacs constructs"
;	     (load-help-article "devel/format/primitives"))
;	    ("System environment variables"
;	     (load-help-article "devel/format/env-vars"))
;	    ("Planned changes"
;	     (load-help-article "devel/format/planned-changes"))))
  (when (url-exists-in-help? "devel/interface/interface.en.tm")
	(-> "Interfacing"
	    ("Browse" (load-help-buffer "devel/interface/interface"))
	    ---
	    ("Introduction"
	     (load-help-article "devel/interface/interface-intro"))
	    ("Basic communication using pipes"
	     (load-help-article "devel/interface/interface-pipes"))
	    ("Formatted and structured output"
	     (load-help-article "devel/interface/interface-nested"))
	    ("Prompts and default input"
	     (load-help-article "devel/interface/interface-channels"))
	    ("Sending commands to TeXmacs"
	     (load-help-article "devel/interface/interface-commands"))
	    ("Background evaluations"
	     (load-help-article "devel/interface/interface-background"))
	    ("Mathematical and customized input"
	     (load-help-article "devel/interface/interface-input"))
	    ("Tab-completion"
	     (load-help-article "devel/interface/interface-tab"))
	    ("Dynamic libraries"
	     (load-help-article "devel/interface/interface-dynlibs"))
	    ("Miscellaneous features"
	     (load-help-article "devel/interface/interface-misc"))
	    ("Plans for the future"
	     (load-help-article "devel/interface/interface-plans"))))
  (when (url-exists-in-help? "devel/source/source.en.tm")
	(-> "Source code"
	    ("Browse" (load-help-buffer "devel/source/source"))
	    ---
	    ("General architecture of TeXmacs"
	     (load-help-article "devel/source/architecture"))
	    ("Basic data types"
	     (load-help-article "devel/source/types"))
	    ("Converters to other data formats"
	     (load-help-article "devel/source/conversions"))
	    ("The graphical user interface"
	     (load-help-article "devel/source/gui"))
	    ("TeXmacs fonts"
	     (load-help-article "devel/source/fonts"))
	    ("Mathematical typesetting"
	     (load-help-article "devel/source/maths"))
	    ("The boxes produced by the typesetter"
	     (load-help-article "devel/source/boxes"))))
  ---
  (-> "Search"
      ("Documentation" ...
       (interactive '("Search words in the documentation:") 'docgrep-in-doc))
      ("Source code" ...
       (interactive '("Search words in the source code:") 'docgrep-in-src))
      ("My documents" ...
       (interactive '("Search words in my documents:") 'docgrep-in-texts)))
  (-> "Full manuals"
      (when (url-exists-in-help? "main/man-user-manual.en.tm")
	    ("User manual" (load-help-book "main/man-user-manual")))
      (when (url-exists-in-help? "tutorial/tut-tutorial.en.tm")
	    ("Tutorial" (load-help-book "tutorial/tut-tutorial")))
      (when (url-exists-in-help? "devel/source/source.en.tm")
	    ("Developers guide" (load-help-book "devel/source/source")))
      ---
      (when (style-has? "tmdoc-style")
	    ("Compile article" (tmdoc-expand-this 'tmdoc-title))
	    ("Compile book" (tmdoc-expand-this 'title))))
  (when (url-exists-in-path? "wget")
	(-> "Online help"
	    ("Browse web" (load-help-online "index.en.tm"))
	    ("Update from web" (update-help-online)))))
