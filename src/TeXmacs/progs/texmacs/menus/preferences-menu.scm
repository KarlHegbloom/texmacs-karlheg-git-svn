
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : preferences-menu.scm
;; DESCRIPTION : the preferences menus
;; COPYRIGHT   : (C) 1999  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (texmacs menus preferences-menu)
  (:use
    (utils edit auto-close)
    (texmacs texmacs tm-server)
    (texmacs texmacs tm-view)
    (texmacs texmacs tm-print)
    (texmacs keyboard config-kbd)
    (convert latex init-latex)
    (language natural)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Preferred scripting language
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-menu (scripts-preferences-menu)
  (let* ((dummy (lazy-plugin-force))
         (l (scripts-list)))
    (for (name l)
      (with menu-name (scripts-name name)
        ((eval menu-name) (set-preference "scripting language" name))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The Preferences menus
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define page-setup-tree
  '((enum ("Preview command" "preview command")
          "default" "ggv" "ghostview" "gv" "kghostview" "open"
          *)
    (enum ("Printing command" "printing command")
          "lpr" "lp" "pdq"
          *)
    (enum ("Paper type" "paper type")
          "A3" "A4" "A5" "B4" "B5" "B6"
          "Letter" "Legal" "Executive"
          *)
    (enum ("Printer dpi" "printer dpi")
          "150" "200" "300" "400" "600" "800" "1200"
          *)
    (enum ("Font type" "font type")
          ("Metafont bitmaps only" "Metafont only")
          ("Metafont and available type 1" "Metafont + Type 1")
          ("Type 1 with metafont fallback" "Type 1 + Metafont")
          ("Type 1 only" "Type 1 only"))))

(tm-define preferences-tree
  `((enum ("Look and feel" "look and feel")
          ("Default" "default")
          ---
          ("Emacs" "emacs")
          ("Gnome" "gnome")
          ("KDE" "kde")
          ("Mac OS" "macos")
          ("Windows" "windows"))
;   (enum ("Profile" "profile")
;         ("Beginner" "beginner")
;         ("Normal" "normal")
;         ("Advanced" "advanced"))
    (enum ("Interactive questions" "interactive questions")
          ("On the footer" "footer")
          ("In popup windows" "popup"))
    (enum ("Details in menus" "detailed menus")
          ("Simplified menus" "simple")
          ("Detailed menus" "detailed"))
    ---
    (enum ("Language" "language")
          ("British" "british")
          ("Bulgarian" "bulgarian")
          ("Chinese" "chinese")
          ("Czech" "czech")
          ("Dutch" "dutch")
          ("Danish" "danish")
          ("English" "english")
          ("Finnish" "finnish")
          ("French" "french")
          ("German" "german")
          ("Hungarian" "hungarian")
          ("Italian" "italian")
          ("Japanese" "japanese")
          ("Korean" "korean")
          ("Polish" "polish")
          ("Portuguese" "portuguese")
          ("Romanian" "romanian")
          ("Russian" "russian")
          ("Slovene" "slovene")
          ("Spanish" "spanish")
          ("Swedish" "swedish")
          ("Taiwanese" "taiwanese")
          ("Ukrainian" "ukrainian"))
    (-> "Keyboard"
        (-> "Remote control"
            (enum ("Left" "ir-left") "pageup" *)
            (enum ("Right" "ir-right") "pagedown" *)
            (enum ("Up" "ir-up") "home" *)
            (enum ("Down" "ir-down") "end" *)
            (enum ("Center" "ir-center") "return" "S-return" *)
            (enum ("Play" "ir-play") "F5" *)
            (enum ("Pause" "ir-pause") "escape" *)
            (enum ("Menu" "ir-menu") "." *))
        (enum ("Cyrillic input method" "cyrillic input method")
              ("Translit" "translit")
              ("Jcuken" "jcuken")
              ("Yawerty" "yawerty")
              ("Koi8-r" "koi8-r")
              ("Cp1251" "cp1251"))
        (enum ("Automatic quotes" "automatic quotes")
              ("Default" "default")
              ---
              ("None" "none")
              ("Dutch" "dutch")
              ("English" "english")
              ("French" "french")
              ("German" "german")
              ("Spanish" "spanish")
              ("Swiss" "swiss"))
        (enum ("Automatic brackets" "automatic brackets")
              ("Disable" "off")
              ("Inside mathematics" "mathematics")
              ("Enable" "on")))
    (-> "Printer" . ,page-setup-tree)
    (enum ("Security" "security")
          ("Accept no scripts" "accept no scripts")
          ("Prompt on scripts" "prompt on scripts")
          ("Accept all scripts" "accept all scripts"))
    (-> "Converters"
        (-> "TeXmacs -> Html"
;           (toggle ("Use CSS" "texmacs->html:css"))
            (toggle ("Use MathML" "texmacs->html:mathml"))
            (toggle ("Export formulas as images" "texmacs->html:images")))
        (-> "LaTeX -> TeXmacs"
            (toggle ("Import sophisticated objects as pictures"
                     "latex->texmacs:fallback-on-pictures"))
            (toggle ("Keep track of the LaTeX source code"
                     "latex->texmacs:preserve-source")))
        (-> "TeXmacs -> LaTeX"
            (toggle ("Replace unrecognized styles"
                     "texmacs->latex:replace-style"))
            (toggle ("Expand unrecognized macros"
                     "texmacs->latex:expand-macros"))
            (toggle ("Expand user-defined macros"
                     "texmacs->latex:expand-user-macros"))
            (toggle ("Export bibliographies as links"
                     "texmacs->latex:indirect-bib"))
            (toggle ("Allow for macro definitions in preamble"
                     "texmacs->latex:use-macros"))
            (enum ("Encoding" "texmacs->latex:encoding")
                  ("Strict Ascii" "ascii")
                  ("Cork charset with TeX catcode definition in preamble" "cork")
                  ("Utf-8 with inputenc LaTeX package" "utf-8")))
        (-> "TeXmacs -> Verbatim"
            (toggle ("Wrap lines"
                     "texmacs->verbatim:wrap"))
            (enum ("Encoding" "texmacs->verbatim:encoding")
                  ("Defined by locale" "auto")
                  ("Cork" "cork")
                  ("Iso-8859-1" "iso-8859-1")
                  ("Utf-8" "utf-8")))
        (-> "Verbatim -> TeXmacs"
            (toggle ("Wrap lines"
                     "verbatim->texmacs:wrap"))
            (enum ("Encoding" "verbatim->texmacs:encoding")
                  ("Automated detection" "auto")
                  ("Cork" "cork")
                  ("Iso-8859-1" "iso-8859-1")
                  ("Utf-8" "utf-8")))
        (-> "TeXmacs -> Image"
            (enum ("Format" "texmacs->graphics:format")
                  ("Svg" "svg")
                  ("Eps" "eps")
                  ("Png" "png"))))
    (-> "Mathematics"
        (-> "Keyboard"
            (item ("Enforce brackets to match" (toggle-matching-brackets)))
            (toggle ("Use extensible brackets" "use large brackets")))
        (-> "Context aids"
            (link context-preferences-menu))
        (-> "Semantics"
            (link semantic-math-preferences-menu))
        (-> "Automatic correction"
            (toggle ("Remove superfluous invisible operators"
                     "remove superfluous invisible"))
            (toggle ("Insert missing invisible operators"
                     "insert missing invisible"))
            (toggle ("Homoglyph substitutions"
                     "homoglyph correct")))
        (-> "Manual correction"
            (toggle ("Remove superfluous invisible operators"
                     "manual remove superfluous invisible"))
            (toggle ("Insert missing invisible operators"
                     "manual insert missing invisible"))
            (toggle ("Homoglyph substitutions"
                     "manual homoglyph correct"))))
    (-> "Scripts"
        ("None" (set-preference "scripting language" "none"))
        ---
        (link scripts-preferences-menu))
    (-> "Tools"
        (toggle ("Debugging tool" "debugging tool"))
        (toggle ("Linking tool" "linking tool"))
        (toggle ("Source macros tool" "source tool"))
        (toggle ("Versioning tool" "versioning tool")))
    ---
    (enum ("Autosave" "autosave")
          ("5 s" "5")
          ("30 s" "30")
          ("120 s" "120")
          ("300 s" "300")
          ---
          ("Disable" "0"))
    (enum ("Bibtex command" "bibtex command")
          "bibtex" "rubibtex" *)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Computation of the preference menu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (id-or-car x)
  (if (string? x) x (car x)))

(define (id-or-cadr x)
  (if (string? x) x (cadr x)))

(define (compute-preferences-entry s)
  `(interactive (lambda (val) (set-preference ,s val))
     ,(upcase-first s)))

(define (compute-preferences-enum s l)
  (cond ((null? l) l)
        ((== (car l) '*)
         (list '--- (list "Other" (compute-preferences-entry s))))
        ((== (car l) '---)
         (cons '--- (compute-preferences-enum s (cdr l))))
        (else (cons (list (id-or-car (car l))
                          `(set-preference ,s ,(id-or-cadr (car l))))
                    (compute-preferences-enum s (cdr l))))))

(tm-define (compute-preferences-menu-sub l)
  (cond ((or (nlist? l) (null? l)) l)
        ((== (car l) 'string)
         (let* ((x (cadr l))
                (s (id-or-car x))
                (v (id-or-cadr x)))
           (list s (compute-preferences-entry v))))
        ((== (car l) 'enum)
         (let* ((x (cadr l))
                (s (id-or-car x))
                (v (id-or-cadr x)))
           (cons* '-> s (compute-preferences-enum v (cddr l)))))
        ((== (car l) 'toggle)
         (let* ((x (cadr l))
                (s (id-or-car x))
                (v (id-or-cadr x)))
           (list s (list 'toggle-preference v))))
        ((== (car l) 'item) (cadr l))
        (else (map-in-order compute-preferences-menu-sub l))))

(tm-menu (compute-preferences-menu l)
  (with r (eval (cons* 'menu-dynamic (compute-preferences-menu-sub l)))
    (dynamic r)))

(tm-menu (page-setup-menu)
  (dynamic (compute-preferences-menu page-setup-tree)))

(tm-menu (preferences-menu)
  (dynamic (compute-preferences-menu preferences-tree)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Appearance preferences
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-preference-names "look and feel"
  ("default" "Default")
  ("emacs" "Emacs")
  ("gnome" "Gnome")
  ("kde" "KDE")
  ("macos" "Mac OS")
  ("windows" "Windows"))

(define-preference-names "language"
  ("british" "British")
  ("bulgarian" "Bulgarian")
  ("chinese" "Chinese")
  ("czech" "Czech")
  ("dutch" "Dutch")
  ("danish" "Danish")
  ("english" "English")
  ("finnish" "Finnish")
  ("french" "French")
  ("german" "German")
  ("hungarian" "Hungarian")
  ("italian" "Italian")
  ("japanese" "Japanese")
  ("korean" "Korean")
  ("polish" "Polish")
  ("portuguese" "Portuguese")
  ("romanian" "Romanian")
  ("russian" "Russian")
  ("slovene" "Slovene")
  ("spanish" "Spanish")
  ("swedish" "Swedish")
  ("taiwanese" "Taiwanese")
  ("ukrainian" "Ukrainian"))

(define-preference-names "interactive questions"
  ("footer" "On the footer")
  ("popup" "In popup windows"))

(define-preference-names "detailed menus"
  ("simple ""Simplified menus")
  ("detailed" "Detailed menus"))

(tm-widget (general-preferences-widget)
  (aligned
    (item (text "Look and feel:")
      (enum (set-pretty-preference "look and feel" answer)
            '("Default" "Emacs" "Gnome" "KDE" "Mac OS" "Windows")
            (get-pretty-preference "look and feel")
            "12em"))
    (item (text "User interface language:")
      (enum (set-pretty-preference "language" answer)
            '("British" "Bulgarian" "Chinese" "Czech" "Dutch" "Danish"
              "English" "Finnish" "French" "German" "Hungarian" "Italian"
              "Japanese" "Korean" "Polish" "Portuguese" "Romanian" "Russian"
              "Slovene" "Spanish" "Swedish" "Taiwanese" "Ukrainian")
            (get-pretty-preference "language")
            "12em"))
    (item (text "Interactive questions:")
      (enum (set-pretty-preference "interactive questions" answer)
            '("On the footer" "In popup windows")
            (get-pretty-preference "interactive questions")
            "12em"))
    (item (text "Details in menus:")
      (enum (set-pretty-preference "detailed menus" answer)
            '("Simplified menus" "Detailed menus")
            (get-pretty-preference "detailed menus")
            "12em"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keyboard preferences
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-preference-names "automatic quotes"
  ("default" "Default")
  ("none" "Disabled")
  ("dutch" "Dutch")
  ("english" "English")
  ("french" "French")
  ("german" "German")
  ("spanish" "Spanish")
  ("swiss" "Swiss"))

(define-preference-names "automatic brackets"
  ("off" "Disabled")
  ("mathematics" "Inside mathematics" "mathematics")
  ("on" "Enabled"))

(define-preference-names "cyrillic input method"
  ("none" "None")
  ("translit" "Translit")
  ("jcuken" "Jcuken")
  ("yawerty" "Yawerty")
  ("koi8-r" "Koi8-r")
  ("cp1251" "Cp1251"))

(tm-widget (keyboard-preferences-widget)
  (aligned
    (item (text "Automatic quotes:")
      (enum (set-pretty-preference "automatic quotes" answer)
            '("Default" "Disabled" "Dutch" "English" "French" "German" "Spanish" "Swiss")
            (get-pretty-preference "automatic quotes")
            "15em"))
    (item (text "Automatic brackets:")
      (enum (set-pretty-preference "automatic brackets" answer)
            '("Disabled" "Enabled" "Inside mathematics")
            (get-pretty-preference "automatic brackets")
            "15em"))
    (item (text "Cyrillic input method:")
      (enum (set-pretty-preference "cyrillic input method" answer)
            '("None" "Translit" "Jcuken" "Yawerty" "Koi8-r" "Cp1251")
            (get-pretty-preference "cyrillic input method")
            "15em")))
  ======
  (bold (text "Remote controllers with keyboard simulation"))
  ===
  (hlist
    (aligned
      (item (text "Left:")
        (enum (set-preference "ir-left" answer) '("pageup" "")
              (get-preference "ir-left") "8em"))
      (item (text "Right:")
        (enum (set-preference "ir-right" answer) '("pagedown" "")
              (get-preference "ir-right") "8em"))
      (item (text "Up:")
        (enum (set-preference "ir-up" answer) '("home" "")
              (get-preference "ir-up") "8em"))
      (item (text "Down:")
        (enum (set-preference "ir-down" answer) '("end" "")
              (get-preference "ir-down") "8em")))
    ///
    (aligned
      (item (text "Center:")
        (enum (set-preference "ir-center" answer) '("return" "S-return" "")
              (get-preference "ir-center") "8em"))
      (item (text "Play:")
        (enum (set-preference "ir-play" answer) '("F5" "")
              (get-preference "ir-play") "8em"))
      (item (text "Pause:")
        (enum (set-preference "ir-pause" answer) '("escape" "")
              (get-preference "ir-pause") "8em"))
      (item (text "Menu:")
        (enum (set-preference "ir-menu" answer) '("." "")
              (get-preference "ir-menu") "8em")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Conversion preferences widget
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Html ----------

(tm-widget (html-preferences-widget)
  ===
  (bold (text "TeXmacs -> Html"))
  ===
  (aligned
    (meti (text "Use CSS for more advanced formatting")
      (toggle (set-boolean-preference "texmacs->html:css" answer)
              (get-boolean-preference "texmacs->html:css")))
    (meti (text "Export mathematical formulas as MathML")
      (toggle (set-boolean-preference "texmacs->html:mathml" answer)
              (get-boolean-preference "texmacs->html:mathml")))
    (meti (text "Export mathematical formulas as images")
      (toggle (set-boolean-preference "texmacs->html:images" answer)
              (get-boolean-preference "texmacs->html:images")))))

;; LaTeX ----------

(define-preference-names "texmacs->latex:encoding"
  ("ascii" "Strict Ascii")
  ("cork" "Cork charset with catcode definitions in preamble")
  ("utf-8" "Utf-8 with inputenc LaTeX package"))

(tm-widget (latex-preferences-widget)
  ===
  (bold (text "LaTeX -> TeXmacs"))
  ===
  (aligned
    (meti (text "Import sophisticated objects as pictures")
      (toggle (set-boolean-preference "latex->texmacs:fallback-on-pictures" answer)
              (get-boolean-preference "latex->texmacs:fallback-on-pictures")))
    (meti (text "Keep track of the LaTeX source code")
      (toggle (set-boolean-preference "latex->texmacs:preserve-source" answer)
              (get-boolean-preference "latex->texmacs:preserve-source"))))
  ======
  (bold (text "TeXmacs -> LaTeX"))
  ===
  (aligned
    (meti (text "Replace TeXmacs styles with no LaTeX equivalents")
      (toggle (set-boolean-preference "texmacs->latex:replace-style" answer)
              (get-boolean-preference "texmacs->latex:replace-style")))
    ;;(meti (text "Expand all TeXmacs macros with no LaTeX equivalents")
    ;;  (toggle (set-boolean-preference "texmacs->latex:expand-macros" answer)
    ;;          (get-boolean-preference "texmacs->latex:expand-macros")))
    (meti (text "Expand all user defined macros")
      (toggle (set-boolean-preference "texmacs->latex:expand-user-macros" answer)
              (get-boolean-preference "texmacs->latex:expand-user-macros")))
    (meti (text "Export bibliographies as links")
      (toggle (set-boolean-preference "texmacs->latex:indirect-bib" answer)
              (get-boolean-preference "texmacs->latex:indirect-bib")))
    (meti (text "Allow for macro definitions in preamble")
      (toggle (set-boolean-preference "texmacs->latex:use-macros" answer)
              (get-boolean-preference "texmacs->latex:use-macros"))))
  ===
  (aligned
    (item (text "Character encoding:")
      (enum (set-pretty-preference "texmacs->latex:encoding" answer)
            '("ascii" "cork" "utf-8")
            (get-pretty-preference "texmacs->latex:encoding")
            "5em"))))

;; BibTeX ----------

(tm-widget (bibtex-preferences-widget)
  ===
  (bold (text "BibTeX -> TeXmacs"))
  ===
  (aligned
    (item (text "BibTeX command:")
      (enum (set-pretty-preference "bibtex command" answer)
            '("bibtex" "rubibtex" "")
            (get-pretty-preference "bibtex command")
            "15em"))))

;; Verbatim ----------

(define-preference-names "texmacs->verbatim:encoding"
  ("auto" "Defined by locale")
  ("cork" "Cork")
  ("iso-8859-1" "Iso-8859-1")
  ("utf-8" "Utf-8"))

(define-preference-names "verbatim->texmacs:encoding"
  ("auto" "Auto")
  ("cork" "Cork")
  ("iso-8859-1" "Iso-8859-1")
  ("utf-8" "Utf-8"))

(tm-widget (verbatim-preferences-widget)
  ===
  (bold (text "TeXmacs -> Verbatim"))
  ===
  (aligned
    (meti (text "Use line wrapping for lines which are longer than 80 characters")
      (toggle (set-boolean-preference "texmacs->verbatim:wrap" answer)
              (get-boolean-preference "texmacs->verbatim:wrap"))))
  ===
  (aligned
    (item (text "Character encoding:")
      (enum (set-pretty-preference "texmacs->verbatim:encoding" answer)
            '("Defined by locale" "Cork" "Iso-8859-1" "Utf-8")
            (get-pretty-preference "texmacs->verbatim:encoding")
            "5em")))
  ======
  (bold (text "Verbatim -> TeXmacs"))
  ===
  (aligned
    (meti (text "Merge lines into paragraphs unless separated by blank lines")
      (toggle (set-boolean-preference "verbatim->texmacs:wrap" answer)
              (get-boolean-preference "verbatim->texmacs:wrap"))))
  ===
  (aligned
    (item (text "Character encoding:")
      (enum (set-pretty-preference "verbatim->texmacs:encoding" answer)
            '("Auto" "Cork" "Iso-8859-1" "Utf-8")
            (get-pretty-preference "verbatim->texmacs:encoding")
            "5em"))))

;; Images ----------

(define-preference-names "texmacs->graphics:format"
  ("svg" "Svg")
  ("eps" "Eps")
  ("png" "Png"))

(tm-widget (image-preferences-widget)
  ===
  (bold (text "TeXmacs -> Image"))
  ===
  (aligned
    (item (text "Format:")
      (enum (set-pretty-preference "texmacs->graphics:format" answer)
            '("Svg" "Eps" "Png")
            (get-pretty-preference "texmacs->graphics:format")
            "5em"))))

;; All converters ----------

(tm-widget (conversion-preferences-widget)
  ======
  (tabs
    (tab (text "Html")
      (centered
        (dynamic (html-preferences-widget))))
    (tab (text "LaTeX")
      (centered
        (dynamic (latex-preferences-widget))))
    (tab (text "BibTeX")
      (centered
        (dynamic (bibtex-preferences-widget))))
    (tab (text "Verbatim")
      (centered
        (dynamic (verbatim-preferences-widget))))
    (tab (text "Image")
      (centered
        (dynamic (image-preferences-widget))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Other
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-preference-names "autosave"
  ("5" "5 sec")
  ("30" "30 sec")
  ("120" "120 sec")
  ("300" "300 sec")
  ("0" "Disable"))

(define-preference-names "security"
  ("accept no scripts" "Accept no scripts")
  ("prompt on scripts" "Prompt on scripts")
  ("accept all scripts" "Accept all scripts"))

(define-preference-names "updater:interval"
  ("0" "Never")
  ("0" "Unsupported")
  ("1" "Every hour")
  ("24" "Once a day")
  ("168" "Once a week")
  ("720" "Once a month"))

(define (updater-last-check-formatted)
  "Time since last update check formatted for use in the preferences dialog"
  (with c (updater-last-check)
    (if (== c 0) 
        "Never"
        (with h (ceiling (/ (- (current-time) c) 3600))
          (cond ((< h 24) (replace "Less than %1 hour(s) ago" h))
                ((< h 720) (replace "%1 days ago" (ceiling (/ h 24))))
                (else (translate "More than 1 month ago")))))))

(define (last-check-string)
  (if (updater-supported?)
      (updater-last-check-formatted)
      "Never (unsupported)"))

(define (automatic-checks-choices)
  (if (updater-supported?)
      '("Never" "Every hour" "Once a day" "Once a week" "Once a month")
      '("Unsupported")))

(tm-define (scripts-preferences-list)
  (lazy-plugin-force)
  (with l (scripts-list)
    (set-preference-name "scripting language" "none" "None")
    (for (x l) (set-preference-name "scripting language" x (scripts-name x)))
    (cons "None" (map scripts-name l))))

(tm-widget (other-preferences-widget)
  (aligned
    (item (text "Automatically save:")
      (enum (set-pretty-preference "autosave" answer)
            '("5 sec" "30 sec" "120 sec" "300 sec" "Disable")
            (get-pretty-preference "autosave")
            "15em"))
    (item (text "Security:")
      (enum (set-pretty-preference "security" answer)
            '("Accept no scripts" "Prompt on scripts" "Accept all scripts")
            (get-pretty-preference "security")
            "15em"))
    (item (text "Scripting language:")
      (enum (set-pretty-preference "scripting language" answer)
            (scripts-preferences-list)
            (get-pretty-preference "scripting language")
            "15em"))
    ;(if (updater-supported?) ; FIXME: (if...) doesn't work here?
        (item (text "Check for automatic updates:")
          (enum (set-pretty-preference "updater:interval" answer)
                (automatic-checks-choices)
                (get-pretty-preference "updater:interval")
                "15em"));)
    ;(if (updater-supported?) ; FIXME: (if...) doesn't work here?
      (item (text "Last check:") (text (last-check-string)))));)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Preferences widget
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-widget (preferences-widget)
  (icon-tabs
    (icon-tab "tm_prefs_general.xpm" (text "General")
      (centered
        (dynamic (general-preferences-widget))))
    (icon-tab "tm_prefs_keyboard.xpm" (text "Keyboard")
      (centered
        (dynamic (keyboard-preferences-widget))))
    (icon-tab "tm_prefs_convert.xpm" (text "Convert")
      (dynamic (conversion-preferences-widget)))
    (icon-tab "tm_prefs_other.xpm" (text "Other")
      (centered
        (dynamic (other-preferences-widget))))))

(tm-define (open-preferences)
  (:interactive #t)
  (top-window preferences-widget "User preferences"))
