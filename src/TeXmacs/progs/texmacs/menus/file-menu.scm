
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : file-menu.scm
;; DESCRIPTION : the file menus
;; COPYRIGHT   : (C) 1999  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (texmacs menus file-menu)
  (:use
    (utils library cursor)
    (texmacs texmacs tm-server)
    (texmacs texmacs tm-files)
    (texmacs texmacs tm-print)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dynamic menu for existing buffers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-menu (buffer-list-menu l)
  (for (name l)
    (let* ((abbr (buffer-get-short-name name))
           (abbr* (if (== abbr "") (url->string (url-tail name)) abbr))
           (mod? (buffer-modified? name))
           (short-name (string-append abbr* (if mod? " *" "")))
           (long-name (url->string name)))
      ((balloon (eval short-name) (eval long-name))
       (switch-to-buffer name)))))

(tm-define (buffer-more-recent? b1 b2)
  (>= (buffer-last-visited b1)
      (buffer-last-visited b2)))

(tm-define (buffer-sorted-list)
  (with l (list-filter (buffer-list) buffer-in-menu?)
    (list-sort l buffer-more-recent?)))

(tm-define (buffer-menu-list aux?)
  (let* ((l1 (list-filter (buffer-list) buffer-in-menu?))
         (l2 (list-filter l1 (lambda (x) (== aux? (aux-buffer? x)))))
         (l3 (list-sort l2 buffer-more-recent?)))
    (sublist l3 0 (min (length l3) 10))))

(tm-define (buffer-same-list)
  (buffer-menu-list (aux-buffer? (this-buffer))))

(tm-define (buffer-other-list)
  (buffer-menu-list (not (aux-buffer? (this-buffer)))))

(tm-define (buffer-same-menu)
  (buffer-list-menu (buffer-same-list)))

(tm-define (buffer-other-menu)
  (buffer-list-menu (buffer-other-list)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dynamic menu for recent files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-menu (file-list-menu l)
  (for (name l)
    (let* ((short-name (url->string (url-tail name))))
      ((balloon (eval short-name) (eval name)) (load-buffer name)))))

(tm-define (recent-file-list nr)
  (with l (map cdar (learned-interactive "recent-buffer"))
    (sublist l 0 (min (length l) nr))))

(tm-define (recent-unloaded-file-list nr)
  (let* ((l1 (map cdar (learned-interactive "recent-buffer")))
         (l2 (map url->string (url->list (get-all-buffers))))
         (dl (list-difference l1 l2)))
    (sublist dl 0 (min (length dl) nr))))

(tm-define (recent-file-menu)
  (file-list-menu (recent-file-list 25)))

(tm-define (recent-unloaded-file-menu)
  (file-list-menu (recent-unloaded-file-list 10)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dynamic menus for formats
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-menu (import-menu flag?)
  (with l (converters-to-special "texmacs-file" "-file" #f)
    (for (fm l)
      (let* ((name (format-get-name fm))
             (load-text (string-append "Load " (string-downcase name) " file"))
             (import-text `(concat "Import " ,name))
             (text (if flag? import-text name))
             (format (if (== fm "verbatim") "" fm)))
        ((eval text) (choose-file (buffer-loader fm) load-text format))))))

(tm-define (import-top-menu) (import-menu #t))
(tm-define (import-import-menu) (import-menu #f))

(tm-menu (export-menu flag?)
  (with l (converters-from-special "texmacs-file" "-file" #f)
    (for (fm l)
      (let* ((name (format-get-name fm))
             (save-text (string-append "Save " (string-downcase name) " file"))
             (export-text `(concat "Export as " ,name))
             (text (if flag? export-text name)))
        ((eval text) (choose-file (buffer-saver fm) save-text fm))))))

(tm-define (export-top-menu) (export-menu #t))
(tm-define (export-export-menu) (export-menu #f))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Submenus of the File menu and for the icon bar
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind new-file-menu
  ("New document" (new-buffer))
  ("Open new window" (open-window))
  ("Clone window" (clone-window)))

(menu-bind load-menu
  ("Load" (open-buffer))
  ("Revert" (revert-buffer))
  ("Load in new window" (choose-file load-in-new-window "Load file" ""))
  ---
  (link import-top-menu)
  (if (nnull? (recent-file-list 1))
      ---
      (link recent-file-menu)))

(menu-bind save-menu
  ("Save" (save-buffer))
  ("Save as" (choose-file save-buffer "Save TeXmacs file" "texmacs"))
  ---
  (link export-top-menu)
  ---
  ((eval '(concat "Export as " "Pdf"))
   (choose-file print-to-file "Save pdf file" "pdf"))
  ((eval '(concat "Export as " "PostScript"))
   (choose-file print-to-file "Save postscript file" "postscript")))

(menu-bind print-menu
  ("Preview" (preview-buffer))
  ---
  ("Print all" (print-buffer))
  ("Print page selection" (interactive print-pages))
  ("Print all to file"
   (choose-file print-to-file "Print all to file" "postscript"))
  ("Print page selection to file"
   (interactive choose-file-and-print-page-selection)))

(menu-bind close-menu
  ("Close document" (safely-kill-buffer))
  ("Close window" (safely-kill-window))
  ("Close TeXmacs" (safely-quit-TeXmacs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The File menu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind file-menu
  ("New" (new-buffer))
  ("Load" (open-buffer))
  ;("Load in new window" (choose-file "Load file" "" 'load-in-new-window))
  ("Revert" (revert-buffer))
  (-> "Recent"
      (link recent-file-menu)
      (if (nnull? (recent-file-list 1)) ---)
      (when (nnull? (recent-file-list 1))
        ("Clear menu" (forget-interactive "recent-buffer"))))
  ---
  ("Save" (save-buffer))
  ("Save as" (choose-file save-buffer "Save TeXmacs file" "texmacs"))
  ---
   (if (experimental-qt-gui?)
       ("Preview" (preview-buffer))
       ("Print" (interactive-print-buffer)))
  (if (not (experimental-qt-gui?))
      (-> "Print" (link print-menu)))
  (-> "Page setup" (link page-setup-menu))
  (-> "Import"
      (link import-import-menu))
  (-> "Export"
      (link export-export-menu)
      ---
      ("Pdf" (choose-file print-to-file "Save pdf file" "pdf"))
      ("Postscript"
       (choose-file print-to-file "Save postscript file" "postscript")))
  ---
  ("Close document" (safely-kill-buffer))
  ("Close TeXmacs" (safely-quit-TeXmacs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The Go menu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind go-menu
  (when (cursor-has-history?)
    ("Back" (cursor-history-backward)))
  (when (cursor-has-future?)
    ("Forward" (cursor-history-forward)))
  ---
  (link buffer-same-menu)
  (if (nnull? (buffer-other-list))
      ---
      (link buffer-other-menu))
  (if (nnull? (recent-unloaded-file-list 1))
      ---
      (link recent-unloaded-file-menu))
  (if (nnull? (bookmarks-menu))
      ---
      (link bookmarks-menu)))
