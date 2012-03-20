
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : files.scm
;; DESCRIPTION : file handling
;; COPYRIGHT   : (C) 2001  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (texmacs texmacs tm-files)
  (:use (texmacs texmacs tm-server) (texmacs texmacs tm-print)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Activation of color highlighting
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (suffix->programming-language s)
  (cond ((== s "scm") "scheme")
	((in? s '("cpp" "hpp" "cc" "hh")) "cpp")
	((in? s '("mmx" "mmh")) "mathemagix")
	(else #f)))

(define (textual-tree? t)
  (or (atomic-tree? t)
      (and (== (tree-label t) 'document)
	   (list-and (map textual-tree? (tree-children t))))))

(define (activate-highlighting)
  (and-let* ((suffix (url-suffix (current-buffer)))
	     (prog-lan (suffix->programming-language suffix)))
    (when (textual-tree? (buffer-tree))
      (init-env "prog-language" prog-lan)
      (init-env "mode" "prog"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Saving
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define current-save-target (url-none))

(define (secure-save-buffer file fm)
  (if (not (url-exists? file))
      (texmacs-save-buffer file fm)
      (user-confirm
	  "File already exists. Overwrite existing file?" #f
	(lambda (answ)
	  (when answ
	    (texmacs-save-buffer file fm)
	    (activate-highlighting))))))

(tm-define (save-buffer . l)
  (if (and (pair? l) (url? (car l)))
      (set! current-save-target (car l)))
  (cond ((= (length l) 0) (save-buffer (current-buffer)))
	((url-scratch? (car l))
	 (choose-file save-buffer "Save TeXmacs file" "texmacs"))
	((= (length l) 1) (texmacs-save-buffer (car l) "generic"))
	(else (secure-save-buffer (car l) (cadr l)))))

(tm-define (export-buffer to)
  ;; Temporary fix for saving to postscript or pdf
  (if (string? to) (set! to (url-relative (buffer-base-url) to)))
  (if (url? to) (set! current-save-target to))
  (if (in? (url-suffix to) '("ps" "pdf"))
      (print-to-file to)
      (texmacs-save-buffer to "generic")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Loading
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (more-recent file suffix1 suffix2)
  (and (url-exists? (url-glue file suffix1))
       (url-exists? (url-glue file suffix2))
       (url-newer? (url-glue file suffix1) (url-glue file suffix2))))

(define (most-recent-suffix file)
  (if (more-recent file "~" "")
      (if (not (more-recent file "#" "")) "~"
          (if (more-recent file "#" "~") "#" "~"))
      (if (more-recent file "#" "") "#" "")))

(define (load-buffer-sub file fm where)
  (let* ((suffix (most-recent-suffix file))
         (question (if (== suffix "#")
                       "Rescue file from crash?"
                       "Load more recent autosave file?")))
    (if (and (!= fm "help")
	     (not (url-rooted-web? file))
	     (!= suffix ""))
	(user-confirm question #t
	  (lambda (answ)
	    (if answ
		(texmacs-load-buffer (url-glue file suffix) fm where #t)
		(texmacs-load-buffer file fm where #f))
	    (activate-highlighting)))
	(begin
	  (texmacs-load-buffer file fm where #f)
	  (activate-highlighting)))))

(tm-define (load-buffer . l)
  (with file (url-append "$TEXMACS_FILE_PATH" (car l))
    (cond ((= (length l) 1)
	   (load-buffer-sub file "generic" 0))
	  ((and (= (length l) 2) (string? (cadr l)))
	   (load-buffer-sub file (cadr l) 0))
	  ((and (= (length l) 2) (integer? (cadr l)))
	   (load-buffer-sub file "generic" (cadr l)))
	  (else (load-buffer-sub file (cadr l) (caddr l))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Autosave
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (delayed-auto-save)
  (let* ((pref (get-preference "autosave"))
	 (len (if (and (string? pref) (integer? (string->number pref)))
		  (* (string->number pref) 1000) 120000)))
    (if (> len 0)
	(delayed
	  (:pause len)
	  (auto-save)))))

(define (notify-autosave var val)
  (if (has-view?) ; delayed-autosave would crash at initialization time
      (delayed-auto-save)))

(define-preferences
  ("autosave" "120" notify-autosave))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Miscellaneous
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (propose-name-buffer)
  (with name (url->string (current-buffer))
    (cond ((not (url-scratch? name)) name)
	  ((os-win32?) "")
	  (else (string-append (var-eval-system "pwd") "/")))))

(tm-property (load-buffer name)
  (:argument name smart-file "File name")
  (:default  name (propose-name-buffer)))

(tm-property (save-buffer name)
  (:argument name texmacs-file "Save as")
  (:default  name (propose-name-buffer)))

(tm-property (choose-file fun text type)
  (:interactive #t))

(tm-define (buffer-loader fm) (lambda (s) (load-buffer s fm)))
(tm-define (buffer-saver fm) (lambda (s) (save-buffer s fm)))
(tm-define (load-in-new-window s) (load-buffer s 1))
(tm-define (load-browse-buffer s) (load-buffer s))

(tm-define (open-buffer)
  (:synopsis "Open a new file")
  (choose-file load-buffer "Load file" ""))

(tm-define (print-buffer)
  (:synopsis "Print the current buffer")
  (print))

(tm-define (interactive-page-setup)
  (:synopsis "Specify the page setup")
  (:interactive #t)
  (set-message "Not yet implemented" "Printer setup"))

(tm-define (interactive-print-buffer)
  (:synopsis "Print the current buffer")
  (:interactive #t)
  (print-to-file "$TEXMACS_HOME_PATH/system/tmp/tmpprint.ps")
  (interactive-print '() "$TEXMACS_HOME_PATH/system/tmp/tmpprint.ps"))

(tm-define (open-auxiliary aux body . opt-master)
  (let* ((name (aux-name aux))
         (master (if (null? opt-master) (buffer-base-url) (car opt-master))))
    (aux-set-document aux body)
    (aux-set-master aux master)
    (switch-to-buffer name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Deprecated functionality
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (set-abbr-buffer name abbr)
  (deprecated-function "set-abbr-buffer" "buffer-set-short-name")
  (buffer-set-short-name (current-buffer) abbr))

(tm-define (get-abbr-buffer name)
  (deprecated-function "get-abbr-buffer" "buffer-get-short-name")
  (buffer-get-short-name (current-buffer)))

(tm-define (set-buffer name doc)
  (deprecated-function "set-buffer" "buffer-set")
  (buffer-set name doc))

(tm-define (set-buffer-tree name doc)
  (deprecated-function "set-buffer-tree" "buffer-set")
  (set-buffer-tree name doc))

(tm-define (get-buffer-tree name)
  (deprecated-function "get-buffer-tree" "buffer-get-body")
  (get-buffer-tree name))

(tm-define (get-name-buffer-path p)
  (deprecated-function "get-name-buffer-path" "path->buffer")
  (path->buffer p))

(tm-define (get-name-buffer)
  (deprecated-function "get-name-buffer" "current-buffer")
  (current-buffer))

(tm-define (set-name-buffer name)
  (deprecated-function "set-name-buffer" "buffer-rename")
  (buffer-rename (current-buffer) name))
