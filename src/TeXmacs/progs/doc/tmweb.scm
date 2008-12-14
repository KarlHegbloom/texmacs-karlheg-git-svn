
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : tmweb.scm
;; DESCRIPTION : automatic generation of web sites
;; COPYRIGHT   : (C) 2003  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (doc tmweb)
  (:use (texmacs texmacs tm-files)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Building a web site
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tmweb-convert-file tm-file html-file)
  (with-aux tm-file
    (if (url? html-file) (set! current-save-target html-file))
    (system-mkdir (url-append html-file (url-parent)))
    (texmacs-save-buffer html-file "html")))

(define (tmweb-convert-file-dir file tm-dir html-dir)
  (let* ((m? (== (get-preference "texmacs->html:mathml") "on"))
	 (u1 (url-delta (url-append tm-dir "dummy") file))
	 (u2 (url-glue (url-unglue u1 2) (if m? "xhtml" "html")))
	 (u3 (url-append html-dir u2)))
    (system-wait "Converting" (url->string u1))
    (display* "TeXmacs] Converting " (url->string u1) "\n")
    (tmweb-convert-file file u3)))

(define (tmweb-copy-file-dir file tm-dir html-dir)
  (let* ((u1 (url-delta (url-append tm-dir "dummy") file))
	 (u2 (url-append html-dir u1))
	 (name (url->string (url-tail u2)))
	 (dir (url-expand (url-append u2 (url-parent))))
	 (dir-name (url->string (url-tail dir))))
    (when (and (!= dir-name "CVS") (not (string-ends? name "~")))
      (system-wait "Copying" (url->string u1))
      (display* "TeXmacs] Copying " (url->string u1) "\n")
      (system-mkdir dir)
      (system-copy file u2))))

(tm-define (tmweb-convert-dir tm-dir html-dir)
  (let* ((u1 (url-append tm-dir (url-any)))
	 (u2 (url-expand (url-complete u1 "dr")))
	 (u3 (url-append u2 (url-wildcard "*.tm")))
	 (u4 (url-expand (url-complete u3 "fr")))
	 (u5 (url-expand (url-complete u1 "fr"))))
    (if (!= html-dir tm-dir)
	(for-each (lambda (x) (tmweb-copy-file-dir x tm-dir html-dir))
		  (list-difference (url->list u5) (url->list u4))))
    (for-each (lambda (x) (tmweb-convert-file-dir x tm-dir html-dir))
	      (url->list u4))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (tmweb-interactive-build)
  (:interactive #t)
  (dialogue
    (let* ((src (dialogue-url "Source directory" "directory"))
	   (dest (dialogue-url "Destination directory" "directory")))
      (tmweb-convert-dir src dest))))
