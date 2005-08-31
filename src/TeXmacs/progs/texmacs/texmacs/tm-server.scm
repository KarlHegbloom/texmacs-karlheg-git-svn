
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : tm-server.scm
;; DESCRIPTION : server wide properties and resource management
;; COPYRIGHT   : (C) 2001  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (texmacs texmacs tm-server)
  (:use (generic document-edit)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Preferences
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (get-default-look-and-feel)
  (if (os-win32?) "windows" "emacs"))

(define (get-default-interactive-questions)
  (if (== (get-preference "look and feel") "windows") "popups" "footer"))

(define (notify-look-and-feel var val)
  (set-message "Restart in order to let the new look and feel take effect"
	       "configure look and feel"))

(define (notify-language var val)
  (set-output-language val)
  (if (and (has-view?) (== (buffer-tree) (stree->tree '(document ""))))
      (init-language val))
  (cond ((or (== val "bulgarian") (== val "russian") (== val "ukrainian"))
	 (notify-preference "cyrillic input method"))))

(define (notify-security var val)
  (cond ((== val "accept no scripts") (set-script-status 0))
	((== val "prompt on scripts") (set-script-status 1))
	((== val "accept all scripts") (set-script-status 2))))

(define (notify-bibtex-command var val)
  (set-bibtex-command val))

(define-preferences
  ("profile" "beginner" (lambda args (noop)))
  ("look and feel" (get-default-look-and-feel) notify-look-and-feel)
  ("interactive questions" (get-default-interactive-questions) noop)
  ("language" (get-locale-language) notify-language)
  ("security" "prompt on scripts" notify-security)
  ("bibtex command" "bibtex" notify-bibtex-command))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Properties of some built-in routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-property (system cmd)
  (:argument cmd "System command"))

(tm-property (footer-eval cmd)
  (:argument cmd "Scheme command"))

(define (symbol<=? s1 s2)
  (string<=? (symbol->string s1) (symbol->string s2)))

(define (get-function-list)
  (list-sort (map car (ahash-table->list ovl-table)) symbol<=?))

(define (get-interactive-function-list)
  (let* ((funs (get-function-list))
	 (pred? (lambda (fun) (not (not (property fun :arguments))))))
    (list-filter funs pred?)))

(tm-define (exec-interactive-command cmd)
  (:argument  cmd "Interactive command")
  (:proposals cmd (cons "" (map symbol->string
				(get-interactive-function-list))))
  (interactive (eval (string->symbol cmd))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Killing buffers, windows and TeXmacs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (safely-kill-buffer)
  (dialogue
    (if (or (not (buffer-unsaved?))
	    (dialogue-confirm?
	     "The buffer has not been saved. Really close it?" #f))
	(kill-buffer))))

(tm-define (safely-kill-window)
  (if (<= (get-nr-windows) 1)
      (safely-quit-TeXmacs)
      (kill-window)))

(tm-define (safely-quit-TeXmacs)
  (dialogue
    (if (or (not (exists-unsaved-buffer?))
	    (dialogue-confirm?
	     "There are unsaved files. Really quit?" #f))
	(quit-TeXmacs))))
