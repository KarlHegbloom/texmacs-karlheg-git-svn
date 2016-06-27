;;; coding: utf-8

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : version-tmfs.scm
;; DESCRIPTION : support for external versioning tools
;; COPYRIGHT   : (C) 2012  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (version version-tmfs))

(define version-tool-table (make-ahash-table))
(define version-tool-loaded (make-ahash-table))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dispatch to support for various external tools
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (svn-active? name)
  (let* ((dir (url-head name))
	 (anc (url-append dir (url-ancestor)))
	 (svn (url-append anc ".svn"))
	 (l   (cDr (url->list (url-expand svn)))))
    (list-or (map url-directory? l))))

(tm-define (version-tool name)
  (or (if (ahash-ref version-tool-table name)
          (with tool (ahash-ref version-tool-table name)
            (and (!= tool "") tool))
          (with tool
              (cond ((svn-active? name) "svn")
                    (else ""))
            (ahash-set! version-tool-table name tool)
            (when (and tool (not (ahash-ref version-tool-loaded tool)))
              (ahash-set! version-tool-loaded tool #t)
              (cond ((== tool "svn")
                     (module-provide '(version version-svn)))))
            (and (!= tool "") tool)))
      (and-with base (url-wrap name)
        (and (version-tool base) "wrap"))))

(tm-define (versioned? name)
  (or (nnot (version-tool name))
      (and-with base (url-wrap name)
        (versioned? base))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Getting the main status of a file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (version-status name)
  (if (version-tool name)
      (version-status name)
      "unknown"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File history
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (version-history name) #f)

(tm-define (version-show-history name)
  (cursor-history-add (cursor-path))
  (with s (url->tmfs-string name)
    (revert-buffer (string-append "tmfs://history/" s))))

(tmfs-title-handler (history name doc)
  (with u (tmfs-string->url name)
    (string-append (url->system (url-tail u)) " - History")))

(tm-define (version-revision-url u rev)
  (string-append "tmfs://revision/" rev "/" (url->tmfs-string u)))

(tmfs-load-handler (history name)
  (with u (tmfs-string->url name)
    (with h (version-history u)
      ($generic
        ($tmfs-title "History of "
                     ($link (url->unix u)
                       ($verbatim (url->system (url-tail u)))))
        ($when (not h)
          "This file is not under version control.")
        ($when h
          ($description-long
            ($for (x h)
              ($with (rev by date msg) x
                ($with dest (version-revision-url u rev)
                  ($describe-item
                      ($inline "Version " ($link dest rev)
                               " by " by " on " date)
                    msg))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Showing a particular revision
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (version-revision? u)
  (and (url-rooted-tmfs? u)
       (with (class name) (tmfs-decompose-name u)
         (== class "revision"))))

(tm-define (version-head u)
  (and (url-rooted-tmfs? u)
       (with (class name) (tmfs-decompose-name u)
         (tmfs-string->url (tmfs-cdr name)))))

(tmfs-format-handler (revision name)
  (with u (tmfs-string->url (tmfs-cdr name))
    (url-format u)))

(tmfs-title-handler (revision name doc)
  (let* ((rev (tmfs-car name))
         (u (tmfs-string->url (tmfs-cdr name))))
    (string-append (url->system (url-tail u)) " - Revision " rev)))

(tm-define (version-revision name rev) "")

(tmfs-load-handler (revision name)
  (let* ((u (tmfs-string->url (tmfs-cdr name)))
         (tool (version-tool u)) ;; NOTE: forces lazy loading
         (rev (tmfs-car name)))
    (version-revision u rev)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Updating, registering and committing a file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (version-update name) "file is not under version control")
(tm-define (version-register name) "file is not under version control")
(tm-define (version-commit name comment) "file is not under version control")

(tm-define (update-buffer name)
  (let* ((old-stamp (url-last-modified name))
         (ret1 (version-update name))
         (ret2 (string-replace-tm ret1 "\n" "; "))
         (new-stamp (url-last-modified name)))
    ;;(display* "ret2= " ret2 "\n")
    (when (> new-stamp old-stamp)
      (revert-buffer name))
    (set-message ret2 "Update buffer")))

(tm-define (register-buffer name)
  (let* ((ret1 (version-register name))
         (ret2 (string-replace-tm ret1 "\n" "; ")))
    (set-message ret2 "Register file")))

(tm-define (commit-buffer-message name message)
  (let* ((ret1 (version-commit name message))
         (ret2 (string-replace-tm ret1 "\n" "; "))
         (ret3 (if (!= ret2 "") ret2
                   "The current version has already been committed")))
    (set-message ret3 "Commit file")))

(tm-define (commit-buffer name)
  (interactive (lambda (message) (commit-buffer-message name message))))

(tm-define (version-interactive-update name)
  (:interactive #t)
  (save-buffer name :update))

(tm-define (version-interactive-commit name)
  (:interactive #t)
  (save-buffer name :commit))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Version control for wrapped urls
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (version-status name)
  (:require (== (version-tool name) "wrap"))
  (version-status (url-wrap name)))

(tm-define (version-history name)
  (:require (== (version-tool name) "wrap"))
  (version-history (url-wrap name)))

(tm-define (version-revision name rev)
  (:require (== (version-tool name) "wrap"))
  (version-revision (url-wrap name) rev))

(tm-define (version-update name)
  (:require (== (version-tool name) "wrap"))
  (version-update (url-wrap name)))

(tm-define (version-register name)
  (:require (== (version-tool name) "wrap"))
  (version-register (url-wrap name)))

(tm-define (version-commit name msg)
  (:require (== (version-tool name) "wrap"))
  (version-commit (url-wrap name) msg))

(tm-define (version-revision-url u rev)
  (:require (url-wrap u))
  (version-revision-url (url-wrap u) rev))

(tm-define (version-revision? u)
  (:require (url-wrap u))
  (version-revision? (url-wrap u)))

(tm-define (version-head u)
  (:require (url-wrap u))
  (version-head (url-wrap u)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Specific customizations for parts of documents
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (version-revision-url u rev)
  (:require (url-rooted-tmfs-protocol? u "part"))
  (import-from (part part-tmfs))
  (let* ((name (part-open-name u))
         (m (part-master name))
         (f (part-file name)))
    (part-url (version-revision-url m rev) (version-revision-url f rev))))

(tm-define (version-head u)
  (:require (url-rooted-tmfs-protocol? u "part"))
  (import-from (part part-tmfs))
  (let* ((name (part-open-name u))
         (m (part-master name))
         (f (part-file name)))
    (part-url (version-head m) (version-head f))))
