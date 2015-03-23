
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : server-tmfs.scm
;; DESCRIPTION : Remote file system, server side
;; COPYRIGHT   : (C) 2013  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (server server-tmfs)
  (:use (server server-base)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Repository
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define repo (url-concretize "$TEXMACS_HOME_PATH/server"))
(define repo-seed-val (+ (* 4294967296 (abs (texmacs-time)))))
(define repo-seed (seed->random-state repo-seed-val))

(define (repository-add-into dir name)
  (when (not (url-exists? dir))
    (system-mkdir dir))
  (with rdir (string-append dir "/_")
    (if (not (url-exists? rdir))
        (begin
          (system-mkdir rdir)
          (string-append rdir "/" name))
        (with sub (number->string (random 10 repo-seed))
          (repository-add-into (string-append dir "/" sub) name)))))

(define (repository-add rid suffix)
  (let* ((name (if (== suffix "") rid (string-append rid "." suffix)))
         (full (repository-add-into repo name))
         (tail (substring full (+ (string-length repo) 1)
                               (string-length full))))
    (db-set rid "location" (list tail))
    name))

(define (repository-get rid)
  (and rid
       (with l (db-field-get rid "location")
         (and (pair? l) (string-append repo "/" (car l))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File hierarchy
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (safe-car l) (and (pair? l) (car l)))

(define (search-file l . where)
  (if (null? l) where
      (let* ((q (if (null? where)
                    (list "type" "dir")
                    (list "dir" (car where))))
             (matches (db-search (list (list "name" (car l)) q))))
        (append-map (cut search-file (cdr l) <>) matches))))

(define (dir-contents dir)
  (db-search (list (list "dir" dir))))

(define (file-name->resource name)
  (safe-car (search-file (tmfs->list name))))

(define (resource->file-name rid)
  (let* ((dir (db-field-get-first rid "dir" #f))
         (name (db-field-get-first rid "name" "?")))
    (if dir (string-append (resource->file-name dir) "/" name) name)))

(define (inheritance-reserved-attributes)
  (append (db-reserved-attributes)
          (list "name")))

(define (inherit-property? x)
  (nin? (car x) (inheritance-reserved-attributes)))

(define (inherit-properties derived-rid base-rid)
  (let* ((props1 (db-get-all base-rid))
         (props2 (list-filter props1 inherit-property?)))
    (for (prop props2)
      (db-set derived-rid (car prop) (cdr prop)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Remote file manipulations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-service (remote-file-create rname doc)
  ;;(display* "remote-file-create " rname ", " doc "\n")
  (let* ((uid (server-get-user envelope))
         (fid (file-name->resource (tmfs-cdr rname)))
         (l (tmfs->list rname))
         (did (safe-car (search-file (cDr (cdr l))))))
    (cond ((not uid)
           (server-error envelope "Error: not logged in"))
          (fid
           (server-error envelope "Error: file already exists"))
          ((not did)
           (server-error envelope "Error: directory does not exist"))
          ((not (db-allow? did uid "writable"))
           (server-error envelope "Error: write access required for directory"))
          (else
            (let* ((rid (db-create (cAr l) "file" uid))
                   (name (repository-add rid (url-suffix rname)))
                   (fname (repository-get rid)))
              (inherit-properties rid did)
              (db-set rid "dir" (list did))
              (string-save doc fname)
              (with props (db-get-all-decoded rid)
                (server-return envelope (list doc props))))))))

(tm-service (remote-file-load rname)
  ;;(display* "remote-file-load " rname "\n")
  (let* ((uid (server-get-user envelope))
         (rid (file-name->resource (tmfs-cdr rname)))
         (fname (repository-get rid)))
    (cond ((not uid) ;; FIXME: anonymous access
           (server-error envelope "Error: not logged in"))
          ((not rid)
           (server-error envelope "Error: file does not exist"))
          ((not (db-allow? rid uid "readable"))
           (server-error envelope "Error: read access denied"))
          ((not (url-exists? fname))
           (server-error envelope "Error: file not found"))
          (else
            (let* ((props (db-get-all-decoded rid))
                   (doc (string-load fname)))
              (server-return envelope (list doc props)))))))

(tm-service (remote-file-save rname doc)
  ;;(display* "remote-file-save " rname ", " doc "\n")
  (let* ((uid (server-get-user envelope))
         (rid (file-name->resource (tmfs-cdr rname)))
         (fname (repository-get rid)))
    (cond ((not uid)
           (server-error envelope "Error: not logged in"))
          ((not rid)
           (server-error envelope "Error: file does not exist"))
          ((not (db-allow? rid uid "writable"))
           (server-error envelope "Error: write access denied"))
          (else
            (with props (db-get-all-decoded rid)
              (string-save doc fname)
              (server-return envelope (list doc props)))))))

(tm-service (remote-file-set-properties rname props)
  ;;(display* "remote-file-set-properties " rname ", " props "\n")
  (let* ((uid (server-get-user envelope))
         (rid (file-name->resource (tmfs-cdr rname))))
    (cond ((not uid)
           (server-error envelope "Error: not logged in"))
          ((not rid)
           (server-error envelope "Error: file does not exist"))
          ((not (db-allow? rid uid "owner"))
           (server-error envelope "Error: administrative access denied"))
          (else
            (db-set-all-encoded rid props)
            (with new-props (db-get-all-decoded rid)
              (server-return envelope new-props))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Remote directories
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-service (remote-dir-create rname)
  (display* "remote-dir-create " rname "\n")
  (let* ((uid (server-get-user envelope))
         (fid (file-name->resource (tmfs-cdr rname)))
         (l (tmfs->list rname))
         (did (safe-car (search-file (cDr (cdr l))))))
    (cond ((not uid)
           (server-error envelope "Error: not logged in"))
          (fid
           (server-error envelope "Error: directory already exists"))
          ((not did)
           (server-error envelope "Error: directory does not exist"))
          ((not (db-allow? did uid "writable"))
           (server-error envelope "Error: write access required for directory"))
          (else
            (let* ((rid (db-create (cAr l) "dir" uid)))
              (inherit-properties rid did)
              (db-set rid "dir" (list did))
              (with props (db-get-all-decoded rid)
                (server-return envelope (list (list) props))))))))

(define (filter-read-access rids uid)
  (cond ((null? rids) rids)
        ((db-allow? (car rids) uid "readable")
         (cons (car rids) (filter-read-access (cdr rids) uid)))
        (else (filter-read-access (cdr rids) uid))))

(define (rewrite-dir-entry rid)
  (let* ((short-name (db-field-get-first rid "name" "?"))
         (full-name (resource->file-name rid))
         (dir? (== (db-field-get-first rid "type" #f) "dir"))
         (props (db-get-all-decoded rid)))
    (list short-name full-name dir? props)))

(tm-service (remote-dir-load rname)
  ;;(display* "remote-dir-load " rname "\n")
  (with uid (server-get-user envelope)
    (if (not uid) (server-error envelope "Error: not logged in")
        (let* ((server (car (tmfs->list rname)))
               (dirs (search-file (cdr (tmfs->list rname))))
               (rid (safe-car dirs)))
          (cond ((not rid)
                 (server-error envelope "Error: directory does not exist"))
                ((not (db-allow? rid uid "readable"))
                 (server-error envelope "Error: read access required"))
                (else
                  (let* ((matches (dir-contents rid))
                         (filtered (filter-read-access matches uid))
                         (rewr (map rewrite-dir-entry filtered))
                         (props (db-get-all-decoded rid)))
                    (server-return envelope (list rewr props)))))))))
