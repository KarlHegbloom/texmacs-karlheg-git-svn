
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : init-qcl.scm
;; DESCRIPTION : Initialize qcl plugin
;; COPYRIGHT   : (C) 1999  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (qcl-initialize)
  (import-from (texmacs plugin plugin-convert))
  (lazy-input-converter (qcl-input) qcl))

(plugin-configure qcl
  (:require (url-exists-in-path? "qcl"))
  (:initialize (qcl-initialize))
  (:launch "qcl --texmacs")
  (:session "Qcl"))
