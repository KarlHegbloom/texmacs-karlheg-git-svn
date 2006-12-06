
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : init-latex.scm
;; DESCRIPTION : setup latex converters
;; COPYRIGHT   : (C) 2003  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (convert latex init-latex))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LaTeX
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; FIXME: the intermediate latex-stree data format is different
;; for conversions to and from TeXmacs. After rewriting
;; the input filter, both formats should be identical.

(define (latex-recognizes-at? s pos)
  (set! pos (format-skip-spaces s pos))
  (cond ((format-test? s pos "\\document") #t)
	((format-test? s pos "\\usepackage") #t)
	((format-test? s pos "\\input") #t)
	((format-test? s pos "\\includeonly") #t)
	((format-test? s pos "\\chapter") #t)
	((format-test? s pos "\\appendix") #t)
	((format-test? s pos "\\section") #t)
	((format-test? s pos "\\begin") #t)
	(else #f)))

(define (latex-recognizes? s)
  (and (string? s) (latex-recognizes-at? s 0)))

(define-format latex
  (:name "LaTeX")
  (:suffix "tex" "ltx" "sty" "cls")
  (:recognize latex-recognizes?))

(lazy-define (convert latex texout) serialize-latex)
(lazy-define (convert latex tmtex) texmacs->latex)
(lazy-define (convert latex tmtex-elsevier) init-elsevier)

(converter latex-document latex-tree
  (:function parse-latex-document))

(converter latex-stree latex-document
  (:function serialize-latex))

(converter latex-snippet latex-tree
  (:function parse-latex))

(converter latex-stree latex-snippet
  (:function serialize-latex))

(converter latex-tree texmacs-tree
  (:function latex->texmacs))

(converter texmacs-stree latex-stree
  (:function-with-options texmacs->latex)
  (:option "texmacs->latex:replace-style" "on")
  (:option "texmacs->latex:expand-macros" "on")
  (:option "texmacs->latex:expand-user-macros" "off")
  (:option "texmacs->latex:indirect-bib" "off")
  (:option "texmacs->latex:use-catcodes" "off")
  (:option "texmacs->latex:use-macros" "on"))
