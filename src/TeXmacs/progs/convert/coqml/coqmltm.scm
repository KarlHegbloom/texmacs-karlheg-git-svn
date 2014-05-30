
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : coqtm.scm
;; DESCRIPTION : conversion of CoqML trees to TeXmacs trees
;; COPYRIGHT   : (C) 2013 François Poulain and Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.coqml>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (convert coqml coqtm)
  (:use (convert tools tmtable)
	(convert tools sxml)
	(convert tools xmltm)))

(define map map-in-order)

(define (coqtm-error message)
  `((with "color" "red" ,message)))

(define (coqtm-string s)
  (utf8->cork s))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Accessors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (coqtm-get-attributes att att-l)
  (if (nsymbol? att) #f
    (filter nnull?
            (map (lambda (x)
                   (if (!= (car x) att) '()
                     (coqtm-as-serial
                       (environment)
                       (coqtm-string (cadr x))))) att-l))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tactics expressions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (coqtm-token env a c)
  (if (== (length c) 1)
    `((coq-token
        ,@(coqtm-get-attributes 'begin a)
        ,@(coqtm-get-attributes 'end a)
        ,(coqtm-as-serial env (first c))))
    (coqtm-error "bad token")))

(define (coqtm-apply env a c)
  (if (> (length c) 0)
    `((coq-apply
        ,@(coqtm-get-attributes 'begin a)
        ,@(coqtm-get-attributes 'end a)
        ,@(map (cut coqtm-as-serial env <>) c)))
    (coqtm-error "bad apply")))

(define (make-tex-args s)
  (cond ((< (string-length s) 2) "")
        ((string-starts? s "#") (string-append
                                  (string-take s 2)
                                  (make-tex-args (string-tail s 2))))
        ((string-starts? s "\\")  (make-tex-args (string-tail s 2)))
        (else (make-tex-args (string-tail s 1)))))

(define (coqtm-operator env a c)
  (if (== (length c) 0)
    (let ((names  (coqtm-get-attributes 'name a))
          (texes  (coqtm-get-attributes 'format-tex a))
          (begins (coqtm-get-attributes 'begin a))
          (ends   (coqtm-get-attributes 'end a)))
      (if (and (list>0? names) (list>0? texes))
        (let* ((val  (car texes))
               (args (make-tex-args val))
               (m    (tree->stree
                       (generic->texmacs
                         (string-append "\\def\\dummy" args "{" val "}")
                         "latex-snippet"))))
          (if (func? m 'assign 2) (set! names (cddr m)))))
      `((coq-operator ,@names ,@begins ,@ends)))
      (coqtm-error "bad operator")))

(define (coqtm-constant env a c)
  (if (== (length c) 0)
    `((coq-constant
        ,@(coqtm-get-attributes 'name a)
        ,@(coqtm-get-attributes 'begin a)
        ,@(coqtm-get-attributes 'end a)))
    (coqtm-error "bad constant")))

(define (coqtm-typed env a c)
  (if (> (length c) 1)
    `((coq-typed
        ,@(map (cut coqtm-as-serial env <>) c)))
    (coqtm-error "bad typed")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Vernacular commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (coqtm-check env a c)
  (if (== (length c) 1)
    `((coq-check
        ,@(coqtm-get-attributes 'begin a)
        ,@(coqtm-get-attributes 'end a)
        ,(coqtm-as-serial env (first c))))
    (coqtm-error "bad check")))

(define (coqtm-definition env a c)
  (if (== (length c) 1)
    `((coq-definition
        ,@(coqtm-get-attributes 'type a)
        ,@(coqtm-get-attributes 'name a)
        ,@(coqtm-get-attributes 'begin a)
        ,@(coqtm-get-attributes 'end a)
        ,(coqtm-as-serial env (first c))))
    (coqtm-error "bad definition")))

(define (coqtm-theorem env a c)
  (if (== (length c) 1)
    `((coq-theorem
        ,@(coqtm-get-attributes 'type a)
        ,@(coqtm-get-attributes 'name a)
        ,@(coqtm-get-attributes 'begin a)
        ,@(coqtm-get-attributes 'end a)
        ,(coqtm-as-serial env (first c))))
    (coqtm-error "bad theorem")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main translation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (coqtm-drop env a c) '())

(define (coqtm-pass env a c)
  (let ((l (coqtm-args env c)))
    (if (and (null? l) (not (assoc 'id a))) '()
	(list (xmltm-label-decorate
                a 'id (coqtm-serial (htmltm-preserve-space? env) l))))))

(define (coqtm-args env l)
  (append-map (lambda (x) (coqtm env x)) l))

(define (coqtm-args-serial env l)
  (coqtm-serial env (coqtm-args env l)))

(define (coqtm env t)
  (sxml-dispatch (lambda (env t) (list t)) coqtm-pass env t))

(tm-define coqtm-as-serial
    (case-lambda
      ((t) (with env (environment)
              (ahash-set! env 'preserve-space? #f)
              (coqtm-as-serial env t)))
      ((env t) (coqtm-serial env (coqtm env t)))))

(define handler coqtm-handler)

(logic-dispatcher coqtm-methods%
  ;; Raw
  (token      (handler :raw    coqtm-token))

  ;; Tactics
  (apply      (handler :tactic coqtm-apply))
  (operator   (handler :tactic coqtm-operator))
  (constant   (handler :tactic coqtm-constant))
  (typed      (handler :tactic coqtm-typed))

  ;; Vernacular
  (check      (handler :vernac coqtm-check))
  (definition (handler :vernac coqtm-definition))
  (theorem    (handler :vernac coqtm-theorem)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (parse-coqml-snippet s)
  (coqmltm-parse s))

(tm-define (parse-coqml-document s)
  `(!file ,(coqmltm-parse s)))

(tm-define (coqml->texmacs coqml)
  (:type (-> stree stree))
  (:synopsis "Convert a parsed CoqML stree @t into a TeXmacs stree.")
  (let* ((snippet? (not (func? coqml '!file 1)))
	 (body (if snippet? coqml (cadr coqml)))
	 (tm (filter (lambda (x) (!= x "\n")) (coqtm-as-serial body))))
    ;; (display* "coqml->texmacs: tm:    " tm "\n\n")
    (if snippet? tm
	(let* ((aux (stm-unary-document tm))
	       (doc (tree->stree (tree-simplify (stree->tree aux))))
	       (body `(body ,doc))
	       (style `(style (tuple "generic" "docoq"))))
	  `(document ,body ,style)))))
