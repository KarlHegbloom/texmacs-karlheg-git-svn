
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; arch-tag: 0591bd1f-9751-4082-8be2-312f88b5e59b
;;
;; MODULE      : proclus-lib.scm
;; DESCRIPTION : Generally useful commands needed by the 'proclus' plugin.
;; COPYRIGHT   : (C) 2003--2004  Alain Herreman, David Allouche
;;
;;   This program is free software; you can redistribute it and/or modify
;;   it under the terms of the GNU General Public License as published by
;;   the Free Software Foundation; either version 2 of the License, or
;;   (at your option) any later version.
;;
;;   You should have received a copy of the GNU General Public License
;;   along with this program; if not, write to the Free Software
;;   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (proclus-lib)
  (:export get-init-env
           get-strg-name-buffer
           tm-substree
           save-excursion
           list->tuple tuple->list))

(define (get-init-env s)
  (tree->string (get-init-tree s)))

(define (get-strg-name-buffer)
  (url->string (get-name-buffer)))

(define (tm-substree p)
  (tree->stree (tm-subtree p)))

(define-macro (save-excursion . body)
  ;; TODO: save point and mark too, like emacs
  (let ((buf-sym (gensym)))

    `(let ((,buf-sym (get-strg-name-buffer)))
       (dynamic-wind
           noop
           (lambda () ,@body)
           (lambda () (switch-to-active-buffer ,buf-sym))))))

(define (list->tuple l)
  ;; Convert a nested scheme list into a stree.
  (let sub ((x l))
    (cond ((null? x) '(tuple))
          ((pair? x) (cons 'tuple (map sub x)))
          (else x))))

(define (tuple->list x)
  ;; Strip tree labels recursively from a compound stree or tree.
  (let sub ((x (if (pair? x) x (tree->stree x))))
    (if (not (pair? x)) x (map sub (cdr x)))))
