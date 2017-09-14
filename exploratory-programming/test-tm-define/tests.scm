;;; tests.scm

(define-module (test-tm-define tests))


(define in-slain-flag #t)

(define-public (in-slain?) in-slain-flag)


(define other-thing-flag #t)

(define-public (is-the-other-thing-flag-on?) other-thing-flag)


(tm-define (blah blah)
  (#:secure)
  (#:interactive)
  (cons 'base-blah blah))

(tm-define (blah blah)
  (#:require (in-slain?))
  (cons 'in-slain-blah (former blah)))

(tm-define (blah blah)
  (#:require (is-the-other-thing-flag-on?))
  (cons 'another-thing-blah (former blah)))

