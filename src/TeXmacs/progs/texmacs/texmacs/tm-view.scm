
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : tm-view.scm
;; DESCRIPTION : setting the view preferences and properties
;; COPYRIGHT   : (C) 2001  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (texmacs texmacs tm-view))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; View preferences
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (main-icon-bar-default)
  (if (like-macos?) "off" "on"))

(define (notify-header var val)
  (show-header (== val "on")))

(define (notify-icon-bar var val)
  (cond ((== var "main icon bar")
         (show-icon-bar 0 (== val "on")))
        ((== var "mode dependent icons")
         (show-icon-bar 1 (== val "on")))
        ((== var "focus dependent icons")
         (show-icon-bar 2 (== val "on")))
        ((== var "user provided icons")
         (show-icon-bar 3 (== val "on")))))

(define (notify-status-bar var val)
  (show-footer (== val "on")))

(define (notify-side-tools var val)
  (cond ((== var "side tools")
         (show-side-tools 0 (== val "on")))))

(define (notify-zoom-factor var val)
  (with z (string->number val)
    (set! z (max (min z 10.0) 0.1))
    (set-default-zoom-factor z)
    (set-window-zoom-factor z)))

(define (notify-remote-control var val)
  (ahash-set! remote-control-remap val var))

(define-preferences
  ("header" "on" notify-header)
  ("main icon bar" (main-icon-bar-default) notify-icon-bar)
  ("mode dependent icons" "on" notify-icon-bar)
  ("focus dependent icons" "on" notify-icon-bar)
  ("user provided icons" "off" notify-icon-bar)
  ("status bar" "on" notify-status-bar)
  ("side tools" "off" notify-side-tools)
  ("zoom factor" "1" notify-zoom-factor)
  ("ir-up" "home" notify-remote-control)
  ("ir-down" "end" notify-remote-control)
  ("ir-left" "pageup" notify-remote-control)
  ("ir-right" "pagedown" notify-remote-control)
  ("ir-center" "S-return" notify-remote-control)
  ("ir-play" "F5" notify-remote-control)
  ("ir-pause" "escape" notify-remote-control)
  ("ir-menu" "." notify-remote-control)
  ("draw cursor" "on" noop))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Changing the view properties
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (toggle-visible-header)
  (:synopsis "Toggle the visibility of the window's header.")
  (:check-mark "v" visible-header?)
  (with val (not (visible-header?))
    (if (== (windows-number) 1)
        (set-boolean-preference "header" val)
        (show-header val))))

(tm-define (toggle-visible-footer)
  (:synopsis "Toggle the visibility of the window's footer.")
  (:check-mark "v" visible-footer?)
  (with val (not (visible-footer?))
    (if (== (windows-number) 1)
        (set-boolean-preference "status bar" val)
        (show-footer val))))

(tm-define (toggle-visible-side-tools n)
  (:synopsis "Toggle the visibility of the @n-th side tools.")
  (:check-mark "v" visible-side-tools?)
  (with val (not (visible-side-tools? n))
    (if (and (== (windows-number) 1) (== n 0))
        (set-boolean-preference "side tools" val)
        (show-side-tools n val))))

(tm-define (toggle-visible-bottom-tools n)
  (:synopsis "Toggle the visibility of the bottom tools.")
  (:check-mark "v" visible-bottom-tools?)
  (with val (not (visible-bottom-tools? n))
    (if (and (== (windows-number) 1) (== n 0))
        (set-boolean-preference "bottom tools" val)
        (show-bottom-tools n val))))

(tm-define (toggle-visible-icon-bar n)
  (:synopsis "Toggle the visibility of the @n-th icon bar.")
  (:check-mark "v" visible-icon-bar?)
  (let* ((val (not (visible-icon-bar? n)))
         (var (cond ((== n 0) "main icon bar")
                    ((== n 1) "mode dependent icons")
                    ((== n 2) "focus dependent icons")
                    ((== n 3) "user provided icons"))))
    (if (== (windows-number) 1)
        (set-boolean-preference var val)
        (show-icon-bar n val))))

(define saved-informative-flags "default")

(tm-define (toggle-full-screen-mode)
  (:synopsis "Toggle full screen mode.")
  (:check-mark "v" full-screen?)
  (if (full-screen?)
      (begin
        (init-env "info-flag" saved-informative-flags)
        (full-screen-mode #f #f)
        (fit-to-screen))
      (begin
        (set! saved-informative-flags (get-init-env "info-flag"))
        (init-env "info-flag" "none")
        (full-screen-mode #t #f)
        (fit-to-screen-width))))

(tm-define (toggle-full-screen-edit-mode)
  (:synopsis "Toggle full screen edit mode.")
  (:check-mark "v" full-screen-edit?)
  (full-screen-mode (not (full-screen-edit?)) (not (full-screen-edit?))))

(tm-define (toggle-remote-control-mode)
  (:synopsis "Toggle remote keyboard control mode.")
  (:check-mark "v" remote-control-mode?)
  (set! remote-control-flag? (not remote-control-flag?)))

(define (test-zoom-factor? z)
  (<= (abs (- (get-window-zoom-factor) (eval z))) 0.01))

(tm-define (change-zoom-factor z)
  (:check-mark "*" test-zoom-factor?)
  (set! z (max (min z 10.0) 0.1))
  (if (== (windows-number) 1)
      (set-preference "zoom factor" (number->string z))
      (set-window-zoom-factor z)))

(tm-define (other-zoom-factor s)
  (:argument s "Zoom factor")
  (if (string-ends? s "%")
      (with p (string->number (string-drop-right s 1))
        (change-zoom-factor (* 0.01 p)))
      (change-zoom-factor (string->number s))))

(define (normalize-zoom-sub zoom l)
  (cond ((null? l) zoom)
        ((< (abs (- zoom (car l))) (* 0.02 zoom)) (car l))
        (else (normalize-zoom-sub zoom (cdr l)))))

(define (normalize-zoom zoom)
  (with std-zooms (map (lambda (x) (exp (* x (/ (log 2.0) 4.0))))
                       (.. -10 10))
    (normalize-zoom-sub zoom std-zooms)))

(tm-define (zoom-in x)
  (let* ((old (get-window-zoom-factor))
         (new (normalize-zoom (* x old))))
    (change-zoom-factor new)))

(tm-define (zoom-out x)
  (zoom-in (/ 1.0 x)))

(tm-define (fit-to-screen)
  (let* ((wf (/ (* 1.0 (get-window-width)) (get-page-width)))
         (hf (/ (* 1.0 (get-window-height)) (get-page-height)))
         (f (min wf hf)))
    (change-zoom-factor (- f 0.0001))))

(tm-define (fit-to-screen-width)
  (with f (/ (* 1.0 (get-window-width)) (get-page-width))
    (change-zoom-factor (- f 0.0001))))

(tm-define (fit-to-screen-height)
  (with f (/ (* 1.0 (get-window-height)) (get-page-height))
    (change-zoom-factor (- f 0.0001))))
