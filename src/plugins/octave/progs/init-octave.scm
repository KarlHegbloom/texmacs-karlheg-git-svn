
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : init-octave.scm
;; DESCRIPTION : Initialize octave plugin
;; COPYRIGHT   : (C) 1999  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (octave-initialize)
  (import-from (utils plugins plugin-convert))
  (plugin-input-converters octave))

(define octave-launcher (if (os-mingw?) "tm_octave.bat" "tm_octave"))

(plugin-configure octave
  (:winpath "Octave*" "bin")
  (:require (url-exists-in-path? "octave"))
  (:initialize (octave-initialize))
  (:launch ,octave-launcher)
  (:session "Octave"))
