;;; .emacs-local.el
;;;
;;; Probably no mechanism inside of emacs for this...
;;; I will just include it from my primary ~/.emacs.d/.emacs
;;;

(defun texmacs-gdb-in-place ()
  ""
  (interactive)
  (gdb (string-join (list cmake-ide-project-dir "/texmacs --gdb"))))
