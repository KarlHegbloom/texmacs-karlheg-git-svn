;;; .emacs-local.el
;;;
;;; Probably no mechanism inside of emacs for this...
;;; I will just include it from my primary ~/.emacs.d/.emacs
;;;

;; (setq tags-table-list '("/home/karlheg/src/TeXmacs/texmacs-git-svn-guile-2.2/src/TAGS"
;;                         "/home/karlheg/src/Guile/guile-2.2-git/TAGS"
;;                         "/home/karlheg/src/Guile/guile-2.2-git/libguile/TAGS"
;;                         "/home/karlheg/src/Guile/guile-2.2-git/lib/TAGS"
;;                         "/home/karlheg/src/Guile/guile-2.2-git/module/TAGS"
;;                         "/home/karlheg/src/Guile/guile-2.2-git/guile-readline/TAGS"))

(defun texmacs-gdb-in-place ()
  ""
  (interactive)
  (gdb (string-join (list cmake-ide-project-dir "/texmacs --gdb -debug"))))
