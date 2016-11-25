
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : init-images.scm
;; DESCRIPTION : setup image converters
;; COPYRIGHT   : (C) 2003  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (convert images init-images))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Graphical document and geometric image formats
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-format postscript
  (:name "Postscript")
  (:suffix "ps" "eps"))

(define-format pdf
  (:name "Pdf")
  (:suffix "pdf"))

;;(converter pdf-file postscript-file
;;  (:require (url-exists-in-path? "pdf2ps"))
;;  (:shell "pdf2ps" from to))
  
;; many options for pdf->ps/eps see http://tex.stackexchange.com/a/20884
;; this one does a better rendering than pdf2ps (also based on gs):
(with gs (if (or (os-win32?) (os-mingw?)) "gswin32c" "gs")
  (converter pdf-file postscript-file
  ;;(:require (url-exists-in-path? gs )) ;; gs IS a dependency
  (:shell ,gs "-q -dNOCACHE -dUseCropBox -dNOPAUSE -dBATCH -dSAFER -sDEVICE=eps2write -sOutputFile=" to from)))  
;; problem: 
;; eps2write available starting with gs  9.14 (2014-03-26)
;; epswrite removed in gs 9.16 (2015-03-30)


(converter postscript-file pdf-file
  (:require (url-exists-in-path? "ps2pdf"))
  (:shell "ps2pdf" from to))

(define-format xfig
  (:name "Xfig")
  (:suffix "fig"))

(converter xfig-file postscript-file
  (:shell "fig2ps" from to))

(define-format xmgrace
  (:name "Xmgrace")
  (:suffix "agr" "xmgr"))

(converter xmgrace-file postscript-file
  (:require (url-exists-in-path? "xmgrace"))
  (:shell "xmgrace" "-noask -hardcopy -hdevice EPS -printfile" to from))

(define-format svg
   (:name "Svg")
   (:suffix "svg"))

(converter svg-file postscript-file
  (:require (url-exists-in-path? "inkscape"))
  (:shell "inkscape" "-z" "-f" from "-P" to))

(converter svg-file pdf-file
  (:require (url-exists-in-path? "inkscape"))
  (:shell "inkscape" "-z" "-f" from "-A" to))

(when (url-exists-in-path? "rsvg-convert")
  (tm-define (rsvg-convert x opts)
  (let* ((dest (assoc-ref opts 'dest))
         (fm (url-format (url-concretize dest)))
         (res (or (assoc-ref opts "texmacs->image:raster-resolution") (get-preference "texmacs->image:raster-resolution")))
		 (cmd (if (or (os-win32?) (os-mingw?)) 
                     (escape-shell (url-concretize (url-resolve-in-path "rsvg-convert"))) "rsvg-convert"))
         )
    (system-2 (string-append cmd " -f " fm " -d " res " -o ") dest x )
	(if (url-exists? dest) dest #f)))

(converter svg-file png-file
    (:function-with-options rsvg-convert)
    ;;(:option "texmacs->image:raster-resolution" "450") ;;if this is set it overrides the preference widget settings
   ))
  

(define-format geogebra
  (:name "Geogebra")
  (:suffix "ggb"))

(converter geogebra-file postscript-file
  (:require (url-exists-in-path? "geogebra"))
  (:shell "geogebra" "--export=" to "--dpi=600" from))

(converter geogebra-file svg-file
  (:require (url-exists-in-path? "geogebra"))
  (:shell "geogebra" "--export=" to "--dpi=600" from))
  
(cond
 ((url-exists-in-path? "pdftocairo")
  (tm-define (pdf-file->pdftocairo-raster x opts)
  (let* ((dest (assoc-ref opts 'dest))
         (fullname (url-concretize dest))
         (fm (url-format fullname))
         (suffix (url-suffix fullname))
         (name (string-drop-right fullname (+ 1 (string-length suffix))))
         (res (or (assoc-ref opts "texmacs->image:raster-resolution") (get-preference "texmacs->image:raster-resolution")))
		 (cmd (if (or (os-win32?) (os-mingw?)) 
                     (escape-shell (url-concretize (url-resolve-in-path "pdftocairo"))) "pdftocairo"))
         )
    ;;(display (string-append cmd " -singlefile -" fm " -r " res " " x " "  name))
    (system-2 (string-append cmd " -singlefile -transp -" fm " -r " res) x name)
	(if (url-exists? dest) dest #f)))

  (converter pdf-file png-file
    (:function-with-options pdf-file->pdftocairo-raster)
    ;;(:option "texmacs->image:raster-resolution" "450") ;;if this is set it overrides the preference widget settings
   )

  (converter pdf-file jpeg-file
    (:function-with-options pdf-file->pdftocairo-raster)
    ;;(:option "texmacs->image:raster-resolution" "300")
   )

  (converter pdf-file postscript-document
    (:shell "pdftocairo" "-eps" from to))

  (converter pdf-file postscript-file
    (:shell "pdftocairo" "-eps" from to))

  (converter pdf-file svg-file
    (:shell "pdftocairo" "-origpagesizes -nocrop -nocenter -svg" from to)))
  
 ((and (url-exists-in-path? "convert") (url-exists-in-path? "conjure"))
  (tm-define (pdf-file->imagemagick-raster x opts)
	  (let* ((dest (assoc-ref opts 'dest))
			  (res (or (assoc-ref opts "texmacs->image:raster-resolution") (get-preference "texmacs->image:raster-resolution"))))
		;;(display (string-append "convert -density " res " " x " "  dest))
        (system-2 (string-append "convert -density " res) x dest)
		(if (url-exists? dest) dest #f)))

  (converter pdf-file png-file
    (:function-with-options pdf-file->imagemagick-raster)
    ;;(:option "texmacs->image:raster-resolution" "300")
    )
  
  (converter pdf-file jpeg-file
    (:function-with-options pdf-file->imagemagick-raster)
    ;;(:option "texmacs->image:raster-resolution" "300")
    )
 
  (converter pdf-file tif-file
    (:function-with-options pdf-file->imagemagick-raster)
    ;;(:option "texmacs->image:raster-resolution" "300")
  )
))
  
(converter pdf-file svg-file
  (:require (url-exists-in-path? "pdf2svg"))
  (:shell "pdf2svg" from to))
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Bitmap image formats
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-format xpm
  (:name "Xpm")
  (:suffix "xpm"))

(converter xpm-file ppm-file
  (:require (url-exists-in-path? "convert"))
  (:shell "convert" from to))

(define-format jpeg
  (:name "Jpeg")
  (:suffix "jpg" "jpeg"))

(converter jpeg-file postscript-document
  (:function image->psdoc))

(converter jpeg-file pnm-file
  (:require (url-exists-in-path? "convert"))
  (:shell "convert" from to))

(define-format tif
  (:name "Tif")
  (:suffix "tif"))

(converter tif-file postscript-document
  (:function image->psdoc))

(define-format ppm
  (:name "Ppm")
  (:suffix "ppm"))

(converter ppm-file gif-file
  (:require (url-exists-in-path? "convert"))
  (:shell "convert" from to))

(define-format gif
  (:name "Gif")
  (:suffix "gif"))

(converter gif-file postscript-document
  (:function image->psdoc))

(converter gif-file pnm-file
  (:require (url-exists-in-path? "convert"))
  (:shell "convert" from to))

(define-format png
  (:name "Png")
  (:suffix "png"))

(converter png-file postscript-document
  (:function image->psdoc))

(converter png-file pnm-file
  (:require (url-exists-in-path? "convert"))
  (:shell "convert" from to))

(converter geogebra-file png-file
  (:require (url-exists-in-path? "geogebra"))
  (:shell "geogebra" "--export=" to "--dpi=600" from))

(define-format pnm
  (:name "Pnm")
  (:suffix "pnm"))

(converter pnm-file postscript-document
  (:function image->psdoc))

