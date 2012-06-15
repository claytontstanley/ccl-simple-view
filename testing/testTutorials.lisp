
#-:act-r-6.0 (load "~/src/actr6/load-act-r-6.lisp")
#+clozure (require :cocoa)
#+clozure (require :easygui)
#+clozure (load "~/src/mcl-migration/easygui/extensions.lisp")
#+clozure (load "~/src/mcl-migration/actr6/devices/ccl/share.lisp")
#+clozure (load "~/src/mcl-migration/actr6/devices/ccl/device.lisp")
#+clozure (load "~/src/mcl-migration/actr6/devices/ccl/uwi.lisp")

(defun run-in-file (file fun &key (close-window-p t))
  (load (concatenate 'string
                     (directory-namestring *load-truename*)
                     file))
  (funcall fun)
  (when close-window-p
    (close-rpm-window *library-experiment-window*)))

(defparameter *path-separator*
  #+:digitool ":"
  #+:clozure "/")

(defun do-tutorial (tutorial)
  (let ((path-to-loader
          (format nil "~a~a~a~a~a~a"
                  (directory-namestring *load-truename*) "tutorial" *path-separator* tutorial *path-separator* "loader.lisp")))
    (print path-to-loader)
    (load path-to-loader)))


; This suite takes about 4 mins to run on CCL b/c of unit4:
;(mapc #'do-tutorial (list "unit1" "unit2" "unit3" "unit4"))

; This suite takes about 4 mins to run on CCL b/c of unit7:
;(mapc #'do-tutorial (list "unit7"))

(mapc #'do-tutorial (list "unit1" "unit2" "unit3" "unit5" "unit6" "unit8"))


