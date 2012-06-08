
#-:act-r-6.0 (load "~/src/actr6/load-act-r-6.lisp")
#-:act-r-6.0 (load "~/src/actr6/load-act-r-6.lisp")
#+clozure (require :cocoa)
#+clozure (require :easygui)
#+clozure (load "~/src/mcl-migration/actr6/devices/ccl/uwi.lisp")
#+clozure (load "~/src/mcl-migration/actr6/devices/ccl/device.lisp")
#+clozure (load "~/src/mcl-migration/easygui/extensions.lisp")

(defparameter *path-separator*
  #+:digitool ":"
  #+:clozure "/")

(dolist (tutorial (list "unit1" "unit2"))
  (let ((path-to-loader
          (format nil "~a~a~a~a~a~a"
                  (directory-namestring *load-truename*) "tutorial" *path-separator* tutorial *path-separator* "loader.lisp")))
    (print path-to-loader)
    (load path-to-loader)))

