; Bootstrap all needed packages (loads ACT-R, Cocoa framework, etc.)
(load (format nil "~a~a" (directory-namestring *load-truename*) "bootstrap.lisp"))

(load-file-list "file-lists" "Gallagher112" "testGallagher.txt")


(begin-experiment)


#|
(window-close (front-window))

|#
