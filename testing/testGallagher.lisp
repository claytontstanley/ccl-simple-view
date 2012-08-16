; Bootstrap all needed packages (loads ACT-R, Cocoa framework, etc.)
(load (format nil "~a~a" (directory-namestring *load-truename*) "bootstrap.lisp"))

; FIXME: Does MCL need CFBundle.lisp to run Phaser?
(provide :cfbundle)

(load-file-list "file-lists" "Gallagher" "testGallagher.txt")


(begin-experiment)


#|
(window-close (get-front-window))

|#
