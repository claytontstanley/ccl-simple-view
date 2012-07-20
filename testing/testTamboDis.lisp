; Bootstrap all needed packages (loads ACT-R, Cocoa framework, etc.)
#-:ccl-simple-view (load (format nil "~a~a" (directory-namestring *load-truename*) "bootstrap.lisp"))

; FIXME: Does MCL need CFBundle.lisp to run Phaser?
(setf *pool* (init-pool))

(provide :cfbundle)

(load-file-list "file-lists" "TamboDis" "testPhaser.txt")
(begin-experiment)

(inspect *experiment*)
