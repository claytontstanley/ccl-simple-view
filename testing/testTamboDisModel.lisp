; Bootstrap all needed packages (loads ACT-R, Cocoa framework, etc.)
#-:ccl-simple-view (load (format nil "~a~a" (directory-namestring *load-truename*) "bootstrap.lisp"))

#+:clozure (setf *pool* (init-pool))

#+:clozure (setf *ar-pool* (ccl::create-autorelease-pool))

; FIXME: Does MCL need CFBundle.lisp to run Phaser?
(provide :cfbundle)

(load-file-list "file-lists" "TamboDisModel" "testPhaser.txt")

(run-all-models :rt t)

(setf *experiment* nil)

