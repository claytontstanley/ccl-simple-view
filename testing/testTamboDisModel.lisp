; Bootstrap all needed packages (loads ACT-R, Cocoa framework, etc.)
(load (format nil "~a~a" (directory-namestring *load-truename*) "bootstrap.lisp"))

; FIXME: Does MCL need CFBundle.lisp to run Phaser?
(provide :cfbundle)

(load-file-list "file-lists" "TamboDisModel" "testPhaser.txt")

(run-all-models :rt nil)
;(run-a-model 0 :rt nil) 

(setf *experiment* nil)

