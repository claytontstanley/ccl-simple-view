; Bootstrap all needed packages (loads ACT-R, Cocoa framework, etc.)
(load (format nil "~a~a" (directory-namestring *load-truename*) "bootstrap.lisp"))


; FIXME: Does MCL need CFBundle.lisp to run Phaser?
(provide :cfbundle)

;(trace (window-close :before :backtrace))

(load-file-list "file-lists" "TamboDis" "testPhaser.txt")
(begin-experiment)

(make-instance 'window)

(all-processes)
;(setf *modal-dialog-ret* nil)
;*modal-dialog-ret*
;
