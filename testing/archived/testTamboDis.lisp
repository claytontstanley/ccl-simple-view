; Bootstrap all needed packages (loads ACT-R, Cocoa framework, etc.)
(load (format nil "~a~a" (directory-namestring *load-truename*) "bootstrap.lisp"))

;(trace (window-close :before :backtrace))

(load-file-list "file-lists" "TamboDis" "testPhaser.txt")
(begin-experiment)


(all-processes)
;(setf *modal-dialog-ret* nil)
;*modal-dialog-ret*
;
