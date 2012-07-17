;#+:clozure (with-continue  (setf (macro-function 'rlet) *ccl-rlet*))

; Bootstrap all needed packages (loads ACT-R, Cocoa framework, etc.)
#-:ccl-simple-view (load (format nil "~a~a" (directory-namestring *load-truename*) "bootstrap.lisp"))

(load-file-list "file-lists" "NextGen" "testNextGen.txt")

;(open-stream)
