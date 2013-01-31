; Bootstrap all needed packages (loads ACT-R, Cocoa framework, etc.)
(load (format nil "~a~a" (directory-namestring *load-truename*) "bootstrap.lisp"))

(load-file-list "file-lists" "NSFMotor" "testNSFMotor.txt")

(setf *exp* (make-instance 'target-window))

(run-model)

(unintern 'check-hit)
