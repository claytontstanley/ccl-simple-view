; Bootstrap all needed packages (loads ACT-R, Cocoa framework, etc.)
(load (format nil "~a~a" (directory-namestring *load-truename*) "bootstrap.lisp"))

(load-file-list "file-lists" "StanleyThesis" "testStanleyThesis.txt")

(begin-experiment
  :snum 1
  :eyetracking nil
  :allowing-quit t
  :short-stack 20)
