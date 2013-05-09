; Bootstrap all needed packages (loads ACT-R, Cocoa framework, etc.)
(load (format nil "~a~a" (directory-namestring *load-truename*) "bootstrap.lisp"))

(load-as-lst ".." "submodules" "stanley-thesis" "model.lisp")

#|
(begin-experiment
  :eyetracking nil
  :allowing-quit nil
  :short-stack 20)

(subviews *library-experiment-window*)
(show-cursor)

|#
