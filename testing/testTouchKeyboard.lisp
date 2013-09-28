; Bootstrap all needed packages (loads ACT-R, Cocoa framework, etc.)
(load (format nil "~a~a" (directory-namestring *load-truename*) "bootstrap.lisp"))

(load-as-lst ".." "bincarbon" "touch-keyboard.lisp")
(load-as-lst ".." "bincarbon" "testing" "testTouchKeyboard.lisp")
