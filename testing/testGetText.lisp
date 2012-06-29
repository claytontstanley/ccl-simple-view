; Bootstrap all needed packages (loads ACT-R, Cocoa framework, etc.)
#-:ccl-gui (load (format nil "~a~a" (directory-namestring *load-truename*) "bootstrap.lisp"))

(setf *str* (get-string-from-user "prompt"))

(print *str*)


