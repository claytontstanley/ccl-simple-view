; Bootstrap all needed packages (loads ACT-R, Cocoa framework, etc.)
(load (format nil "~a~a" (directory-namestring *load-truename*) "bootstrap.lisp"))

(setf *sound* (create-resource 
                'sound 
                (format nil "~a~a/~a" (directory-namestring *load-truename*) "data" "Warning.aif")))

(add-resource *sound* "Warning")

(#/play (get-resource-val "Warning"))
