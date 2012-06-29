; Bootstrap all needed packages (loads ACT-R, Cocoa framework, etc.)
#-:ccl-gui (load (format nil "~a~a" (directory-namestring *load-truename*) "bootstrap.lisp"))

(setf *pool* (init-pool))

(setf *sound* (create-resource 
                'sound 
                (format nil "~a~a/~a" (directory-namestring *load-truename*) "data" "Warning.aif")))

(add-resource *sound* "Warning")

(#/play (get-resource-val "Warning"))
