

(setf *pool* (init-pool))

(setf *sound* (create-resource 
                'sound 
                (format nil "~a~a/~a" (directory-namestring *load-truename*) "data" "Warning.aif")))

(add-resource *sound* "Warning")

(#/play (get-resource-val "Warning"))
