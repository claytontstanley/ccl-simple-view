; Bootstrap all needed packages (loads ACT-R, Cocoa framework, etc.)
(load (format nil "~a~a" (directory-namestring *load-truename*) "bootstrap.lisp"))

(setf *win* (make-instance 'window
                           :view-size (make-point 500 500)))

(setf *view* (make-instance 'thermometer
                            :position (make-point 100 100)
                            :size (make-point 10 100)))

(add-subviews *win* *view*)



;(setf (direction *view*) :vertical)
