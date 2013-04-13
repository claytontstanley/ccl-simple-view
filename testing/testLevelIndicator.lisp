; Bootstrap all needed packages (loads ACT-R, Cocoa framework, etc.)
(load (format nil "~a~a" (directory-namestring *load-truename*) "bootstrap.lisp"))

(setf *win* (make-instance 'window
                           :view-size (make-point 500 500)))

(setf *view* (make-instance 'thermometer
                            :position (make-point 20 100)
                            :thermometer-value 3
                            :direction :vertical
                            :max-value 5
                            :size (make-point 10 100)))
(add-subviews *win* *view*)

(setf *view* (make-instance 'thermometer
                            :position (make-point 35 100)
                            :thermometer-value 3
                            :direction :horizontal
                            :max-value 5
                            :size (make-point 10 100)))
(add-subviews *win* *view*)

(dotimes (i 5)
  (dolist (view (subviews *win*))
    (setf (thermometer-value view) i)
    (sleep .1)))

#|
(setf (thermometer-value *view*) 3)
(setf (thermometer-max-value *view*) 5)
(view-size *view*)
(setf (direction *view*) :vertical)
|#
