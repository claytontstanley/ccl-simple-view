; Bootstrap all needed packages (loads ACT-R, Cocoa framework, etc.)
(load (format nil "~a~a" (directory-namestring *load-truename*) "bootstrap.lisp"))

(defparameter *win* (make-instance 'window))

(defparameter *view* (make-instance 'simple-view
                            :view-size (make-point 50 50)))

(defmethod view-draw-contents ((view (eql *view*))) 
  (start-polygon *view*)
  (move-to view 0 0)
  (line-to view 0 10)
  (line-to view 10 10)
  (line-to view 10 0)
  (line-to view 0 0)
  (let ((polygon (get-polygon view)))
    (fill-polygon view *black-pattern* polygon)
    (kill-polygon polygon)))

(add-subviews *win* *view*)

