; Bootstrap all needed packages (loads ACT-R, Cocoa framework, etc.)
#-:ccl-simple-view (load (format nil "~a~a" (directory-namestring *load-truename*) "bootstrap.lisp"))

(setf *win* (make-instance 'window))

(setf *view* (make-instance 'simple-view
                            :view-size (make-point 50 50)))

(add-subviews *win* *view*)

(move-to *view* 0 0)

(start-polygon *view*)

(line-to *view* 0 10)
(line-to *view* 10 10)
(line-to *view* 10 0)
(line-to *view* 0 0)

(fill-polygon *view* *black-pattern* nil)
