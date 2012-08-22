; Bootstrap all needed packages (loads ACT-R, Cocoa framework, etc.)
(load (format nil "~a~a" (directory-namestring *load-truename*) "bootstrap.lisp"))

(defparameter *win* (make-instance 'static-window))

(defparameter *view* (make-instance 'simple-view
                                    :view-size (make-point 50 50)
                                    :view-position (make-point 10 60)))

(with-focused-view *win*
  (with-fore-color *blue-color*
    (start-polygon *win*)
    (move-to *win* 10 10)
    (line *win* 10 0)
    (line *win* 0 -10)
    (line *win* -10 0)
    (line *win* 0 10)
    (let ((polygon (get-polygon *win*)))
      (fill-polygon *win* *black-pattern* polygon)
      (kill-polygon polygon))))

(defmethod view-draw-contents ((view (eql *view*))) 
  (start-polygon *view*)
  (move-to view 0 0)
  (line view 10 10)
  (line view -10 10)
  ;(line-to view 10 0)
  ;(line-to view 2 0)
  (let ((polygon (get-polygon view)))
    (fill-polygon view *black-pattern* polygon)
    (kill-polygon polygon)))

(add-subviews *win* *view*)

