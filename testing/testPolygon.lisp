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

(defparameter *new-view* nil)

(defclass polygon-view (simple-view)
  ((polygon :accessor polygon)))

(setf *new-view* (make-instance 'polygon-view
                                :view-size (make-point 50 50)
                                :view-position (make-point 20 50)))

(defmethod view-draw-contents ((self (eql *new-view*)))
  (with-focused-view *win*
    (with-fore-color (get-fore-color self)
      (paint-polygon *win* (polygon self)))))

(with-focused-view *win*
  (move-to *new-view* 100 0)
  (start-polygon *new-view*)
  (line *new-view* 110 0)
  (move-to *new-view* 110 0)
  (line *new-view* 110 10)
  (move-to *new-view* 110 10)
  (line *new-view* 100 10)
  (move-to *new-view* 100 10)
  (line *new-view* 100 0)
  (move-to *new-view* 100 0)
  (setf (polygon *new-view*) (get-polygon *win*)))

(sleep 2)
(add-subviews *win* *new-view*)
#|
(inspect *new-view*)
(invalidate-view *win*)
|#
