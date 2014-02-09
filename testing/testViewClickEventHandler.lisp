; Bootstrap all needed packages (loads ACT-R, Cocoa framework, etc.)
(load (format nil "~a~a" (directory-namestring *load-truename*) "bootstrap.lisp"))

(defclass custom-button (button-dialog-item) ())

(defparameter *click-location* nil)

(defmethod view-click-event-handler ((view view) location)
  (setf *click-location* location))

(defclass custom-window (window) ())

(defparameter *win-click-location* nil)

(defmethod view-click-event-handler ((win custom-window) location)
  (setf *win-click-location* location))

(defun check-click-location ()
  (make-instance
    'custom-window
    :view-size (make-point 300 300)
    :view-subviews
    (list 
      (make-instance 'custom-button :view-position (make-point 10 10))
      (make-instance ; Also check that location is correct when buttons are nested in the view hierarchy
        'view
        :view-position (make-point 60 100)
        :view-size (make-point 100 100)
        :view-subviews
        (list (make-instance 'custom-button :view-position (make-point 20 20))))))
  (loop for (point expected) in (list (list (make-point 30 30) (make-point 20 20))  ; Check click on button as subview at top level 
                                      (list (make-point 90 130) (make-point 10 10)) ; Check click on nested button
                                      (list (make-point 1 1) (make-point 1 1)))     ; Check click on content-view of window
        do (left-mouse-click (add-points (view-position (front-window)) point))
        do (event-dispatch)
        do (check (equalp (as-list expected) (as-list *click-location*)))
        do (check (equalp (as-list point) (as-list *win-click-location*)))
        finally (window-close (front-window))))

(check-click-location)
