

(defclass custom-button (button-dialog-item) ())

(defparameter *click-location* nil)

(defmethod view-click-event-handler ((view custom-button) location)
  (setf *click-location* location))

(defun check-click-location ()
  (make-instance
    'window
    :view-size (make-point 300 300)
    :view-subviews
    (list 
      (make-instance 'custom-button :view-position (make-point 0 0))
      (make-instance ; Also check that location is correct when buttons are nested in the view hierarchy
        'view
        :view-position (make-point 60 100)
        :view-size (make-point 100 100)
        :view-subviews
        (list (make-instance 'custom-button :view-position (make-point 20 20))))))
  (loop for point in (list (make-point 20 20) (make-point 90 130))
        do (left-mouse-click (add-points (view-position (front-window)) point))
        do (check (equalp (as-list point) (as-list *click-location*)))
        finally (window-close (front-window))))

(check-click-location)
