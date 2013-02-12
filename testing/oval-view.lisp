(defclass oval-view (simple-overlay-view)
  ((draw-offset :accessor draw-offset :initarg :draw-offset :initform 1)
   (oval-color :accessor oval-color :initarg :oval-color :initform (color-symbol->system-color 'blue)))
  (:default-initargs
    :view-size (make-point 20 20)))

(defmethod view-draw-contents ((view oval-view))
  (destructuring-bind (size-x size-y) (list (point-h (view-size view))
                                            (point-v (view-size view)))
    (with-slots (draw-offset) view
      (with-fore-color (oval-color view) 
                       (frame-oval view
                                   draw-offset
                                   draw-offset
                                   (- size-x draw-offset)
                                   (- size-y draw-offset))))))

(defmethod view-click-event-handler ((window window) where)
  (when (some (lambda (view)
                (within-oval-p view where))
              (view-subviews window))
    (beep)))

(defmethod within-oval-p ((view oval-view) where)
  (format t "x,y: ~a, ~a~%"
          (point-h where)
          (point-v where))
  (finish-output)
  t)

; Test code

;(awhen (get-front-window)
;  (window-close it))

(make-instance
  'window
  :view-subviews
  (list
    (make-instance
      'oval-view
      :view-position (make-point 30 30))
    (make-instance
      'oval-view
      :view-size (make-point 10 50)
      :view-position (make-point 50 40)
      :oval-color (color-symbol->system-color 'red))))


