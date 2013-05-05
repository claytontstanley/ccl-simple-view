(defclass oval-view (view)
  ((draw-offset :accessor draw-offset :initarg :draw-offset :initform 1))
  (:default-initargs
    :view-size (make-point 20 20)
    :fore-color (color-symbol->system-color 'blue)))

(defmethod view-draw-contents ((view oval-view))
  (destructuring-bind (size-x size-y) (list (point-x (view-size view))
                                            (point-y (view-size view)))
    (with-slots (draw-offset) view
      (with-fore-color (get-fore-color view) 
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
  (print where)
  (finish-output)
  t)

; Test code

(awhen (front-window)
  (window-close it))

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
      :fore-color (color-symbol->system-color 'red))))

