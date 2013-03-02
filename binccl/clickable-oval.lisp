(defclass clickable-oval (view)
  ((draw-offset :accessor draw-offset :initarg :draw-offset :initform 1))
  (:default-initargs
    :view-size (make-point 20 20)
    :fore-color (color-symbol->system-color 'blue)))

(defmethod view-draw-contents ((view clickable-oval))
  (destructuring-bind (size-x size-y) (list (point-x (view-size view))
                                            (point-y (view-size view)))
    (with-slots (draw-offset) view
      (with-fore-color (get-fore-color view) 
        (frame-oval view
                    draw-offset
                    draw-offset
                    (- size-x draw-offset)
                    (- size-y draw-offset))))))

(defmethod view-click-event-handler ((view clickable-oval) where)
  (destructuring-bind (loc-pos-x loc-pos-y) (list (point-x where)
                                                  (point-y where))
    (destructuring-bind (glob-pos-x glob-pos-y) (list
                                                  (point-x (local-to-global view where))
                                                  (point-y (local-to-global view where)))
      ; And a totally hacky way to do it, but if you can't reference the view, or did not have the 'where' variable,
      ; here's how you can find the mouse's current position w.r.t the top window and w/o any variables
      (destructuring-bind (glob-pos-x-2 glob-pos-y-2) (list 
                                                        (point-x (view-mouse-position (get-front-window)))
                                                        (point-y (view-mouse-position (get-front-window))))
        (format t "loc-pos-x=~a~%" loc-pos-x)
        (format t "loc-pos-y=~a~%" loc-pos-y)
        (format t "glob-pos-x=~a~%" glob-pos-x)
        (format t "glob-pos-y=~a~%" glob-pos-y)
        (format t "glob-pos-x-2=~a~%" glob-pos-x-2)
        (format t "glob-pos-y-2=~a~%" glob-pos-y-2)
        (finish-output)
        ; Maybe get more complex here and check if 'where' is really within the circle,
        ; and not just within the view's boundary
        (beep)))))

; Test code

(awhen (get-front-window)
  (window-close it))

(make-instance
  'window
  :view-subviews
  (list
    (make-instance
      'clickable-oval
      :view-position (make-point 30 30))
    (make-instance
      'clickable-oval
      :view-size (make-point 10 50)
      :view-position (make-point 50 40)
      :fore-color (color-symbol->system-color 'red))))

