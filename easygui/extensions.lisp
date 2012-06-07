(in-package :easygui)


; easygui by default starts position 0,0 at bottom left, going to the right and up for positive values
; ; This flips the screen vertically, so that it matches MCL's default. That is, position 0,0 is at top left
(setf *screen-flipped* t)

(defclass liner (drawing-view) ())

(defclass td-liner (liner) ())

(defclass bu-liner (liner) ())

(defmethod get-start ((view td-liner))
  (list 0 (easygui:point-y (view-size view))))

(defmethod get-start ((view bu-liner))
  (list 0 0))

(defmethod get-end ((view td-liner))
  (list (easygui:point-x (view-size view)) 0))

(defmethod get-end ((view bu-liner))
  (list (easygui:point-x (view-size view))
        (easygui:point-y (view-size view))))

;(#/redColor ns:ns-color)
;(easygui:make-rgb :red 255 :green 0 :blue 0)

(defmethod draw-view-rectangle ((view liner) rectangle)
  (declare (ignore rectangle))
  ;(#/set (#/blackColor ns:ns-color))
  (#/set (slot-value view 'foreground))
  (destructuring-bind (startx starty) (get-start view)
    (destructuring-bind (endx endy) (get-end view)
      (print startx)
      (#/strokeLineFromPoint:toPoint:
       ns:ns-bezier-path
       (ns:make-ns-point startx starty) 
       (ns:make-ns-point endx endy)))))
