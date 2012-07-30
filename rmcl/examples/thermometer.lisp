(defclass cocoa-level-indicator (easygui::cocoa-extension-mixin ns:ns-level-indicator)
  ()
  (:metaclass ns:+ns-object))

(defclass thermometer (view)
  ((direction :reader direction :initarg :direction :initform :vertical)
   (pattern :initarg :pattern)
   (thermometer-value :reader thermometer-value :initarg :thermometer-value :initform 0)
   (max-value :reader max-value :initarg :max-value :initform 100))
  (:default-initargs
    :specifically 'cocoa-level-indicator
    :fore-color (color-symbol->system-color 'black)))

(defmethod (setf direction) (direction (self thermometer))
  (unwind-protect (setf (slot-value self 'direction) direction)
    (#/setBoundsRotation: (cocoa-ref self)
     (ecase direction
       (:horizontal 0.0)
       (:vertical 90.0))))
  (easygui::set-needs-display self t))

(defmethod (setf max-value) (max-value (self thermometer))
  (unwind-protect (setf (slot-value self 'max-value) max-value)
    (#/setMaxValue: (cocoa-ref self) (coerce max-value 'double-float))))

(defmethod (setf thermometer-value) (value (self thermometer))
  (unwind-protect (setf (slot-value self 'thermometer-value) value)
    (#/setDoubleValue: (cocoa-ref self) (coerce value 'double-float))))

(defmethod initialize-instance :after ((view thermometer) &key)
  (#/setLevelIndicatorStyle: (#/cell (cocoa-ref view))
   #$NSContinuousCapacityLevelIndicatorStyle)
  (setf (direction view) (direction view))
  (setf (thermometer-value view) (thermometer-value view))
  (setf (max-value view) (max-value view)))

(objc:defmethod (#/drawRect: :void) ((self cocoa-level-indicator) (rect :<NSR>ect))
  (let ((view (easygui::easygui-view-of self))
        (bounds (#/bounds self)))
    (destructuring-bind (point-x point-y width height) (list (ns:ns-rect-x bounds)
                                                             (ns:ns-rect-y bounds)
                                                             (ns:ns-rect-width bounds)
                                                             (ns:ns-rect-height bounds))
      (with-focused-view view
        (with-fore-color (get-fore-color view)
          (frame-rect view point-x point-y (+ point-x width) (+ point-y height)))
        (let ((fraction-full (/ (thermometer-value view)
                                (max-value view))))
          (with-fore-color (get-fore-color view)
            (paint-rect view
                        point-x
                        point-y
                        (+ point-x (* width fraction-full))
                        (+ point-y height))))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (provide :thermometer))
