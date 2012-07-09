(defclass cocoa-level-indicator (easygui::cocoa-extension-mixin ns:ns-level-indicator)
  ()
  (:metaclass ns:+ns-object))

(defclass thermometer (view)
  ((direction :reader direction :initform :vertical)
   (pattern :initarg :pattern)
   (thermometer-value :reader thermometer-value :initform 0))
  (:default-initargs :specifically 'cocoa-level-indicator))

(defmethod (setf direction) (direction (self thermometer))
  (unwind-protect (setf (slot-value self 'direction) direction)
    (#/setBoundsRotation: (cocoa-ref self)
     (ecase direction
       (:horizontal 0.0)
       (:vertical 90.0))))
  (easygui::set-needs-display self t))

(defmethod initialize-instance :after ((view thermometer) &key)
  (#/setLevelIndicatorStyle: (#/cell (cocoa-ref view))
   #$NSContinuousCapacityLevelIndicatorStyle)
  (setf (direction view) (direction view))
  (setf (thermometer-value view) (thermometer-value view)))

(defmethod (setf thermometer-value) (value (self thermometer))
  (unwind-protect (setf (slot-value self 'thermometer-value) value)
    (#/setIntValue: (cocoa-ref self) value)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (provide :thermometer))
