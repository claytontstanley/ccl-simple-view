#+:digitool
(let ((*module-search-path* (reverse *module-search-path*)))
  (require :thermometer)
  (require :bootstrap-mcl))

#+:clozure
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass cocoa-thermometer (easygui::cocoa-extension-mixin ns:ns-level-indicator)
    ()
    (:metaclass ns:+ns-object))

  (defclass thermometer (view)
    ((thermometer-direction :reader thermometer-direction :initarg :thermometer-direction :initform :vertical)
     (pattern :initarg :pattern)
     (thermometer-value :reader thermometer-value :initarg :thermometer-value :initform 0)
     (max-value :initarg :max-value :initform 100
                :reader thermometer-max-value :writer (setf thermometer-max-value-slot)))
    (:default-initargs
      :specifically 'cocoa-thermometer
      :fore-color (color-symbol->system-color 'black)))

  (defmethod (setf thermometer-direction) (thermometer-direction (self thermometer))
    (unwind-protect (setf (slot-value self 'thermometer-direction) thermometer-direction)
      (#/setBoundsRotation: (cocoa-ref self)
       (ecase thermometer-direction
         (:horizontal 0.0)
         (:vertical 90.0))))
    (easygui::set-needs-display self t))

  (defmethod (setf thermometer-max-value) (max-value (self thermometer))
    (unwind-protect (setf (thermometer-max-value-slot self)  max-value)
      (#/setMaxValue: (cocoa-ref self) (coerce max-value 'double-float))))

  (defmethod (setf thermometer-value) (value (self thermometer))
    (unwind-protect (setf (slot-value self 'thermometer-value) value)
      (#/setDoubleValue: (cocoa-ref self) (coerce value 'double-float))))

  (defmethod initialize-instance :after ((view thermometer) &key)
    (#/setLevelIndicatorStyle: (#/cell (cocoa-ref view))
     #$NSContinuousCapacityLevelIndicatorStyle)
    (setf (thermometer-direction view) (thermometer-direction view))
    (setf (thermometer-value view) (thermometer-value view))
    (setf (thermometer-max-value view) (thermometer-max-value view)))

  ; I couldn't figure out how to change the color of the NSLevelIndicator object, so
  ; instead of forcing a default Cocoa object to be drawn how I want, just extend the
  ; class and use a custom drawing method for the thermometer.

  (objc:defmethod (#/drawRect: :void) ((self cocoa-thermometer) (rect :<NSR>ect))
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
                                                                 (thermometer-max-value view))))
                                           ; Due to how the NSLevelIndicator is drawn, width of the cocoa object will always be the
                                           ; dimension of the value of the thermometer, so no case statement is necessary here to figure
                                           ; out if the thermometer is being displayed horizontally or vertically. This is a nicety from having
                                           ; #/bounds and #/frame attributes for Cocoa objects.
                                           (with-fore-color (get-fore-color view)
                                                            (paint-rect view
                                                                        point-x
                                                                        point-y
                                                                        (+ point-x (* width fraction-full))
                                                                        (+ point-y height))))))))

  (eval-when (:compile-toplevel :load-toplevel :execute)
    (provide :thermometer))
  )
