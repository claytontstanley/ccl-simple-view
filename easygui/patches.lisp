(eval-when (:compile-toplevel :load-toplevel :execute)
  (shadowing-import 'easygui:cocoa-ref)
  (shadowing-import 'easygui:dcc)
  (shadowing-import 'easygui::running-on-main-thread))

; Providing a keyword argument to allow negative points; used for mouse coordinates
(defun easygui::point (x y &key (allow-negative-p nil))
  (unless allow-negative-p
    (assert (>= x 0))
    (assert (>= y 0)))
  (make-instance 'easygui::eg-point :x x :y y))

; Patching the function to provide a keyword argument that allows for negative mouse coordinates
(defun easygui::view-mouse-position (view &key (allow-negative-position-p nil))
  (let* ((w (cocoa-ref (easygui::easygui-window-of view)))
         (mouselocation (dcc (#/mouseLocationOutsideOfEventStream w)))
         (cview (if (typep view 'window) (easygui::content-view view) view))
         (nspt (dcc (#/convertPoint:fromView: (cocoa-ref cview) mouselocation NIL))))
    ;; TODO: check point is inside bounds, lest negative coordinates
    (easygui:point (ns:ns-point-x nspt)
                   (ns:ns-point-y nspt)
                   :allow-negative-p allow-negative-position-p)))

; I think I found a bug in these two methods in the easygui package, so redefining them here with correct setNeedsDisplay: call
(defmethod (setf easygui::view-position) (point (self easygui::view))
  (running-on-main-thread ()
    (setf (slot-value self 'easygui::position) point)
    (when (slot-value self 'easygui::frame-inited-p)
      (dcc (#/setFrame: (cocoa-ref self) (easygui::view-content-rect self)))
      (dcc (#/setNeedsDisplay: (cocoa-ref self) t)))))

(defmethod (setf easygui::view-size) (point (self easygui::view))
  (running-on-main-thread ()
    (setf (slot-value self 'easygui::size) point)
    (when (slot-value self 'easygui::frame-inited-p)
      (dcc (#/setFrame: (cocoa-ref self) (easygui::view-content-rect self)))
      (dcc (#/setNeedsDisplay: (cocoa-ref self) t)))))


; I wanted to instantiate my own extended contained-view class, but I didn't see an easy way to do this given the current
; easygui code. So adding a contained-view-specifically slot to the mixin class, defaulting it to the contained-view class
; defined in easygui. If you want to instantiate a different class for the contained view, just overwrite this default.
(defclass easygui::content-view-mixin ()
  ((easygui::content-view)
   (easygui::flipped :initarg :flipped :initform easygui::*screen-flipped*)
   (easygui::contained-view-specifically :initarg :contained-view-specifically :initform 'easygui::contained-view)))

; Added code to instantiate the contained view class that is stored as a slot on the mixin object
(defmethod easygui::initialize-view :after ((view easygui::content-view-mixin))
  (unless (slot-boundp view 'easygui::content-view)
    (let ((containee (make-instance (slot-value view 'easygui::contained-view-specifically) 
                                    :cocoa-ref (dcc (#/contentView (cocoa-ref view)))
                                    :view-nick-name 'easygui::%CONTENT-OF-CONTENT-VIEW%
                                    :flipped (slot-value view 'easygui::flipped))))
      (setf (slot-value view 'easygui::content-view) containee
            (slot-value containee 'easygui::parent) view))))
