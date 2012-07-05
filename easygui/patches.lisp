(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :cocoa)
  (require :easygui))

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
    ;; todo: check point is inside bounds, lest negative coords
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

; Overwriting this method; patching it so that the view-size slot is initialized after 
; the view is drawn, if it wasn't already.
(defmethod easygui::add-1-subview :around ((view easygui:view) (super-view easygui:view))
  "Correctly initialize view positions"
  (call-next-method)
  (with-slots (easygui::position easygui::size easygui::frame-inited-p) view
    (unless easygui::frame-inited-p
      (setf easygui::frame-inited-p t)
      (easygui::running-on-this-thread ()
                              (let ((cocoa-view (cocoa-ref view)))
                                (dcc (#/setFrameOrigin: cocoa-view (easygui::ns-point-from-point easygui::position)))
                                (if (slot-boundp view 'easygui::size)
                                  (dcc (#/setFrameSize: cocoa-view (easygui::ns-point-from-point easygui::size)))
                                  (dcc (#/sizeToFit cocoa-view))))))
    ; Section for the patched code. I would have liked to have done this with an :after method, but I don't think a specialied class can
    ; get :around an :around method.
    (unless (slot-boundp view 'easygui::size)
      (let ((frame (#/frame (cocoa-ref view))))
        (setf (slot-value view 'easygui::size)
              (easygui:point (ns:ns-rect-width frame)
                          (ns:ns-rect-height frame)))))
    (easygui::set-needs-display view t)
    (unless (easygui::view-subviews-busy super-view) (easygui::set-needs-display super-view t))))

(defclass easygui::content-view-mixin ()
  ((easygui::content-view)
   (easygui::flipped :initarg :flipped :initform easygui::*screen-flipped*)
   (easygui::contained-view-specifically :initarg :contained-view-specifically :initform 'easygui::contained-view)))

(defmethod easygui::initialize-view :after ((view easygui::content-view-mixin))
  (unless (slot-boundp view 'easygui::content-view)
    (let ((containee (make-instance (slot-value view 'easygui::contained-view-specifically) 
                       :cocoa-ref (dcc (#/contentView (cocoa-ref view)))
                       :view-nick-name 'easygui::%CONTENT-OF-CONTENT-VIEW%
                       :flipped (slot-value view 'easygui::flipped))))
      (setf (slot-value view 'easygui::content-view) containee
            (slot-value containee 'easygui::parent) view))))










