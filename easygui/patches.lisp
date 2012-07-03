(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :cocoa)
  (require :easygui))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (shadowing-import 'easygui:cocoa-ref)
  (shadowing-import 'easygui:dcc)
  (shadowing-import 'easygui::running-on-main-thread))

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
; the view is drawn, if it wasn't already
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
