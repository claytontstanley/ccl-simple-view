(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :cocoa)
  (require :easygui))

(shadowing-import 'easygui:cocoa-ref)
(shadowing-import 'easygui:dcc)
(shadowing-import 'easygui::running-on-main-thread)

(defun easygui::eg-point-from-ns-point (point)
  (easygui::point 
    (ns:ns-point-x point)
    (ns:ns-point-y point)))

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
              (make-point (ns:ns-rect-width frame)
                          (ns:ns-rect-height frame)))))
    (easygui::set-needs-display view t)
    (unless (easygui::view-subviews-busy super-view) (easygui::set-needs-display super-view t))))

; easygui by default starts position 0,0 at bottom left, going to the right and up for positive values
; This flips the screen vertically, so that it matches MCL's default. That is, position 0,0 is at top left
(setf easygui::*screen-flipped* t)

; ----------------------------------------------------------------------
; Extend the Objective C cocoa-drawing-view in the easygui package with a view that does not monitor mouse movement or clicks
;
; lisp->objective c class name mapping: drawing-overlay-view->cocoa-drawing-overlay-view
; ----------------------------------------------------------------------

; Create the objective c class
(defclass easygui::cocoa-drawing-overlay-view (easygui::cocoa-drawing-view)
  ()
  (:metaclass ns:+ns-object))

; And create the lisp equivalent class
; And register the objective c extension and lisp class to the easygui package, so that it instantiates a 
; cocoa-drawing-overlay-view object in the cocoa-ref slot when a drawing-overlay-view lisp object is instantiated
(defclass easygui::drawing-overlay-view (easygui::drawing-view)
  ()
  (:default-initargs :specifically 'easygui::cocoa-drawing-overlay-view))

; Add the hook method in objective c that will cause the new class to not respond to mouse activity
(objc:defmethod #/hitTest: ((self easygui::cocoa-drawing-overlay-view) (point :<NSP>oint))
  ccl:+null-ptr+)

; ----------------------------------------------------------------------
; Use a consuming-view class to say that all subviews within an instance of that class will not respond to mouse clicks. 
;
; This is to work around the differences in first responders between MCL and CCL. MCL looks down the view hierarchy for the first responder
; (breadth first), CCL looks down the hierarchy for the deepest responder (depth first). In order
; to simulate breadth first by stopping at a particular view in the tree (and not inspecting that view's
; subviews), create an instance of the consuming-view class. 
; ----------------------------------------------------------------------

(defclass easygui::cocoa-drawing-consuming-view (easygui::cocoa-drawing-view)
  ()
  (:metaclass ns:+ns-object))

(defclass easygui::drawing-consuming-view (easygui::drawing-view)
  ()
  (:default-initargs :specifically 'easygui::cocoa-drawing-consuming-view))

; Override hitTest; if a view (or one of its subviews) returns a non-nil value
; for the default hitTest call, then return self; this suppresses subviews of 
; self from responding to mouse clicks
; 
; Ref. this url for call-next-method syntax in objc:defmethod macro: 
; http://clozure.com/pipermail/openmcl-devel/2008-November/008645.html

(objc:defmethod #/hitTest: ((self easygui::cocoa-drawing-consuming-view) (point :<NSP>oint))
  (let ((ret (call-next-method point)))
    (if (not (equal ccl:+null-ptr+ ret))
      self
      ccl:+null-ptr+)))

; ----------------------------------------------------------------------
; Providing a view container to hold and display images.
; ----------------------------------------------------------------------

(defclass easygui::cocoa-image-view (easygui::cocoa-extension-mixin ns:ns-image-view)
  ()
  (:metaclass ns:+ns-object))

(defclass easygui::image-view (easygui::view)
  ()
  (:default-initargs :specifically 'easygui::cocoa-image-view))

; ----------------------------------------------------------------------
; Creating MCL's top-level simple-view class
;
; In order to implement MCL's top-level simple-view class, I needed a cocoa view class that was capable of drawing to the display
; (since simple-view can do this in MCL). Cocoa-drawing-view in easygui seemed like the appropriate class for this. However, the 
; default lisp class (drawing-view) for this class did a bit more than a top-level simple-view class should do. It tracks mouse movement, 
; which was a problem because a cocoa window class should not track movement, MCL's window class inherits from simple-view, so there were 
; many collisions with window class being a subclass of drawing-view. So the current fix is to define a simple-view class that inherits
; only from the easygui view top-level class, but associate that class with cocoa-drawing-view. So make-instance 'simple-view will
; create a simple-view object, and instantiate a cocoa-drawing-view object for that view. This allows window to be a subclass of simple-view,
; simple-view objects to draw to the display, and little code modification/extension to easygui since we're leveraging the objective c methods
; on cocoa-drawing-view.
; ----------------------------------------------------------------------

(defclass easygui::simple-view (easygui::view)
  ((easygui::flipped :initform easygui::*screen-flipped* :initarg :flipped :reader easygui::flipped-p))
  (:default-initargs :specifically 'easygui::cocoa-drawing-view))

; This section is the additional code required to have a simple-view object behave mostly like a drawing-view type object, 
; but without inheriting from drawing-view. Sort of a workaround to avoid the drawing-view mouse-tracking methods, since those aren't mixins (yet).

(defmethod easygui::link-cocoa-view ((cocoa-view ns:ns-view) view)
  ())

(defmethod easygui::link-cocoa-view ((cocoa-view ns:ns-window) view)
  ())

(defmethod easygui::link-cocoa-view ((cocoa-view easygui::cocoa-drawing-view) view)
  (setf (slot-value cocoa-view 'easygui::flipped) (slot-value view 'easygui::flipped))
  (setf (slot-value cocoa-view 'easygui::easygui-view) view))

(defmethod easygui::initialize-view :after ((view easygui::simple-view))
  (easygui::link-cocoa-view (cocoa-ref view) view))

