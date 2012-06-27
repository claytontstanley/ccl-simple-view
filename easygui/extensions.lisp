(in-package :easygui)

(defun eg-point-from-ns-point (point)
  (easygui::point 
    (ns:ns-point-x point)
    (ns:ns-point-y point)))

; I think I found a bug in these two methods in the easygui package, so redefining them here with correct setNeedsDisplay: call
(defmethod (setf view-position) (point (self view))
  (running-on-main-thread ()
    (setf (slot-value self 'position) point)
    (when (slot-value self 'frame-inited-p)
      (dcc (#/setFrame: (cocoa-ref self) (view-content-rect self)))
      (dcc (#/setNeedsDisplay: (cocoa-ref self) t)))))

(defmethod (setf view-size) (point (self view))
  (running-on-main-thread ()
    (setf (slot-value self 'size) point)
    (when (slot-value self 'frame-inited-p)
      (dcc (#/setFrame: (cocoa-ref self) (view-content-rect self)))
      (dcc (#/setNeedsDisplay: (cocoa-ref self) t)))))

; easygui by default starts position 0,0 at bottom left, going to the right and up for positive values
; This flips the screen vertically, so that it matches MCL's default. That is, position 0,0 is at top left
(setf *screen-flipped* t)

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

; Use a consuming-view class to say that all subviews within an instance
; of that class will not respond to mouse clicks. This is to work around the differences
; in first responders between MCL and CCL. MCL looks down the view hierarchy for the first responder
; (breadth first), CCL looks down the hierarchy for the deepest responder (depth first). In order
; to simulate breadth first by stopping at a particular view in the tree (and not inspecting that view's
; subviews), create an instance of the consuming-view class. 

(defclass cocoa-drawing-consuming-view (cocoa-drawing-view)
  ()
  (:metaclass ns:+ns-object))

(defclass drawing-consuming-view (easygui::drawing-view)
  ()
  (:default-initargs :specifically 'easygui::cocoa-drawing-consuming-view))

; Override hitTest; if a view (or one of its subviews) returns a non-nil value
; for the default hitTest call, then return self; this suppresses subviews of 
; self from responding to mouse clicks
; 
; Ref. this url for call-next-method syntax in objc:defmethod macro: 
; http://clozure.com/pipermail/openmcl-devel/2008-November/008645.html

(objc:defmethod #/hitTest: ((self cocoa-drawing-consuming-view) (point :<NSP>oint))
  (let ((ret (call-next-method point)))
    (if (not (equal ccl:+null-ptr+ ret))
      self
      ccl:+null-ptr+)))

; Providing a view container to hold and display images.

(defclass cocoa-image-view (cocoa-extension-mixin ns:ns-image-view)
  ()
  (:metaclass ns:+ns-object))

(defclass image-view (easygui::view)
  ()
  (:default-initargs :specifically 'easygui::cocoa-image-view))

; In order to implement MCL's top-level simple-view class, I needed a cocoa view class that was capable of drawing to the display
; (since simple-view can do this in MCL). Cocoa-drawing-view in easygui seemed like the appropriate class for this. However, the 
; default lisp class (drawing-view) for this class did a bit more than a top-level simple-view class should do. It tracks mouse movement, 
; which was a problem because a cocoa window class should not track movement, MCL's window class inherits from simple-view, so there were 
; many collisions with window class being a subclass of drawing-view. So the current fix is to define a simple-view class that inherits
; only from the easygui view top-level class, but associate that class with cocoa-drawing-view. So make-instance 'simple-view will
; create a simple-view object, and instantiate a cocoa-drawing-view object for that view. This allows window to be a subclass of simple-view,
; simple-view objects to draw to the display, and little code modification/extension to easygui since we're leveraging the objective c methods
; on cocoa-drawing-view.

(defclass easygui::simple-view (easygui::view)
  ((flipped :initform *screen-flipped* :initarg :flipped :reader flipped-p))
  (:default-initargs :specifically 'easygui::cocoa-drawing-view))

; This section is the additional code required to have a simple-view object behave mostly like a drawing-view type object, 
; but without inheriting from drawing-view. Sort of a workaround to avoid the drawing-view mouse-tracking methods, since those aren't mixins (yet).

(defmethod link-cocoa-view ((cocoa-view ns:ns-view) view)
  ())

(defmethod link-cocoa-view ((cocoa-view ns:ns-window) view)
  ())

(defmethod link-cocoa-view ((cocoa-view cocoa-drawing-view) view)
  (setf (slot-value cocoa-view 'flipped) (slot-value view 'flipped))
  (setf (slot-value cocoa-view 'easygui-view) view))

(defmethod easygui::initialize-view :after ((view easygui::simple-view))
  (link-cocoa-view (cocoa-ref view) view))

