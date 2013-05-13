(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :cocoa)
  (require :easygui))

(defun easygui::point-from-ns-point (point)
  (easygui::point 
    (ns:ns-point-x point)
    (ns:ns-point-y point)))

; easygui by default starts position 0,0 at bottom left, going to the right and up for positive values
; This flips the screen vertically, so that it matches MCL's default. That is, position 0,0 is at top left
(setf easygui::*screen-flipped* t)


(setf easygui::*debug-cocoa-calls* nil)

; There are particular window configurations that keep the window from becoming key or main (borderless windows for example).
; And this causes odd behavior for these types of windows (can't select the win when using command `, window is backgrounded behind
; the listener window after a dialog window opens and closes).
;
; For the time being, there are no types of cocoa windows that should not be able to become key or main. So until customization is
; needed, override these methods for cocoa windows and allow everyone the ability to become key and main.

(objc:defmethod (#/canBecomeKeyWindow #>BOOL) ((self easygui::cocoa-window))
  #$YES)

(objc:defmethod (#/canBecomeMainWindow #>BOOL) ((self easygui::cocoa-window))
  #$YES)

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
(defclass easygui::overlay-view (easygui::view)
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

(defclass easygui::consuming-view (easygui::view)
  ()
  (:default-initargs :specifically 'easygui::cocoa-drawing-consuming-view))

; Override hitTest; if a view (or one of its subviews) returns a non-nil value
; for the default hitTest call, then return self; this suppresses subviews of 
; self from responding to mouse clicks
; 
; Reference this URL for call-next-method syntax in objc:defmethod macro: 
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

(defmethod easygui::initialize-view :after ((view easygui::image-view))
  (setf (slot-value (cocoa-ref view) 'easygui::easygui-view) view))

(defclass easygui::cocoa-clickable-image-view (easygui::cocoa-image-view)
  ()
  (:metaclass ns:+ns-object))

(easygui::define-useful-mouse-event-handling-routines easygui::cocoa-clickable-image-view)
(easygui::define-useful-mouse-event-handling-routines easygui::cocoa-contained-view)
(easygui::define-useful-mouse-event-handling-routines easygui::cocoa-mouseable-text-field)


(defmethod easygui::click-location ((cocoa-self ns:ns-view) (the-event ns:ns-event))
  (let* ((ns-point (#/locationInWindow the-event))
         (ns-converted-point (#/convertPoint:fromView: (#/superview cocoa-self)
                              ns-point
                              nil))
         (where (easygui::point-from-ns-point ns-converted-point)))
    where))

(objc:defmethod (#/mouseDown: :void) ((self easygui::cocoa-button) the-event)
  (call-next-method the-event)
  (easygui::mouse-down (easygui::easygui-view-of self)
                       :location (easygui::click-location self the-event)))


(defclass easygui::clickable-image-view (easygui::image-view)
  ()
  (:default-initargs :specifically 'easygui::cocoa-clickable-image-view))

; ----------------------------------------------------------------------
; Providing a mixin class that keeps a view from implicitly redrawing each
; time a subview is added to the display
; ----------------------------------------------------------------------

(defclass easygui::static-view-mixin ()
  ())

(defmethod easygui::set-needs-display ((view easygui::static-view-mixin) flag)
  (declare (ignore flag))
  (values))

(defmethod easygui::invalidate-view ((view easygui::static-view-mixin) &optional total)
  (declare (ignore total))
  (mapc #'easygui::invalidate-view (easygui:view-subviews view)))

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

(defmethod easygui::link-cocoa-view ((cocoa-view t) view)
  (declare (ignore view))
  (values))

(defmethod easygui::link-cocoa-view ((cocoa-view easygui::cocoa-extension-mixin) view)
  (setf (slot-value cocoa-view 'easygui::easygui-view) view))

(defmethod easygui::link-cocoa-view :after ((cocoa-view easygui::cocoa-drawing-view) view)
  (setf (slot-value cocoa-view 'easygui::flipped) (slot-value view 'easygui::flipped)))

(defmethod easygui::initialize-view :after ((view easygui::simple-view))
  (easygui::link-cocoa-view (easygui:cocoa-ref view) view))

; This keeps the setDrawsBackground attribute on the Cocoa object in sync with the 
; current background color (is it transparent or not).
(defmethod easygui:set-back-color :after ((view easygui::background-coloring-mixin) (new-color ns:ns-color) &optional redisplay-p)
  (setf (slot-value view 'easygui::drawsbackground) 
        (if (equal (#/clearColor ns:ns-color) new-color) nil t))
  (#/setDrawsBackground: (cocoa-ref view) (slot-value view 'easygui::drawsbackground))
  (when redisplay-p 
    (easygui:invalidate-view view)))

; Relay keypress events to the window, after allowing the text field to handle the keypress properly.
; Note that #/keyUp is used for the text-field, which calls #/keyDown. I could only get keyUp: to fire
; (not #/keyDown:) when typing in a text field, so that's why there's the discrepancy here.
(objc:defmethod (#/keyUp: :void) ((cocoa-self easygui::cocoa-text-field) the-event)
  (call-next-method the-event)
  (#/keyDown: (#/window cocoa-self) the-event)
  ) 

(defmethod easygui::cocoa-win-p ((win t))
  nil)

(defmethod easygui::cocoa-win-p ((win easygui::cocoa-window))
  (cond ((slot-boundp win 'easygui::easygui-window)
         t)
        (t
         (sv-log "not yet a cocoa win ~a" win)
         nil)))

(defmethod easygui::size-to-fit ((view easygui::view))
  (let ((frame (#/frame (cocoa-ref view))))
    (setf (slot-value view 'easygui::size)
          (easygui:point (ns:ns-rect-width frame)
                         (ns:ns-rect-height frame)))))

; Extending this method; patching it so that the view-size slot is initialized after 
; the view is drawn, if it wasn't already.
(defmethod easygui::add-1-subview :around ((view easygui::simple-view) (super-view easygui::simple-view))
  "Correctly initialize view positions"
  (unwind-protect (call-next-method)
    (with-slots (easygui::position easygui::size easygui::frame-inited-p) view
      (unless (slot-boundp view 'easygui::size)
        (easygui::size-to-fit view))
      (easygui::set-needs-display view t)
      (unless (easygui::view-subviews-busy super-view) (easygui::set-needs-display super-view t)))))

; Isolating the code to convert a vertical coordinate if the screen is flipped. Using just this part in ccl-simple-view.lisp
(defun easygui::convert-if-screen-flipped (y height)
  (if easygui::*screen-flipped*
    (- (easygui::screen-height) height y)
    y))

(defmethod easygui::window-hide ((window easygui::window))
  (easygui::running-on-this-thread ()
    (let ((cwin (cocoa-ref window)))
      (unless (easygui::window-hidden window)
        (setf (slot-value window 'easygui::hidden) t)
        (unless (dcc (#/isMiniaturized cwin))
          (dcc (#/miniaturize: cwin cwin))))
      (when (dcc (#/isFlushWindowDisabled cwin))
        (dcc (#/enableFlushWindow cwin))
        (dcc (#/flushWindow cwin)))
      window)))

(defclass easygui::cocoa-matrix (easygui::cocoa-extension-mixin ns:ns-form)
  ()
  (:metaclass ns:+ns-object))

(defun easygui::screen-width nil
  (easygui::running-on-this-thread ()
    (ns:ns-rect-width (dcc (#/frame (#/objectAtIndex: (#/screens ns:ns-screen) 0))))))
