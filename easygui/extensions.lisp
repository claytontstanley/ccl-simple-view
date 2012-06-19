(in-package :easygui)

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
(defclass easygui::drawing-overlay-view (easygui::drawing-view)
  ())

; Add the hook method in objective c that will cause the new class to not respond to mouse activity
(objc:defmethod #/hitTest: ((self easygui::cocoa-drawing-overlay-view)
                            (point :<NSP>oint))
                ccl:+null-ptr+)

; and register the objective c extension and lisp class to the easygui package, so that it instantiates a 
; cocoa-drawing-overlay-view object in the cocoa-ref slot when a drawing-overlay-view lisp object is instantiated
(push (cons 'easygui::drawing-overlay-view 'easygui::cocoa-drawing-overlay-view) easygui::*view-class-to-ns-class-map*)

; In order to implement MCL's top-level simple-view, I needed an easygui class that inherited from ns-view, and not much else.
; For instance, using easygui's drawing view didn't work, b/c then I couldn't have MCL's window inherit from view, and I had other
; issues with the dialog types as well. I think the crux of the problem is the Cocoa's nswindow class does not inherit from nsview, but
; window is a view in MCL. So in order to keep the MCL hierarchy, and have it work with cocoa, I built my own top-level cocoa view, and started from there.

(defclass easygui::simple-view (easygui::view) ())

(defclass easygui::cocoa-simple-view (easygui::cocoa-extension-mixin ns:ns-view)
  ((easygui::flipped :initarg :flipped :initform easygui::*screen-flipped*))
  (:metaclass ns::+ns-object))

; Note that the *view-class-to-ns-class-map*, as implemented in easygui, is sort of an already-sorted class-precedence-list. So in order to keep from 
; all make-instance calls from creating the most general cocoa-simple-view objects, this class is placed at the end of *view-class-to-ns-class-map*. 

(setf easygui::*view-class-to-ns-class-map* 
      (nconc easygui::*view-class-to-ns-class-map*
             (list (cons 'easygui::simple-view 'easygui::cocoa-simple-view))))
