(require :cocoa)
(require :easygui)

; ----------------------------------------------------------------------
; Building class definitions to match MCL's GUI class heirarchy
;
; Most of the class definitions used by MCL are available in CCL using
; the easygui package. However, a few of the slot initargs in the easygui
; package do not match up with MCL initargs. So for these, use mixin
; classes that override initargs in easygui with initargs that match
; MCL's spec.
; ----------------------------------------------------------------------

(defclass view-text-via-title-mixin (easygui::view-text-via-title-mixin)
  ((easygui::text :initarg :window-title)))

(defclass view-text-via-stringvalue-mixin (easygui::view-text-via-stringvalue-mixin)
  ((easygui::text :initarg :text)))

(defclass view-mixin (easygui:view)
  ((easygui::size :initarg :view-size)
   (easygui::position :initarg :view-position)
   (easygui::foreground :initarg :color)))

(defclass easygui::cocoa-drawing-overlay-view (easygui::cocoa-drawing-view)
  ()
  (:metaclass ns:+ns-object))

(defclass easygui::drawing-overlay-view (easygui::drawing-view)
  ())

(objc:defmethod #/hitTest: ((self easygui::cocoa-drawing-overlay-view)
                            (point :<NSP>oint))
                ccl:+null-ptr+)

(push (cons 'easygui::drawing-overlay-view 'easygui::cocoa-drawing-overlay-view) easygui::*view-class-to-ns-class-map*)

(defclass simple-view (view-mixin easygui:drawing-view)
  ((pen-position :accessor pen-position :initarg :pen-position :initform (make-point 0 0)))
  (:documentation "Top-level class for views"))

(defclass simple-overlay-view (view-mixin easygui::drawing-overlay-view)
  ((pen-position :accessor pen-position :initarg :pen-position :initform (make-point 0 0)))
  (:documentation "Top-level class for views that do not monitor mouse clicks and mouse movement"))

(defclass color-dialog (view-text-via-title-mixin view-mixin easygui:window)
  ()
  (:documentation "Top-level class for windows"))

(defclass liner (simple-view) ())

(defclass td-liner (liner) ())

(defclass bu-liner (liner) ())

(defclass button-dialog-item (view-text-via-title-mixin view-mixin easygui::text-fonting-mixin easygui:push-button-view)
  ((easygui::default-button-p :initarg :default-button)))

(defclass static-text-dialog-item (view-text-via-stringvalue-mixin view-mixin easygui:static-text-view) ())

(defun make-dialog-item (class position size text &optional action &rest attributes)
  (apply #'make-instance 
         class
         :view-position position
         :view-size size
         :text text
         :action (lambda () (funcall action 'obj))
         attributes))

; ----------------------------------------------------------------------
; Building methods that allow CCL to understand basic MCL drawing commands

; Many of the functions/methods for basic MCL drawing are available in CCL's 
; easygui package. For the functions, import them into the current package.
; For the methods, add a generic method to the current CCL package
; that calls the generic method in the easygui package. Don't import the 
; generic functions from the easygui package, because this will cause 
; symbol collisions for the generic methods in the current package that are
; already defined (might be b/c they are an act-r interface method, or b/c
; they are an already-defined ccl method)
; ----------------------------------------------------------------------

(shadowing-import 'easygui:window)
(shadowing-import 'easygui:view)
(setf (symbol-function 'point-v) #'easygui:point-y)
(setf (symbol-function 'point-h) #'easygui:point-x)
(import 'easygui:point-x)
(import 'easygui:point-y)
(defparameter *black-color* 'black)
(defparameter *red-color* 'red)
(defparameter *light-gray-pattern* 'gray)

(defun make-point (x y)
  (make-instance 'easygui::eg-point :x x :y y))

(defmethod add-points ((p1 easygui::eg-point) (p2 easygui::eg-point))
  (make-point
    (+ (point-x p1) (point-x p2))
    (+ (point-y p1) (point-y p2))))

(defmethod add-subviews ((view view) &rest subviews)
  (when subviews
    (apply #'easygui:add-subviews view subviews)))

(defmethod remove-subviews ((view view) &rest subviews)
  (when subviews
    (apply #'easygui:remove-subviews view subviews)))

(defmethod subviews ((view view) &optional subview-type)
  (declare (ignore subview-type))
  (easygui:view-subviews view))

(defmethod window-select ((win window))
  (easygui:window-show win))

(defmethod window-close ((win window))
  (easygui:perform-close win))

(defmethod window-title ((view view))
  ;TODO: Maybe use easygui:view-text method here?
  (easygui::window-title view))

(defmethod dialog-item-text ((view easygui::view-text-mixin))
  (easygui:view-text view))

(defmethod view-position ((view easygui:view))
  (easygui:view-position view))

(defmethod set-view-position ((view easygui:view) x &optional (y nil))
  (let ((pos (if y
               (make-point x y)
               x)))
    (setf (easygui:view-position view) pos)))

(defmethod view-size ((view easygui:view))
  (easygui:view-size view))

(defmethod view-window ((view easygui:window))
  view)

(defmethod view-window ((view easygui:view))
  (awhen (easygui:view-container view)
    (view-window it)))

(defmethod view-container ((view easygui:view))
  (easygui:view-container view))

; ----------------------------------------------------------------------
; Other MCL drawing methods are not available in the easygui package.
; For these, move down a layer below easygui, and implement the functionality
; using CCL's Objective C bridge. Most bridge calls will have #/ or #_ reader
; macros in the expression
; ----------------------------------------------------------------------

(defmethod wptr ((view view))
  (#/isVisible (easygui::cocoa-ref view)))

(defmethod local-to-global ((view view) local-pos)
  (let ((view-pos (easygui:view-position view)))
    (make-point
      (+ (point-h view-pos) (point-h local-pos))
      (+ (point-v view-pos) (point-v local-pos)))))

(defmethod move-to ((view simple-view) position)
  (setf (pen-position view) position))

(defmethod line-to ((view simple-view) position)
  (destructuring-bind (startx starty) (list (point-x (pen-position view))
                                            (point-y (pen-position view)))
    (destructuring-bind (endx endy) (list (point-x position)
                                          (point-y position))
      ; TODO Use with-fore-color instead of set explicitly here
      (#/set (get-fore-color view))
      (#/strokeLineFromPoint:toPoint:
       ns:ns-bezier-path
       (ns:make-ns-point startx starty) 
       (ns:make-ns-point endx endy)))))

(defmacro with-fore-color (color &body body)
  `(progn
     ,@body))

(defmacro with-focused-view (view &body body)
  `(easygui:with-focused-view (easygui:cocoa-ref ,view)
     ,@body))

(defmethod get-fore-color ((view easygui:view))
  (easygui:get-fore-color view))

(defmethod part-color ((view easygui:static-text-view) part)
  (declare (ignore part))
  (get-fore-color view))

(defmethod color ((view easygui:view))
  (get-fore-color view))

(defmethod set-fore-color ((view easygui:view) new-color)
  (easygui:set-fore-color view new-color))

(defmethod set-part-color ((view easygui:view) part new-color)
  (declare (ignore part))
  (set-fore-color view new-color))

; Handling mouse movement/interaction

(defmethod easygui::mouse-down ((view easygui::drawing-view) &key location &allow-other-keys)
  (view-click-event-handler view location))

(defmethod view-click-event-handler ((device easygui:view) position)
  (awhen (easygui:view-container device) 
    (view-click-event-handler it position)))

(defmethod device-move-cursor-to ((device window) (xyloc vector))
  (setf xyloc (local-to-global device (vpt2p xyloc)))
  (#_CGWarpMouseCursorPosition (ns:make-ns-point (point-h xyloc)
                                                 (point-v xyloc))))

(defmethod view-mouse-position ((view view))
  (let* ((w (easygui:cocoa-ref (easygui::easygui-window-of view)))
         (mouselocation (easygui:dcc (#/mouseLocationOutsideOfEventStream w)))
         (cview (if (typep view 'window) (easygui:content-view view) view))
         (nspt (easygui:dcc (#/convertPoint:fromView: (easygui:cocoa-ref cview) mouselocation NIL))))
    (make-point (ns:ns-point-x nspt) (ns:ns-point-y nspt))))

(defun create-mouse-event (event pos)
  (#_CGEventCreateMouseEvent
   ccl:+null-ptr+
   event
   pos
   0))

(defun left-mouse-up (pos)
  (#_CGEventPost
   0
   (create-mouse-event #$NSLeftMouseUp pos)))

(defun left-mouse-down (pos)
  (#_CGEventPost
   0
   (create-mouse-event #$NSLeftMouseDown pos)))

(defun left-mouse-click (pos)
  (let ((pos (easygui::ns-point-from-point pos)))
    (left-mouse-down pos)
    (left-mouse-up pos)))


; Handling keyboard interaction

(defmethod easygui::view-key-event-handler ((device color-dialog) key)
  (view-key-event-handler device key))

(defmethod easygui::initialize-view :after ((window color-dialog))
  (let ((view (make-instance 'simple-view :accept-key-events-p t)))
    (setf (slot-value view 'easygui::parent) window)
    (setf (easygui::content-view window) view)
    (easygui::window-show window)))


; MCL's Pen

(defmethod pen-mode ((view easygui:view)) ())

(defmethod pen-pattern ((view easygui:view)) ())

(defmethod pen-size ((view easygui:view))
  (make-point 4 4))

(defmethod set-pen-mode ((view easygui:view) newmode)
  (declare (ignore newmode))
  ())

(defmethod set-pen-pattern ((view easygui:view) newpattern)
  (declare (ignore newpattern))
  ())

(defmethod set-pen-size ((view easygui:view) h &optional v)
  (declare (ignore h v))
  ())

; ----------------------------------------------------------------------
; Triggering MCL's view-draw-contents method on a Cocoa redraw of views.
;
; CCL's Objective C bridge provides an interface to define objective c 
; methods. Use this to define a method that will be called any time
; an object on the screen needs to be redrawn. This will in turn call
; view-draw-contents in CCL, which means that the way to describe how an 
; object is drawn in CCL is the same way that it is in MCL: Add a view-draw-contents
; method that dispatches on the object type, and code to draw that type of object
; to the display
; ----------------------------------------------------------------------

(objc:defmethod (#/drawRect: :void) ((self easygui::cocoa-drawing-view)
                                     (rect :<NSR>ect))
                (easygui::dcc (view-draw-contents (easygui::easygui-view-of self))))

; Drawing methods

(defmethod view-draw-contents ((view view))
  (declare (ignore view))
  ())

(defmethod get-start ((view bu-liner))
  (make-point 0 (point-y (view-size view))))

(defmethod get-start ((view td-liner))
  (make-point 0 0))

(defmethod get-end ((view bu-liner))
  (make-point (point-x (view-size view)) 0))

(defmethod get-end ((view td-liner))
  (view-size view))

(defmethod view-draw-contents ((view liner))
  (move-to view (get-start view))
  (line-to view (get-end view)))

(defmethod frame-oval ((view easygui:view) left &optional top right bottom)
  (assert (not right))
  (assert (not bottom))
  (assert left)
  (assert top)
  (destructuring-bind (startx starty) (list (point-x left) (point-y left))
    (destructuring-bind (width height) (list (point-x top) (point-y top))
      (let* ((rect (ns:make-ns-rect startx starty width height))
             (path (#/bezierPathWithOvalInRect: ns:ns-bezier-path rect)))
        (#/stroke path)))))

; Handling fonts and string width/height in pixels

(defmethod view-font ((view easygui::text-fonting-mixin))
  (#/font (easygui:cocoa-ref view)))

(defun font-info (font-spec)
  (values (#/ascender font-spec)
          (abs (#/descender font-spec))))

(defun string-width (str font)
  (let* ((dict (#/dictionaryWithObjectsAndKeys: ns:ns-mutable-dictionary
                font #$NSFontAttributeName 
                ccl:+null-ptr+))
         (attr (#/initWithString:attributes: (#/alloc ns:ns-attributed-string)
                (ccl::%make-nsstring str)
                dict))
         (size (#/size attr)))
    (ns:ns-size-width size)))

; Miscellaneous wrappers

(defun event-dispatch ()
  ())
