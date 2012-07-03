(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :cocoa)
  (require :easygui)
  (require :resources)
  (require :mcl-ccl-colors))

; ----------------------------------------------------------------------
; Building class definitions to match MCL's GUI class heirarchy
;
; Most of the class definitions used by MCL are available in CCL using
; the easygui package. However, a few of the slot initargs in the easygui
; package do not match up with MCL initargs. So for these, use mixin
; classes that override initargs in easygui with initargs that match
; MCL's spec.
; ----------------------------------------------------------------------

(defconstant $tejustleft :left)
(defconstant $tejustcenter :center)
(defconstant $tejustright :right)

(defclass view-text-via-title-mixin (easygui::view-text-via-title-mixin)
  ((easygui::text :initarg :window-title)))

(defclass view-text-via-stringvalue-mixin (easygui::view-text-via-stringvalue-mixin)
  ((easygui::text :initarg :text)))

(defclass view-text-mixin (easygui::view-text-mixin)
  ((text-justification :accessor text-justification :initarg :text-justification :initform $tejustleft)))

;((easygui::size :initarg :view-size :initform (make-point 100 100))
(defclass view-mixin (easygui:view)
  ((easygui::size :initarg :view-size)
   (easygui::position :initarg :view-position :initform (make-point 0 0))
   (temp-view-subviews :initarg :view-subviews)
   (easygui::foreground :initform (color-symbol->system-color 'black))
   (easygui::background :initform (color-symbol->system-color 'white))))

; Try to keep the class hierarchy of the public interface the same as it is for MCL.
; So, simple-view is top; then view (allows subviews); then types that inherit from view,
; like, window, dialog stuff, etc.

(defclass simple-view (easygui::simple-view view-mixin)
  ((pen-position :accessor pen-position :initarg :pen-position :initform (make-point 0 0))))

; TODO: Use the MOP to remove this dup.

(defmethod view-default-size ((view simple-view))
  (make-point 100 100))

(defmethod initialize-instance :around ((view simple-view) &rest args &key back-color)
  (if back-color
    (apply #'call-next-method view
         :back-color (mcl-color->system-color back-color)
         args)
    (call-next-method)))

#|
(defmethod initialize-instance :after ((view simple-view) &key)
  (let ((rect (easygui::view-content-rect view)))
    (destructuring-bind (x y width height) (list (ns:ns-rect-x rect)
                                                 (ns:ns-rect-y rect)
                                                 (ns:ns-rect-width rect)
                                                 (ns:ns-rect-height rect))
      (unless (slot-boundp view 'easygui::position)
        (set-view-position view x y))
      (unless (slot-boundp view 'easygui::size)
        (set-view-size view width height)))))|#

(defclass view (simple-view)
  ()
  (:documentation "Top-level class for views"))

(defclass window (easygui:window view-text-via-title-mixin view)
  ((grow-icon-p :initform nil :initarg :grow-icon-p :reader grow-icon-p)
   (grow-box-p :initarg :grow-box-p)
   (theme-background :initarg :theme-background)
   (window-show :initarg :window-show)
   (window-type :initarg :window-type)
   (close-box-p :accessor close-box-p :initarg :close-box-p :initform t))
  (:default-initargs :view-position (make-point 200 200)))

(defclass windoid (window) ())

(defclass simple-overlay-view (easygui::drawing-overlay-view view) 
  ()
  (:documentation "Top-level class for views that do not monitor mouse clicks and mouse movement"))

(defclass consuming-view (easygui::drawing-consuming-view view)
  ())

(defclass color-dialog (window)
  ()
  (:documentation "Top-level class for windows"))

(defclass liner (simple-view)
  ((easygui::foreground :reader color :initarg :color)))

(defclass td-liner (liner) ())

(defclass bu-liner (liner) ())

(defclass dialog (window)
  ()
  (:default-initargs
    :window-title "Untitled Dialog"
    :window-type :document))

(defclass dialog-item (view view-text-mixin)  ())

; Note that the :specifically initarg says what cocoa view class to associate with an instance of the object. 
; These really should have been specified in the easygui package, alongside each easygui class definition IMHO, but they weren't.
; Most of the easygui package uses a global easygui::*view-class-to-ns-class-map* variable that contains mappings of lisp
; classes to cocoa view classes, but I found using this flat mapping to be problematic with clos hierarchies. 
; Easygui also provides a :specifically method to overrule the easygui::*view-class-to-ns-class-map* variable, and I like this better, 
; so I'm using it. The benefits of the :specifically method are: 
; [1] cocoa view class mappings are explicitly written, and contained within each clos class definition. 
; [2] As the clos classes are extended, the :specifically values are inherited/over-ridden in the usual way.

(defclass button-dialog-item (easygui:push-button-view view-text-via-title-mixin easygui::text-fonting-mixin dialog-item)
  ((easygui::default-button-p :initarg :default-button)
   (cancel-button :initarg :cancel-button))
  (:default-initargs :specifically 'easygui::cocoa-button))

(defclass default-button-dialog-item (button-dialog-item)
  ()
  (:default-initargs :dialog-item-text "OK" :default-button t :cancel-button nil))

(defclass static-text-dialog-item (easygui:static-text-view view-text-via-stringvalue-mixin dialog-item)
  ((part-color-list :reader part-color-list :initarg :part-color-list)
   (bordered-p :reader bordered-p)
   (text-truncation :initarg :text-truncation))
  (:default-initargs :specifically 'easygui::cocoa-mouseable-text-field))

(defmethod (setf bordered-p) (bordered-p (view static-text-dialog-item))
  (unwind-protect (setf (slot-value view 'bordered-p) bordered-p)
    (#/setBordered: (easygui:cocoa-ref view) (if bordered-p #$YES #$NO))))

; FIXME: part-color-list and foreground/background slots should all remain in sync; how does MCL achieve this cleanly?

(defmethod initialize-instance :after ((view static-text-dialog-item) &key)
  (when (slot-boundp view 'part-color-list)
    (loop for (part color) in (group (part-color-list view) 2)
          do (set-part-color view part (mcl-color->system-color color)))))

(defclass editable-text-dialog-item (easygui:text-input-view view-text-via-stringvalue-mixin easygui::action-view-mixin dialog-item)
  ((allow-returns :initarg :allow-returns)
   (draw-outline :initarg :draw-outline))
  (:default-initargs :specifically 'easygui::cocoa-text-field))

(defclass radio-button-dialog-item (easygui:radio-button-view view-text-via-title-mixin dialog-item)
  ((easygui::cluster :initarg :radio-button-cluster))
  (:default-initargs :specifically 'easygui::cocoa-button))

(defclass check-box-dialog-item (easygui:check-box-view view-text-via-title-mixin dialog-item)
  ((easygui::text :initform ""))
  (:default-initargs :specifically 'easygui::cocoa-button))

; FIXME: what's this view-text hack?

(defclass icon-dialog-item (easygui::image-view easygui::action-view-mixin dialog-item)
  ((icon :reader icon :initarg :icon)
   (easygui::view-text :accessor easygui::view-text :initarg :view-text)))

(defclass image-view (view-mixin easygui::image-view) ())

(defun convert-icon (icon)
  (#/iconForFileType: (#/sharedWorkspace ns:ns-workspace)
   (#_NSFileTypeForHFSTypeCode icon)))

(defclass image-view-mixin ()
  ((pict-id :reader pict-id :initarg :pict-id)
   (image-view :accessor image-view)))

(defmethod (setf pict-id) (pict-id (view image-view-mixin))
  (unwind-protect (setf (slot-value view 'pict-id) pict-id)
    (#/setImage: (easygui:cocoa-ref (image-view view)) (get-resource-val pict-id))))

(defmethod initialize-instance :after ((view image-view-mixin) &key)
  (let ((image-view (make-instance 'image-view
                                   :view-size (view-size view)
                                   :view-position (make-point 0 0))))
    (setf (image-view view) image-view)
    (add-subviews view image-view)
    (when (slot-boundp view 'pict-id)
      (#/setImage: (easygui:cocoa-ref image-view) (get-resource-val (pict-id view))))))

; Place all images in the background (behind all other views). Do this by
; specializing on the add-1-subview method in the easygui package. And call
; cocoa's method for adding a subview that is behind all other views

(defmethod easygui::add-1-subview ((view image-view) (super-view easygui::view))
  (setf (slot-value view 'easygui::parent) super-view)
  (push view (slot-value super-view 'easygui::subviews))
  (#/addSubview:positioned:relativeTo: 
   (easygui:cocoa-ref super-view) 
   (easygui:cocoa-ref view)
   #$NSWindowBelow
   nil))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (provide :icon-dialog-item))

(defclass thermometer (simple-view)
  ())

(eval-when (:compile-toplevel :load-toplevel :execute)
  (provide :thermometer))

(defun make-dialog-item (class position size text &optional action &rest attributes)
  ; easygui's action slot takes a lambda with zero arguments; mcl's action slots take a lambda 
  ; with the object/view as an argument. So to enable this feature in easygui, wrap the provided lambda
  ; in a closure that takes zero arguments. 
  ;
  ; To build the closure, allocate storage for a variable first, then set the value of that variable to the created 
  ; instance, but within that instance, use the reference to the value before the value is actually updated. 
  ; This technique is actually wrapped up in a macro called alet in Hoyte's book, but I'm not using the macro here.
  (let ((obj))
    (setf obj (apply #'make-instance 
                     class
                     :view-position position
                     :view-size size
                     :text text
                     :action (if action 
                               (lambda () (funcall action obj))
                               nil)
                     attributes))
    obj))

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

(setf (symbol-function 'point-v) #'easygui:point-y)
(setf (symbol-function 'point-h) #'easygui:point-x)
(shadowing-import 'easygui:point-x)
(shadowing-import 'easygui:point-y)

(ccl::register-character-name "UpArrow" #\U+F700)
(ccl::register-character-name "DownArrow" #\U+F701)
(ccl::register-character-name "BackArrow" #\U+F702)
(ccl::register-character-name "ForwardArrow" #\U+F703)
(defparameter *arrow-cursor* 'fixme)
(defparameter *black-pattern* 'fixme)

(defun make-point (x y)
  (easygui::point x y :allow-negative-p t))

(defmethod add-points ((p1 easygui::eg-point) (p2 easygui::eg-point))
  (make-point
    (+ (point-x p1) (point-x p2))
    (+ (point-y p1) (point-y p2))))

(defmethod subtract-points ((p1 easygui::eg-point) (p2 easygui::eg-point))
  (make-point
    (- (point-x p1) (point-x p2))
    (- (point-y p1) (point-y p2))))

(defmethod point-string ((point easygui::eg-point))
  (format nil "#@(~a ~a)" (point-x point) (point-y point)))

(defmethod add-subviews ((view view) &rest subviews)
  (when subviews
    (apply #'easygui:add-subviews view subviews)))

(defmethod remove-subviews ((view view) &rest subviews)
  (when subviews
    (apply #'easygui:remove-subviews view subviews)))

(defmethod subviews ((view view) &optional subview-type)
  (declare (ignore subview-type))
  (easygui:view-subviews view))

(defmethod view-subviews ((view easygui::view))
  (easygui:view-subviews view))

(defmethod view-named (name (view view))
  (easygui:view-named name view))

(defmethod view-nick-name ((view view))
  (easygui:view-nick-name view))

(defmethod window-select ((win window))
  (easygui:window-show win))

(defmethod window-show ((win window))
  (easygui:window-show win))

(defun ccl::window-bring-to-front (w &optional (wptr (wptr w)))
  (window-select w))

(defmethod set-window-layer ((window window) new-layer &optional include-invisibles)
  'fixme)

(defmethod window-close ((win window))
  (easygui:perform-close win))

(defmethod window-title ((view window))
  ;TODO: Maybe use easygui:view-text method here?
  (easygui::window-title view))

(defmethod dialog-item-text ((view easygui::view-text-mixin))
  (easygui:view-text view))

(defmethod text-just ((view view-text-mixin))
  (text-justification view))

(defun convert-justification (justification)
  (let ((mapping (list (cons $tejustleft #$NSLeftTextAlignment)
                       (cons $tejustcenter #$NSCenterTextAlignment)
                       (cons $tejustright #$NSRightTextAlignment))))
    (cdr (assoc justification mapping))))

(defmethod set-text-justification ((view view-text-mixin) justification)
  (#/setAlignment: (easygui:cocoa-ref view) (convert-justification justification))
  (setf (text-justification view) justification))

(defmethod initialize-instance :after ((view view-text-mixin) &key)
  (set-text-justification view (text-justification view)))

(defmethod set-dialog-item-text ((view easygui::view-text-mixin) text)
  (setf (easygui:view-text view) text))

(defmethod dialog-item-enable ((view easygui::action-view-mixin))
  (easygui:set-dialog-item-enabled-p view t))

(defmethod dialog-item-disable ((view easygui::action-view-mixin))
  (easygui:set-dialog-item-enabled-p view nil))

(defmethod check-box-check ((item check-box-dialog-item))
  (easygui:check-box-check item nil))

(defmethod check-box-uncheck ((item check-box-dialog-item))
  (easygui:check-box-uncheck item nil))

(defmethod check-box-checked-p ((item check-box-dialog-item))
  (easygui:check-box-checked-p item))

(defmethod radio-button-unpush ((item radio-button-dialog-item))
  (easygui:radio-button-deselect item))

(defmethod radio-button-push ((item radio-button-dialog-item))
  (easygui:radio-button-select item))

(defmethod view-position ((view simple-view))
  (easygui:view-position view))

(defun canonicalize-point (x y)
  (cond (y (list x y))
        (t (list (point-h x) (point-v x)))))

(defmethod set-view-position ((view simple-view) x &optional (y nil))
  (destructuring-bind (x y) (canonicalize-point x y)
    (let ((pos (make-point x y)))
      (setf (easygui:view-position view) pos))))

(defmethod set-view-size ((view simple-view) x &optional (y nil))
  (destructuring-bind (x y) (canonicalize-point x y)
    (let ((size (make-point x y)))
      (setf (easygui:view-size view) size))))

(defmethod view-size ((view simple-view))
  (easygui:view-size view))

(defmethod width ((view simple-view))
  (point-h (view-size view)))

(defmethod view-window ((view window))
  view)

(defmethod view-container ((view easygui:view))
  (easygui:view-container view))

(defmethod view-window ((view easygui:view))
  (awhen (view-container view)
    (view-window it)))

; Other MCL drawing methods are not available in the easygui package.
; For these, move down a layer below easygui, and implement the functionality
; using CCL's Objective C bridge. Most bridge calls will have #/ or #_ reader
; macros in the expression

; A few with-... macros to handle setup/teardown, and make programming a bit easier

; This one uses Doug Hoyte's defmacro! and ,g!... syntax to easily handle unwanted variable capture. 
(defmacro! with-graphics-context (&body body)
  "Any changes to the graphics environment by body, will be valid only in body"
  `(let ((,g!context (#/currentContext ns::ns-graphics-context)))
     (unwind-protect (progn 
                       (#/saveGraphicsState ,g!context)
                       ,@body)
       (#/restoreGraphicsState ,g!context))))

(defmacro with-fore-color (color &body body)
  `(with-graphics-context
     (#/set ,color)
     ,@body))

(defmacro with-focused-view (view &body body)
  "Any changes to the graphics environment by body will be directed to the view object"
  `(easygui:with-focused-view (easygui:cocoa-ref ,view)
     ,@body))

(defmethod wptr ((view window))
  (#/isVisible (easygui::cocoa-ref view)))

(defmethod local-to-global ((view simple-view) local-pos)
  (add-points (easygui:view-position view) local-pos))

(defmethod move-to ((view simple-view) x &optional (y nil))
  (destructuring-bind (x y) (canonicalize-point x y)
    (let ((position (make-point x y)))
      (setf (pen-position view) position))))

(defmethod line-to ((view simple-view) x &optional (y nil))
  (destructuring-bind (endx endy) (canonicalize-point x y)
    (destructuring-bind (startx starty) (list (point-x (pen-position view))
                                              (point-y (pen-position view)))
        (#/strokeLineFromPoint:toPoint:
         ns:ns-bezier-path
         (ns:make-ns-point startx starty) 
         (ns:make-ns-point endx endy)))))

(defmethod part-color ((view easygui:static-text-view) (part (eql :text)))
  (declare (ignore part))
  (get-fore-color view))

(defmethod set-part-color ((view static-text-dialog-item) (part (eql :body)) new-color)
  (set-back-color view new-color))

(defmethod set-part-color ((view static-text-dialog-item) (part (eql :text)) new-color)
  (set-fore-color view new-color))

(defmethod set-part-color ((view static-text-dialog-item) (part (eql :frame)) new-color)
  (setf (bordered-p view) t))

(defmethod get-fore-color ((view simple-view))
  (easygui:get-fore-color view))

(defmethod set-fore-color ((view simple-view) new-color)
  (easygui:set-fore-color view new-color))

(defmethod set-back-color ((view simple-view) new-color)
  (easygui:set-back-color view new-color))

; Handling mouse movement/interaction

(defmethod easygui::mouse-down ((view easygui::drawing-view) &key location &allow-other-keys)
  (view-click-event-handler view location))

(defmethod easygui::mouse-down ((view simple-view) &key location &allow-other-keys)
  (view-click-event-handler view location))

(defmethod window-update-cursor ((window window) point)
  nil)

(defmethod view-click-event-handler ((device easygui:view) position)
  (awhen (view-container device) 
    (view-click-event-handler it position)))

(defmethod device-move-cursor-to ((device window) (xyloc vector))
  (setf xyloc (local-to-global device (vpt2p xyloc)))
  (#_CGWarpMouseCursorPosition (ns:make-ns-point (point-h xyloc)
                                                 (point-v xyloc))))

(defmethod view-mouse-position ((view simple-view))
  (easygui:view-mouse-position view :allow-negative-position-p t))

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

; MCL's Pen

(defmethod pen-mode ((view simple-view)) ())

(defmethod pen-pattern ((view simple-view)) ())

(defmethod pen-size ((view simple-view))
  (make-point 4 4))

(defmethod set-pen-mode ((view simple-view) newmode)
  (declare (ignore newmode))
  ())

(defmethod set-pen-pattern ((view simple-view) newpattern)
  (declare (ignore newpattern))
  ())

(defmethod set-pen-size ((view simple-view) h &optional v)
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

(defmethod view-draw-contents ((view easygui:view))
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
  (with-fore-color (get-fore-color view) 
    (line-to view (get-end view))))

(defmethod frame-oval ((view simple-view) left &optional top right bottom)
  (assert (not right))
  (assert (not bottom))
  (assert left)
  (assert top)
  (destructuring-bind (startx starty) (list (point-x left) (point-y left))
    (destructuring-bind (width height) (list (point-x top) (point-y top))
      (let* ((rect (ns:make-ns-rect startx starty width height))
             (path (#/bezierPathWithOvalInRect: ns:ns-bezier-path rect)))
        (#/stroke path)))))

(defmethod frame-rect ((view simple-view) left &optional top right bottom)
  (assert (not right))
  (assert (not bottom))
  (assert left)
  (assert top)
  (destructuring-bind (startx starty) (list (point-x left) (point-y left))
    (destructuring-bind (width height) (list (point-x top) (point-y top))
      (let ((rect (ns:make-ns-rect startx starty width height)))
        (#/strokeRect: ns:ns-bezier-path
         rect)))))

(defmethod fill-rect ((view simple-view) pattern left &optional top right bottom)
  (let ((rect (ns:make-ns-rect left top right bottom)))
    (#/fillRect: ns:ns-bezier-path rect)))

; Handling fonts and string width/height in pixels

(defun convert-font (font)
  (etypecase font
    (ns:ns-font font)
    (list 
      (destructuring-bind (name pt &rest rest) font
        (#/fontWithName:size: ns:ns-font
         (objc:make-nsstring name)
         pt)))))

; easygui expects the font slot to be initialized with an ns-font type. However, MCL uses the
; same slot name and expects the font slot to be initialized with a font spec as a list.
; So in order to make it so that the font slot is correct for easygui, shadow the :view-font
; initarg if it is provided by the equivalent ns-font value
(defmethod initialize-instance :around ((view easygui::text-fonting-mixin) &rest args &key view-font)
  (if view-font
    (apply #'call-next-method view :view-font (convert-font view-font) args)
    (call-next-method)))

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

; MCL allows for subviews to be passed at object initialization. I tried shadowing the 'easygui::subviews :initargs symbol
; with :view-subviews, so that MCL code cleanly initialized easygui's subviews slot, but it turns out that this slot isn't always 
; where the subviews are supposed to go. If the view is a window, then the subviews go as subviews under the content-view slot.
; easygui handles all of this in their add-subviews method, so the technique here is to use a temp slot on the view-mixin class,
; make that :initarg :view-subviews, and then on object initialization, take any provided subviews and call easygui's add-subviews method
; on them. Then clear the temp slot. Again, kinda' hacky, but it seems to work, and requires minimal code additions and still uses
; easygui's add-subviews machinery, etc.

(defmethod initialize-instance :after ((view view-mixin) &key) 
  (when (slot-boundp view 'temp-view-subviews)
    (apply #'add-subviews view (slot-value view 'temp-view-subviews))
    (slot-makunbound view 'temp-view-subviews)))

; Mock up the :quickdraw package and place it on *modules*. Keeps from having to comment out the (require :quickdraw) lines in the MCL code
(defpackage quickdraw
  (:use "COMMON-LISP")
  (:nicknames :quickdraw))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (provide :quickdraw))

; nothing has broken yet with this one...
(defun event-dispatch ()
  ())

(defun choose-file-dialog (&key directory mac-file-type button-string prompt file)
  (gui::cocoa-choose-file-dialog :directory directory
                                 :file-types mac-file-type
                                 :file file
                                 :button-string button-string))

(defun choose-new-file-dialog (&key directory prompt button-string)
  (gui::cocoa-choose-new-file-dialog :directory directory))

(defun choose-directory-dialog (&key directory)
  (easygui:choose-directory-dialog :directory directory))

; ----------------------------------------------------------------------
; Manipulate the read table so that MCL's #@(a b) make-point shorthand works. 
;
; CCL does not support this by default, and the objective-c bridge has its own use for the
; #@ macro character, so note that no easygui/objective-c code should be loaded/read
; after this read-table mod is made. If this needs to be done, restore the readtable first
; ----------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *nonhacked-readtable* (copy-readtable))
  ; Code grabbed from RMCL, since MCL is now open-sourced (yay!)
  ; For reading #@(h v) as points.
  (set-dispatch-macro-character 
    #\# #\@
    (defun |#@-reader| (stream char arg)
      (declare (ignore arg char))
      (let ((list (read stream t nil t)))
        (unless *read-suppress*
          `(make-point ,@list))))))

(defun set-cursor (cursor)
  cursor)

; ----------------------------------------------------------------------
; Manipulate reader functionality so that references to foreign functions that no longer exist can
; be defined as native functions, while keeping the same access syntax
;
; I did not want to have to modify the source code in the Phaser task where all of these carbon foreign 
; functions were used. CCL does not support the carbon framework, as far as I can tell. So in order to 
; trick CCL into thinking that these foreign functions are defined, add a bit of a 'before' section of 
; code to the load-external-function call. If the symbol name of the external function being loaded is
; in the list of function names that are being defined natively, then just return the symbol that maps
; to that function in the funcion symbol table. Otherwise, call the usual load-external-funcion funcion,
; and have CCL do the standard thing to try to find the foreign function
; ----------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *load-external-function-orig* #'ccl::load-external-function)
  (with-continue 
    (defun ccl::load-external-function (sym query)
      (let* ((fun-names (list "showmenubar" "hidemenubar" "getcursor" "showcursor" "ShowCursor" "HideCursor"))
             (the-package (find-package :X86-Darwin64))
             (fun-syms (mapcar (lambda (name)
                                 (intern name the-package))
                               fun-names)))
        (if (member sym fun-syms)
          (return-from ccl::load-external-function sym)
          (funcall *load-external-function-orig* sym query))))))

  ; Use the same approach to define foreign constants that MCL uses that no longer exist for CCL

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *load-os-constant-orig* #'ccl::load-os-constant)
  (with-continue
    (defun ccl::load-os-constant (sym &optional query)
      (let* ((con-names (list "tejustleft" "tejustcenter" "tejustright"))
             (the-package (find-package :X86-Darwin64))
             (con-syms (mapcar (lambda (name)
                                 (intern name the-package))
                               con-names)))
        (if (member sym con-syms)
          (return-from ccl::load-os-constant sym)
          (funcall *load-os-constant-orig* sym query))))))

; All of the functions being natively defined are here

(defun X86-Darwin64::|getcursor| (num)
  num)

(defun X86-Darwin64::|showcursor| ()
  t)

(defun X86-Darwin64::|hidecursor| ()
  t)

(defun X86-Darwin64::|hidemenubar| ()
  t)

(defun X86-Darwin64::|showmenubar| ()
  t)

; And the constants are here
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant X86-Darwin64::|tejustleft| $tejustleft)
  (defconstant X86-Darwin64::|tejustcenter| $tejustcenter)
  (defconstant X86-Darwin64::|tejustright| $tejustright))

