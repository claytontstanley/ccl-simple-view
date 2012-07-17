(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :cocoa)
  (require :easygui)
  (require :resources)
  (require :mcl-ccl-colors))

; These are shorthand guard macros for usual cases. Only use these if you quickly want to add
; a guard statement with minimal useful error messages. Otherwise, use the guard macro and 
; provide a more meaningful error message

(defmacro guard-!null-ptr (&body body)
  `(guard ((not (equal it1 ccl:+null-ptr+)) "null ptr returned when evaling form ~a" ',body)
     (progn ,@body)))

(defmacro guard-!nil (&body body)
  `(guard (it1 "nil returned when evaling form ~a" ',body)
     (progn ,@body)))

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

(defclass view-mixin (easygui:view)
  ((easygui::size :initarg :view-size)
   (easygui::position :initarg :view-position :initform (make-point 0 0))
   (easygui::font :initform (second (parse-mcl-font-lst '("Monaco" 9 :SRCOR :PLAIN (:COLOR-INDEX 0)))))
   (temp-view-subviews :initarg :view-subviews)
   (easygui::foreground :initform (color-symbol->system-color 'black))
   (easygui::background :initform (#/clearColor ns:ns-color))))

; Try to keep the class hierarchy of the public interface the same as it is for MCL.
; So, simple-view is top; then view (allows subviews); then types that inherit from view,
; like, window, dialog stuff, etc.

(defclass simple-view (easygui::simple-view view-mixin output-stream)
  ((pen-position :accessor pen-position :initform (make-point 0 0))
   (bezier-path :accessor bezier-path :initform nil)))

(defmethod view-default-size ((view simple-view))
  (make-point 100 100))

(defun parse-mcl-back-color (back-color)
  (if back-color
    (list :back-color (mcl-color->system-color back-color))))

; easygui expects the font slot to be initialized with an ns-font type. However, MCL uses the
; same slot name and expects the font slot to be initialized with a font spec as a list.
; So in order to make it so that the font slot is correct for easygui, shadow the :view-font
; initarg if it is provided by the equivalent ns-font value

(defmethod initialize-instance :around ((view simple-view) &rest args &key back-color view-font)
  (if (or back-color view-font)
    (let ((view-font-lst (parse-mcl-font-lst view-font))
          (back-color-lst (parse-mcl-back-color back-color)))
      (apply #'call-next-method view (append view-font-lst back-color-lst args)))
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

(defclass contained-view (view easygui::contained-view) ())

(defclass static-view-mixin (easygui::static-view-mixin) ())

(defclass window (easygui:window view-text-via-title-mixin view)
  ((grow-icon-p :initform nil :initarg :grow-icon-p :reader grow-icon-p)
   (grow-box-p :initarg :grow-box-p)
   (theme-background :initarg :theme-background)
   (window-show :initarg :window-show)
   (window-type :initarg :window-type)
   (close-box-p :accessor close-box-p :initarg :close-box-p :initform t)
   (maintenance-thread :accessor maintenance-thread)
   (easygui::background :initform (color-symbol->system-color 'white)))
  (:default-initargs 
    :view-position (make-point 200 200)
    :view-size (make-point 200 200)
    :contained-view-specifically 'contained-view))

(defmethod initialize-instance :after ((win window) &key)
  (setf (maintenance-thread win)
        (process-run-function 
          (format nil "maintenance thread for win ~a" win)
          (lambda ()
            (while (wptr win)
              (sleep .1)
              (window-null-event-handler win))))))

(defmethod window-null-event-handler ((win window))
  ())

(defclass windoid (window) ())

(defclass simple-overlay-view (easygui::drawing-overlay-view view easygui::drawing-view) 
  ()
  (:documentation "Top-level class for views that do not monitor mouse clicks and mouse movement"))

(defclass consuming-view (easygui::drawing-consuming-view view easygui::drawing-view)
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

(defclass action-view-mixin (easygui::action-view-mixin) ())

(defclass dialog-item (view view-text-mixin action-view-mixin)  ())

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
  (:default-initargs :specifically 'easygui::cocoa-button :text-justification $tejustcenter))

(defclass default-button-dialog-item (button-dialog-item)
  ()
  (:default-initargs :dialog-item-text "OK" :default-button t :cancel-button nil))

(defclass static-text-dialog-item (easygui:static-text-view view-text-via-stringvalue-mixin dialog-item)
  ((part-color-list :reader part-color-list :initarg :part-color-list)
   (bordered-p :reader bordered-p)
   (text-truncation :initarg :text-truncation)
   (easygui::drawsbackground :initform nil))
  (:default-initargs :specifically 'easygui::cocoa-mouseable-text-field))

; Cocoa doesn't automatically determine the value of drawsbackground dependent on the background color.
; If the back color is clear, drawsbackground should be nil, otherwise t. So if a back-color is passed in
; as an initform, inform easygui that the background should be drawn by passing a t for :drawsbackground keyword.

(defmethod initialize-instance :around ((view easygui::background-coloring-mixin) &rest args &key back-color)
  (if back-color
    (apply #'call-next-method view :drawsbackground t args)
    (call-next-method)))

(defmethod (setf bordered-p) (bordered-p (view static-text-dialog-item))
  (unwind-protect (setf (slot-value view 'bordered-p) bordered-p)
    (#/setBordered: (easygui:cocoa-ref view) (if bordered-p #$YES #$NO))))

; FIXME: part-color-list and foreground/background slots should all remain in sync; how does MCL achieve this cleanly?

(defmethod initialize-instance :after ((view static-text-dialog-item) &key)
  (when (slot-boundp view 'part-color-list)
    (loop for (part color) in (group (part-color-list view) 2)
          do (set-part-color view part (mcl-color->system-color color)))))

(defclass editable-text-dialog-item (easygui:text-input-view view-text-via-stringvalue-mixin dialog-item)
  ((allow-returns :initarg :allow-returns)
   (draw-outline :initarg :draw-outline))
  (:default-initargs :specifically 'easygui::cocoa-text-field))

(defclass radio-button-dialog-item (easygui:radio-button-view view-text-via-title-mixin dialog-item)
  ((easygui::cluster :initarg :radio-button-cluster)
   (easygui::selected :initarg :radio-button-pushed-p))
  (:default-initargs :specifically 'easygui::cocoa-button))

(defclass check-box-dialog-item (easygui:check-box-view view-text-via-title-mixin dialog-item)
  ((easygui::text :initform ""))
  (:default-initargs :specifically 'easygui::cocoa-button))

(defclass image-view (easygui::image-view view) ())

(defclass clickable-image-view (easygui::clickable-image-view image-view) ())

(defclass back-image-view (image-view) ())

; FIXME: what's this view-text hack?

(defclass icon-dialog-item (clickable-image-view dialog-item view)
  ((icon :reader icon :initarg :icon)
   (easygui::view-text :accessor easygui::view-text :initarg :view-text)))

#|(defun convert-icon (icon)
    (guard-!null-ptr
      (#/iconForFileType: (#/sharedWorkspace ns:ns-workspace)
       (#_NSFileTypeForHFSTypeCode icon))))|#

#|
(defun convert-icon (icon)
  (#/initWithIconRef: ns:ns-image
   icon))
|#

(defun icon->pict-id (icon)
  (format nil "~a" icon))

(defmethod initialize-instance :after ((view icon-dialog-item) &key)
  (when (slot-boundp view 'icon)
    (#/setImage: (easygui:cocoa-ref view)
     (get-resource-val (icon->pict-id (icon view))))))

(defclass image-view-mixin ()
  ((pict-id :reader pict-id :initarg :pict-id)
   (image-view :accessor image-view)))

(defmethod (setf pict-id) (pict-id (view image-view-mixin))
  (unwind-protect (setf (slot-value view 'pict-id) pict-id)
    (#/setImage: (easygui:cocoa-ref (image-view view)) (get-resource-val pict-id))))

(defmethod initialize-instance :after ((view image-view-mixin) &key)
  (let ((image-view (make-instance 'back-image-view
                                   :view-size (view-size view)
                                   :view-position (make-point 0 0))))
    (setf (image-view view) image-view)
    (add-subviews view image-view)
    (when (slot-boundp view 'pict-id)
      (#/setImage: (easygui:cocoa-ref image-view) (get-resource-val (pict-id view))))))

; Place all images in the background (behind all other views). Do this by
; specializing on the add-1-subview method in the easygui package. And call
; cocoa's method for adding a subview that is behind all other views

(defmethod easygui::add-1-subview ((view back-image-view) (super-view view))
  (setf (slot-value view 'easygui::parent) super-view)
  (push view (slot-value super-view 'easygui::subviews))
  (#/addSubview:positioned:relativeTo: 
   (easygui:cocoa-ref super-view) 
   (easygui:cocoa-ref view)
   #$NSWindowBelow
   nil))

(defmethod easygui::add-1-subview :after ((view image-view) (super-view view))
  (unless (slot-boundp view 'easygui::size)
    (let ((ns-size (#/size (#/image (cocoa-ref view)))))
      (destructuring-bind (width height) (list
                                           (ns:ns-size-width ns-size)
                                           (ns:ns-size-height ns-size))
        (setf (slot-value view 'easygui::size) (make-point width height))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (provide :icon-dialog-item))

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
                                   (lambda ()
                                     (funcall action obj))
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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (symbol-function 'point-v) #'easygui:point-y)
  (setf (symbol-function 'point-h) #'easygui:point-x)
  (shadowing-import 'easygui:point-x)
  (shadowing-import 'easygui:point-y))

(ccl::register-character-name "UpArrow" #\U+F700)
(ccl::register-character-name "DownArrow" #\U+F701)
(ccl::register-character-name "BackArrow" #\U+F702)
(ccl::register-character-name "ForwardArrow" #\U+F703)
(defparameter *arrow-cursor* 'arrow-cursor-fixme)
(defparameter *black-pattern* 'black-pattern-fixme)

(defun make-point (x y)
  (easygui::point x y :allow-negative-p t))

(defun make-rect (&rest args)
  (let ((topleft) (bottomright))
    (loop for (key val) in (group args 2)
          do (ecase key
               (:topleft (setf topleft val))
               (:bottomright (setf bottomright val))))
    (destructuring-bind (left top right bottom) (canonicalize-rect topleft bottomright nil nil)
      (destructuring-bind (startx starty width height) (list left top (- right left) (- bottom top))
        (ns:make-ns-rect startx starty width height)))))

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

(defmethod view-subviews ((view view))
  (easygui:view-subviews view))

(defmethod view-named (name (view view))
  (acond ((easygui:view-named name view)
          it)
         (t
           (format t "no subview with view-nick-name ~a found in ~a" name view)
           nil)))

(defmethod view-nick-name ((view simple-view))
  (easygui:view-nick-name view))

(defmethod window-select ((win window))
  (easygui:window-show win))

(defmethod window-show ((win window))
  (easygui:window-show win))

;FIXME: This looks very strange. Prob related to Phaser's floating window
(defun ccl::window-bring-to-front (w &optional (wptr (wptr w)))
  (window-select w))

(defmethod set-window-layer ((window window) new-layer &optional include-invisibles)
  'fixme)

(defmethod window-close ((win window))
  (guard ((wptr win) "Window ~a is already closed" win) ())
  (easygui:perform-close win))

(defmethod window-title ((view window))
  ;TODO: Maybe use easygui:view-text method here?
  (easygui::window-title view))

(defmethod dialog-item-text ((view view-text-mixin))
  (easygui:view-text view))

(defmethod text-just ((view view-text-mixin))
  (text-justification view))

(defun convert-justification (justification)
  (let ((mapping (list (cons $tejustleft #$NSLeftTextAlignment)
                       (cons $tejustcenter #$NSCenterTextAlignment)
                       (cons $tejustright #$NSRightTextAlignment))))
    (guard (it1 "No mapping found for justification ~a" justification)
      (cdr (assoc justification mapping)))))

(defmethod set-text-justification ((view view-text-mixin) justification)
  (#/setAlignment: (easygui:cocoa-ref view) (convert-justification justification))
  (setf (text-justification view) justification))

(defmethod initialize-instance :after ((view view-text-mixin) &key)
  (set-text-justification view (text-justification view)))

(defmethod set-dialog-item-text ((view easygui::view-text-mixin) text)
  (setf (easygui:view-text view) text))

(defmethod set-selection-range ((view view-text-mixin) &optional position cursorpos)
  (destructuring-bind (position cursorpos) (if position
                                             (list position cursorpos)
                                             (list 0 0))
    ; In order for setSelectedRange: to work, the view must be selected first, so the 
    ; view is currently selected by calling selectText:, which actually highlights all
    ; text in the view. So, a bit of a kludge, but it seems to behave just fine.
    (#/selectText: (cocoa-ref view)
     ccl:+null-ptr+)
    (#/setSelectedRange:
     (#/fieldEditor:forObject: (cocoa-ref (view-window view))
      #$YES 
      (cocoa-ref view))
     (ns:make-ns-range position (- cursorpos position)))))

(defmethod dialog-item-enable ((view action-view-mixin))
  (easygui:set-dialog-item-enabled-p view t))

(defmethod dialog-item-disable ((view action-view-mixin))
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

; FIXME: This seems to work properly, but I don't currently understand why,
; or what view-origin is supposed to do in MCL
(defmethod view-origin ((view simple-view))
  (make-point 0 0))

(defmethod origin ((view simple-view))
  (view-origin view))

(defmethod invalidate-view ((view simple-view) &optional erase-p)
  ; Cocoa takes care of erasing and redrawing; AFAIK this is OK to ignore
  (declare (ignore erase-p))
  (easygui:invalidate-view view))

(defun canonicalize-point (x y)
  (cond (y (list x y))
        (t (list (point-h x) (point-v x)))))

(defun canonicalize-rect (left top right bottom)
  (cond (bottom (list left top right bottom))
        (top (list (point-h left)
                   (point-v left)
                   (point-h top)
                   (point-v top)))
        (t (list (ns:ns-rect-x left)
                 (ns:ns-rect-y left)
                 (+ (ns:ns-rect-x left) (ns:ns-rect-width left))
                 (+ (ns:ns-rect-y left) (ns:ns-rect-height left))))))

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

(defmethod height ((view simple-view))
  (point-v (view-size view)))

(defmethod view-window ((view window))
  view)

(defmethod view-container ((view simple-view))
  (easygui:view-container view))

(defmethod view-window ((view simple-view))
  (awhen (view-container view)
    (view-window it)))

(defmethod content-view ((view window))
  (easygui:content-view view))

(defmethod content-view ((view simple-view))
  view)

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

(defparameter *current-graphics-context-stroke-color* nil)

(defmacro with-fore-color (color &body body)
  `(progn
     (guard ((eq (type-of ,color) 'ns:ns-color) "color ~a is not a system color" ,color) ())
     (let ((*current-graphics-context-stroke-color* ,color))
       (with-graphics-context
         (#/set ,color)
         ,@body))))

(defmacro with-focused-view (view &body body)
  "Any changes to the graphics environment by body will be directed to the view object"
  `(easygui:with-focused-view (easygui:cocoa-ref (content-view ,view))
     ,@body))

(defmethod wptr ((view window))
  (if (slot-boundp view 'easygui::ref)
    (#/isVisible
     (guard-!null-ptr
       (easygui::cocoa-ref view)))))

(defmethod easygui::window-may-close :around ((win window))
  (when (call-next-method)
    (slot-makunbound win 'easygui::ref)))

(defmethod local-to-global ((view simple-view) local-pos)
  (add-points (easygui:view-position view) local-pos))

(defmethod part-color ((view easygui:static-text-view) (part (eql :text)))
  (declare (ignore part))
  (get-fore-color view))

(defmethod set-part-color ((view static-text-dialog-item) (part (eql :body)) new-color)
  (set-back-color view new-color))

(defmethod set-part-color ((view static-text-dialog-item) (part (eql :text)) new-color)
  (set-fore-color view new-color))

; FIXME: Keep this as a compiler warning until you figure out how to color a border with Cocoa
(defmethod set-part-color ((view static-text-dialog-item) (part (eql :frame)) new-color)
  (setf (bordered-p view) t))

(defmethod get-fore-color ((view simple-view))
  (easygui:get-fore-color view))

(defmethod get-back-color ((view view))
  (easygui:get-back-color view))

(defmethod set-fore-color ((view simple-view) new-color)
  (easygui:set-fore-color view new-color))

(defmethod set-back-color ((view simple-view) new-color)
  (easygui:set-back-color view new-color))

; Handling mouse movement/interaction

(defmethod easygui::mouse-down ((view simple-view) &key location &allow-other-keys)
  (view-click-event-handler view location))

; FIXME: What does this do? Keep as compiler warning until you figure it out
(defmethod window-update-cursor ((window window) point)
  nil)

(defmethod view-click-event-handler ((device simple-view) position)
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

(defmethod easygui::view-key-event-handler ((device window) key)
  (view-key-event-handler device key))

(defmethod view-key-event-handler ((device window) key)
  (declare (ignore key))
  ; Default primary method on the window is to do nothing
  (values))

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

(defmethod view-draw-contents ((view simple-view))
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

(defmethod move-to ((view window) x &optional y)
  (move-to (content-view view) x y))

(defmethod move-to ((view simple-view) x &optional (y nil))
  (destructuring-bind (x y) (canonicalize-point x y)
    (let ((position (make-point x y)))
      (when (bezier-path view)
        (#/moveToPoint: (bezier-path view) (ns:make-ns-point x y)))
      (setf (pen-position view) position))))

(defmethod line-to ((view window) x &optional y)
  (line-to (content-view view) x y))

(defmethod line-to ((view simple-view) x &optional (y nil))
  (destructuring-bind (endx endy) (canonicalize-point x y)
    (destructuring-bind (startx starty) (list (point-x (pen-position view))
                                              (point-y (pen-position view)))
      (when (bezier-path view)
        (#/lineToPoint: (bezier-path view) (ns:make-ns-point endx endy)))
      (setf (pen-position view) (make-point endx endy))
      (#/strokeLineFromPoint:toPoint:
       ns:ns-bezier-path
       (ns:make-ns-point startx starty) 
       (ns:make-ns-point endx endy)))))

(defmethod frame-oval ((view simple-view) left &optional top right bottom)
  (destructuring-bind (left top right bottom) (canonicalize-rect left top right bottom)
    (destructuring-bind (startx starty width height) (list left top (- right left) (- bottom top))
      (let* ((rect (ns:make-ns-rect startx starty width height))
             (path (#/bezierPathWithOvalInRect: ns:ns-bezier-path rect)))
        (#/stroke path)))))

(defmethod fill-oval ((view simple-view) pattern left &optional top right bottom)
  (destructuring-bind (left top right bottom) (canonicalize-rect left top right bottom)
    (destructuring-bind (startx starty width height) (list left top (- right left) (- bottom top))
      (let* ((rect (ns:make-ns-rect startx starty width height))
             (path (#/bezierPathWithOvalInRect: ns:ns-bezier-path rect)))
        (with-focused-view view
          (#/fill path))))))

(defmethod stroke-ns-rect ((rect ns:ns-rect))
  (#/strokeRect: ns:ns-bezier-path rect))

(defmethod frame-rect ((view simple-view) left &optional top right bottom)
  (destructuring-bind (left top right bottom) (canonicalize-rect left top right bottom)
    (destructuring-bind (startx starty width height) (list left top (- right left) (- bottom top))
      (let ((rect (ns:make-ns-rect startx starty width height)))
        (stroke-ns-rect rect)))))

(defmethod fill-ns-rect ((rect ns:ns-rect) &optional pattern)
  (#/fillRect: ns:ns-bezier-path rect))

(defmethod fill-rect ((view simple-view) pattern left &optional top right bottom)
  (destructuring-bind (left top right bottom) (canonicalize-rect left top right bottom)
    (destructuring-bind (startx starty width height) (list left top (- right left) (- bottom top))
      (let ((rect (ns:make-ns-rect startx starty width height)))
        (fill-ns-rect rect pattern)))))

(defmethod paint-rect ((view simple-view) left &optional top right bottom)
  (fill-rect view (pen-pattern view) left top right bottom))

(defmethod erase-rect ((view window) left &optional top right bottom)
  (erase-rect (content-view view) left top right bottom))

(defmethod erase-rect ((view view) left &optional top right bottom)
  (destructuring-bind (left top right bottom) (canonicalize-rect left top right bottom)
    (destructuring-bind (startx starty width height) (list left top (- right left) (- bottom top))
      (let ((rect (ns:make-ns-rect startx starty width height)))
        (with-focused-view view
          (with-fore-color (get-back-color view)
            (fill-ns-rect rect)))))))

(defmethod start-polygon ((view simple-view))
  (setf (bezier-path view) (#/bezierPath ns:ns-bezier-path))
  (#/retain (bezier-path view))
  (#/moveToPoint: (bezier-path view)
   (easygui::ns-point-from-point (pen-position view))))

(defun pattern->system-color (pattern)
  (color-symbol->system-color
    (guard-!nil
      (cond ((eq pattern *black-pattern*) 'black)))))

(defmethod fill-polygon ((view simple-view) pattern polygon)
  (unwind-protect (with-focused-view view
                      (#/fill (bezier-path view))))
    ())

(defmethod frame-polygon ((view simple-view) polygon)
  (unwind-protect (with-focused-view view
                      (#/stroke (bezier-path view))))
    ())

(defmethod kill-polygon ((polygon ns:ns-bezier-path))
  (#/release polygon)
  (setf polygon nil))

(defmethod get-polygon ((view simple-view))
  (bezier-path view))

(defmethod stream-write-string ((v simple-view) string &optional start end)
  (let* ((string
           (objc:make-nsstring
             (if start
               (subseq string start end)
               string)))
         (dict (#/dictionaryWithObjectsAndKeys: ns:ns-mutable-dictionary
                (view-font v) #$NSFontAttributeName
                *current-graphics-context-stroke-color* #$NSForegroundColorAttributeName
                ccl:+null-ptr+)))
    (#/drawAtPoint:withAttributes: string
     (easygui::ns-point-from-point (pen-position v))
     dict)))

; Handling fonts and string width/height in pixels

(defun convert-font (name pt)
  (guard ((not (equal it1 ccl:+null-ptr+)) "font not found for font-name ~a" name)
    (#/fontWithName:size: ns:ns-font
     (objc:make-nsstring name)
     pt)))

(defun color-lst->color (lst)
  (destructuring-bind (type val) lst
    (ecase type
      (:color (mcl-color->system-color val))
      (:color-index
        (unless (eq val 0)
          (error "need to support this")
          ; Default, so return nil
          ())))))

(defun parse-mcl-font-lst (font-lst)
  (let ((name) (pt) (color))
    (dolist (atom font-lst)
      (etypecase atom
        (string (setf name atom))
        (integer (setf pt atom))
        ; FIXME; Parse these style and transfer mode values
        (keyword ())
        (list (setf color (color-lst->color atom)))))
    (append
      (list :view-font (convert-font name pt))
      (if color
        (list :fore-color color)))))

(defmethod view-font ((view simple-view))
  (guard-!null-ptr
    (guard-!nil
      (easygui:view-font view))))

(defun font-info (font-spec)
  (values (guard-!null-ptr (#/ascender font-spec))
          (abs (guard-!null-ptr (#/descender font-spec)))))

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
  (let ((directory
          (aif directory
            it
            (aif *load-truename*
              (directory-namestring it)))))
    (easygui:choose-directory-dialog :directory directory)))

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

(defun beep ()
  (#_NSBeep))

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
      (let* ((fun-names (list "showmenubar" "hidemenubar" "getcursor" "showcursor" "ShowCursor" "HideCursor"
                              "paintrect" "framerect" "drawstring"))
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

(defun X86-Darwin64::|paintrect| (rect)
  (fill-ns-rect rect))

(defun X86-Darwin64::|framerect| (rect)
  (stroke-ns-rect rect))

(defun X86-Darwin64::|drawstring| ()
  t)

; And the constants are here
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant X86-Darwin64::|tejustleft| $tejustleft)
  (defconstant X86-Darwin64::|tejustcenter| $tejustcenter)
  (defconstant X86-Darwin64::|tejustright| $tejustright))

