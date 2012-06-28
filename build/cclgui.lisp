; ----------------------------------------------------------------------
; Begin file: easygui/extensions.lisp
; ----------------------------------------------------------------------


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

; ----------------------------------------------------------------------
; Use a consuming-view class to say that all subviews within an instance of that class will not respond to mouse clicks. 
;
; This is to work around the differences in first responders between MCL and CCL. MCL looks down the view hierarchy for the first responder
; (breadth first), CCL looks down the hierarchy for the deepest responder (depth first). In order
; to simulate breadth first by stopping at a particular view in the tree (and not inspecting that view's
; subviews), create an instance of the consuming-view class. 
; ----------------------------------------------------------------------

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

; ----------------------------------------------------------------------
; Providing a view container to hold and display images.
; ----------------------------------------------------------------------

(defclass cocoa-image-view (cocoa-extension-mixin ns:ns-image-view)
  ()
  (:metaclass ns:+ns-object))

(defclass image-view (easygui::view)
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

; ----------------------------------------------------------------------
; End file: easygui/extensions.lisp
; ----------------------------------------------------------------------


; ----------------------------------------------------------------------
; Begin file: binccl/resources.lisp
; ----------------------------------------------------------------------


; ----------------------------------------------------------------------
; Some functions to create and manage resource data for CCL
;
; If images or sounds are needed for your application, these functions can 
; be used to manage those resources. Usual getters/setters/creators are available:
;
; #'create-resource: Creates an image or sound resource, given a path to that file
; #'add-resource: Adds a created resource to the pool
; #'get-resource-val: Retrieves a resource's value from the pool
;
; Note that a form of lazy evaluation is used to alloc the resources only when needed
; That is, each resource is allocd the first time it's retrieved, 'not' when it's created, or
; added to the pool. If you want to alloc all resources currently in the pool (for pre-caching), 
; call #'alloc-resources

(require :cocoa)

(defun init-pool ()
  (make-hash-table :test #'equalp))

(defvar *pool* (init-pool))

(defun print-pool (&optional (pool *pool*))
  (maphash (lambda (key val)
             (format t "~a->~a~%" key val))
           pool))

(defclass resource ()
  ((val :accessor val :initarg :val)
   (alloc-fn :accessor alloc-fn :initarg :alloc-fn)))

(defmacro when-bound ((name instance))
  `(if (slot-boundp  ,instance ',name)
     (,name ,instance)
     'slot-unbound))

(defmethod print-object ((obj resource) stream)
  (print-unreadable-object (obj stream :identity t :type t)
    (format stream "val->~a,alloc-fn->~a~%"
            (when-bound (val obj))
            (when-bound (alloc-fn obj)))))

(defmethod alloc-resource ((obj resource))
  (unless (slot-boundp obj 'val)
    (setf (val obj) (funcall (alloc-fn obj))))
  obj)

(defun alloc-resources (&optional (pool *pool*))
  (maphash 
    (lambda (key val)
      (declare (ignore key))
      (alloc-resource val))
    pool))

(defmethod get-val ((obj resource))
  (alloc-resource obj)
  (val obj))

(defun get-resource (id &optional (pool *pool*))
  (multiple-value-bind (resource present-p) (gethash id pool)
    (unless present-p
      (error "resource with id ~a not present in pool ~a~%" id pool))
    resource))

(defun get-resource-val (id &optional (pool *pool*))
  (get-val (get-resource id pool)))

(defun get-id (resource &optional (pool *pool*))
  (error "write this when needed"))

(defun add-resource (resource id &optional (pool *pool*))
  (setf (gethash id pool) resource))

(defun remove-resource (resource &optional (pool *pool*))
  (error "write this when needed"))

(defmethod create-resource ((type (eql 'image)) path)
  (make-instance 
    'resource
    :alloc-fn
    (lambda ()
      (#/initWithContentsOfFile: 
       (#/alloc ns:ns-image) 
       (objc:make-nsstring path)))))

(defmethod create-resource ((type (eql 'sound)) path)
  (make-instance
    'resource
    :alloc-fn
    (lambda ()
      (#/initWithContentsOfFile:byReference:
       (#/alloc ns:ns-sound)
       (objc:make-nsstring path)
       nil))))

; I am requiring all objective-c/lisp functions to not ever use ns-arrays as inputs or outputs
; This slows down computation time b/c conversions have to be done within each function, but it
; makes each one much easier to use in a lisp environment (keep lisp types for inputs and outputs).
;
; I am not doing this for ns-mutable string; I'm putting up with doing the conversion on that one when 
; needed. It also would be problematic to convert the type of anything that can be placed within an ns-array.
; Done this way, where the containers (arrays/lists) are converted, but not the containees, the container
; conversion functions do not have to do any type conversion or type checking.

(defmacro! do-array ((varsym array &optional ret) &body body)
  `(loop for ,g!i below (#/count ,array)
         for ,varsym = (#/objectAtIndex: ,array ,g!i)
         do (progn ,@body)
         ,@(if ret `(finally (return ,ret)))))

(defun ns-array->list (ns-array)
  (let ((out))
    (do-array (item ns-array out)
      (push-to-end item out))))

(defun list->ns-array (lst)
  (let ((out (#/array ns:ns-mutable-array)))
    (dolist (item lst out)
      (#/addObject: out item))))

(defun contents-of-directory (dir)
  (ns-array->list
    (#/contentsOfDirectoryAtPath:error:
     (#/defaultManager ns:ns-file-manager)
     (objc:make-nsstring dir)
     ccl:+null-ptr+)))

(defun remove-if-not-predicate (lst predicate)
  (ns-array->list
    (#/filteredArrayUsingPredicate:
     (list->ns-array lst)
     (#/predicateWithFormat:
      ns:ns-predicate
      (objc:make-nsstring predicate)))))

(defun remove-if-not-image (lst)
  (remove-if-not-predicate lst "self ENDSWITH '.tiff'"))

(defun remove-if-not-sound (lst)
  (remove-if-not-predicate lst "self ENDSWITH '.aif'"))

(defun open-resource-folder (dir)
  (let ((dir (if (pathnamep dir) 
               (directory-namestring dir)
               dir)))
    (loop for (type filter-fun) in (list (list 'image #'remove-if-not-image)
                                         (list 'sound #'remove-if-not-sound))
          do (dolist (image-name (funcall filter-fun (contents-of-directory dir)))
               (let* ((image-name-lisp-str (objc:lisp-string-from-nsstring image-name))
                      (image-name-no-ext (#/stringByDeletingPathExtension image-name))
                      (res (create-resource type (format nil "~a~a" dir image-name-lisp-str))))
                 (add-resource res (objc:lisp-string-from-nsstring image-name-no-ext)))))))

; This is a wrapper function to maintain backwards compatibility with lab code that was loading a resource file.
; The idea is that we take all images and sounds in the resource file, and place them into a single folder. Then
; on the old code, load the folder instead of the resource file, but the old code doesn't have to be changed, since
; it just calls the function below, which calls the proper open-resource-folder function

#|
(defun open-resource-file (dir &key if-does-not-exist errorp direction perm data-fork-p)
  (open-resource-folder dir))|#

(provide :resources)

; Section for test code:
#| 
(let ((dir (choose-directory-dialog)))
  (setf *pool* (init-pool))
  (open-resource-file (directory-namestring dir))
  (print-pool)
  (get-resource-val "voteboxbg"))
|#

; ----------------------------------------------------------------------
; End file: binccl/resources.lisp
; ----------------------------------------------------------------------


; ----------------------------------------------------------------------
; Begin file: binccl/mcl-ccl-colors.lisp
; ----------------------------------------------------------------------


(require :cocoa)
(require :easygui)

; ----------------------------------------------------------------------
; Defining color-symbol->system-color and system-color->symbol for CCL.
;
; These functions may have been in the base CCL distro, but I couldn't find them.
; So I searched online for a table of color names -> rgb mappings, threw that
; data into a bash shell, cleaned up the text, and then pasted it here. A few lisp
; parenths were wrapped around that, which turned the data into a lexical closure.
; ----------------------------------------------------------------------

(let ((rgb-list
        (list
          'Grey (list 84 84 84)
          'grey (list 190 190 190)
          'gray (list 190 190 190)
          'LightGray (list 211 211 211)
          'LightSlateGrey (list 119 136 153)
          'SlateGray (list 112 128 144)
          'black (list 0 0 0)
          'AliceBlue (list 240 248 255)
          'BlueViolet (list 138 43 226)
          'CadetBlue (list 95 158 160)
          'CadetBlue (list 95 158 160)
          'CornflowerBlue (list 100 149 237)
          'DarkSlateBlue (list 72 61 139)
          'DarkTurquoise (list 0 206 209)
          'DeepSkyBlue (list 0 191 255)
          'DodgerBlue (list 30 144 255)
          'LightBlue (list 173 216 230)
          'LightCyan (list 224 255 255)
          'LightSkyBlue (list 135 206 250)
          'LightSlateBlue (list 132 112 255)
          'LightSteelBlue (list 176 196 222)
          'Aquamarine (list 112 219 147)
          'MediumBlue (list 0 0 205)
          'MediumSlateBlue (list 123 104 238)
          'MediumTurquoise (list 72 209 204)
          'MidnightBlue (list 25 25 112)
          'NavyBlue (list 0 0 128)
          'PaleTurquoise (list 175 238 238)
          'PowderBlue (list 176 224 230)
          'RoyalBlue (list 65 105 225)
          'SkyBlue (list 135 206 235)
          'SlateBlue (list 106 90 205)
          'SteelBlue (list 70 130 180)
          'aquamarine (list 127 255 212)
          'azure (list 240 255 255)
          'blue (list 0 0 255)
          'aqua (list 0 255 255)
          'cyan (list 0 255 255)
          'navy (list 0 0 128)
          'teal (list 0 128 128)
          'turquoise (list 64 224 208)
          'DarkSlateGray (list 47 79 79)
          'Iris (list 3 180 200)
          'RosyBrown (list 188 143 143)
          'SaddleBrown (list 139 69 19)
          'SandyBrown (list 244 164 96)
          'beige (list 245 245 220)
          'brown (list 165 42 42)
          'brown (list 166 42 42)
          'burlywood (list 222 184 135)
          'chocolate (list 210 105 30)
          'peru (list 205 133 63)
          'tan (list 210 180 140)
          'Sienna (list 142 107 35)
          'Tan (list 219 147 112)
          'DarkGreen (list 0 100 0)
          'DarkKhaki (list 189 183 107)
          'DarkOliveGreen (list 85 107 47)
          'olive (list 128 128 0)
          'DarkSeaGreen (list 143 188 143)
          'ForestGreen (list 34 139 34)
          'GreenYellow (list 173 255 47)
          'LawnGreen (list 124 252 0)
          'LightSeaGreen (list 32 178 170)
          'LimeGreen (list 50 205 50)
          'MediumSeaGreen (list 60 179 113)
          'MediumSpringGreen (list 0 250 154)
          'MintCream (list 245 255 250)
          'OliveDrab (list 107 142 35)
          'PaleGreen (list 152 251 152)
          'SpringGreen (list 0 255 127)
          'YellowGreen (list 154 205 50)
          'chartreuse (list 127 255 0)
          'green (list 0 255 0)
          'green (list 0 128 0)
          'lime (list 0 255 0)
          'khaki (list 240 230 140)
          'DarkOrange (list 255 140 0)
          'DarkSalmon (list 233 150 122)
          'LightCoral (list 240 128 128)
          'LightSalmon (list 255 160 122)
          'PeachPuff (list 255 218 185)
          'bisque (list 255 228 196)
          'coral (list 255 127 0)
          'coral (list 255 127 80)
          'honeydew (list 240 255 240)
          'orange (list 255 165 0)
          'salmon (list 250 128 114)
          'sienna (list 160 82 45)
          'Orange (list 255 127 0)
          'DeepPink (list 255 20 147)
          'HotPink (list 255 105 180)
          'IndianRed (list 205 92 92)
          'LightPink (list 255 182 193)
          'MediumVioletRed (list 199 21 133)
          'MistyRose (list 255 228 225)
          'OrangeRed (list 255 69 0)
          'PaleVioletRed (list 219 112 147)
          'VioletRed (list 208 32 144)
          'firebrick (list 178 34 34)
          'pink (list 255 192 203)
          'Flesh (list 245 204 176)
          'Feldspar (list 209 146 117)
          'red (list 255 0 0)
          'tomato (list 255 99 71)
          'Firebrick (list 142 35 35)
          'Pink (list 188 143 143)
          'Salmon (list 111 66 66)
          'Scarlet (list 140 23 23)
          'DarkOrchid (list 153 50 204)
          'DarkViolet (list 148 0 211)
          'LavenderBlush (list 255 240 245)
          'MediumOrchid (list 186 85 211)
          'MediumPurple (list 147 112 219)
          'lavender (list 230 230 250)
          'magenta (list 255 0 255)
          'fuchsia (list 255 0 255)
          'maroon (list 176 48 96)
          'orchid (list 218 112 214)
          'Orchid (list 219 112 219)
          'plum (list 221 160 221)
          'purple (list 160 32 240)
          'purple (list 128 0 128)
          'thistle (list 216 191 216)
          'violet (list 238 130 238)
          'Maroon (list 128 0 0)
          'Plum (list 234 173 234)
          'Thistle (list 216 191 216)
          'Turquoise (list 173 234 234)
          'Violet (list 79 47 79)
          'AntiqueWhite (list 250 235 215)
          'FloralWhite (list 255 250 240)
          'GhostWhite (list 248 248 255)
          'NavajoWhite (list 255 222 173)
          'OldLace (list 253 245 230)
          'WhiteSmoke (list 245 245 245)
          'gainsboro (list 220 220 220)
          'ivory (list 255 255 240)
          'linen (list 250 240 230)
          'seashell (list 255 245 238)
          'snow (list 255 250 250)
          'wheat (list 245 222 179)
          'white (list 255 255 255)
          'Quartz (list 217 217 243)
          'Wheat (list 216 216 191)
          'BlanchedAlmond (list 255 235 205)
          'DarkGoldenrod (list 184 134 11)
          'LemonChiffon (list 255 250 205)
          'LightGoldenrod (list 238 221 130)
          'LightGoldenrodYellow (list 250 250 210)
          'LightYellow (list 255 255 224)
          'PaleGoldenrod (list 238 232 170)
          'PapayaWhip (list 255 239 213)
          'cornsilk (list 255 248 220)
          'goldenrod (list 218 165 32)
          'moccasin (list 255 228 181)
          'yellow (list 255 255 0)
          'gold (list 255 215 0)
          'Goldenrod (list 219 219 112)
          'copper (list 184 115 51)
          'brass (list 181 166 66)
          'bronze (list 140 120 83)
          'CSS (list 204 153 0)
          'gold (list 205 127 50)
          'silver (list 230 232 250))))
  (defun color-symbol->rgb (symb)
    (getf rgb-list symb))
  (defun rgb->color-symbol (rgb)
    (loop for item on rgb-list by #'cddr
          do (destructuring-bind (cur-symb cur-rgb) (list (first item) (second item))
               (when (equal cur-rgb rgb)
                 (return-from rgb->color-symbol cur-symb))))))

(defun rgb->system-color (red green blue)
  (easygui:make-rgb :red red :green green :blue blue))

(defun color-symbol->system-color (symb)
  (destructuring-bind (red green blue) (color-symbol->rgb symb)
    (rgb->system-color red green blue)))

(defun system-color->symbol (color)
  (let ((red (easygui:rgb-red color))
        (green (easygui:rgb-green color))
        (blue (easygui:rgb-blue color)))
    (rgb->color-symbol (list red green blue))))

(defparameter *black-color* (color-symbol->system-color 'black))
(defparameter *red-color* (color-symbol->system-color 'red))
(defparameter *light-gray-pattern* (color-symbol->system-color 'gray))

; Converting MCL colors (specified as a huge (technical term) integer) to 'system' colors

; FIXME: Stole this code from CCL's src; couldn't figure out how to load it with a require;
; fix this. This code was initially stolen from MCL, so this is the actual MCL code to do the conversion

(defun color-red (color &optional (component (logand (the fixnum (lsh color -16)) #xff)))
  "Returns the red portion of the color"
  (declare (fixnum component))
  (the fixnum (+ (the fixnum (ash component 8)) component)))

(defun color-green (color &optional (component (logand (the fixnum (lsh color -8)) #xff)))
  "Returns the green portion of the color"
  (declare (fixnum component))
  (the fixnum (+ (the fixnum (ash component 8)) component)))

(defun color-blue (color &optional (component (logand color #xff)))
  "Returns the blue portion of the color"
  (declare (fixnum component))
  (the fixnum (+ (the fixnum (ash component 8)) component)))

(defun color-values (color)
  "Given an encoded color, returns the red, green, and blue components"
  (values
    (ceiling (* (/ (float (color-red color)) (float 65535)) 255))
    (ceiling (* (/ (float (color-green color)) (float 65535)) 255))
    (ceiling (* (/ (float (color-blue color)) (float 65535)) 255))))

(defun mcl-color->system-color (color)
  "Converts an MCL color to a CCL system color"
  (multiple-value-bind (r g b) (color-values color)
    (rgb->system-color r g b)))

(provide :mcl-ccl-colors)
; ----------------------------------------------------------------------
; End file: binccl/mcl-ccl-colors.lisp
; ----------------------------------------------------------------------


; ----------------------------------------------------------------------
; Begin file: actr6/devices/ccl/share.lisp
; ----------------------------------------------------------------------


(require :cocoa)
(require :easygui)
(require :resources)
(require :mcl-ccl-colors)

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

(defclass view-text-mixin (easygui::view-text-mixin)
  ((text-justification :accessor text-justification :initarg :text-justification :initform $tejustleft)))

(defclass view-mixin (easygui:view)
  ((easygui::size :initarg :view-size :initform (make-point 100 100))
   (easygui::position :initarg :view-position)
   (easygui::foreground :initarg :color)
   (temp-view-subviews :initarg :view-subviews)))

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
    (apply #'call-next-method view :back-color (mcl-color->system-color back-color) args)
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
   (close-box-p :accessor close-box-p :initarg :close-box-p :initform t)))

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
  ((easygui::foreground :reader color)))

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
   (text-truncation :initarg :text-truncation))
  (:default-initargs :specifically 'easygui::cocoa-mouseable-text-field))

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

(provide :icon-dialog-item)

(defclass thermometer (simple-view)
  ())

(provide :thermometer)

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
(import 'easygui:point-x)
(import 'easygui:point-y)

(ccl::register-character-name "UpArrow" #\U+F700)
(ccl::register-character-name "DownArrow" #\U+F701)
(ccl::register-character-name "BackArrow" #\U+F702)
(ccl::register-character-name "ForwardArrow" #\U+F703)
(defparameter *arrow-cursor* 'fixme)
(defparameter *black-pattern* 'fixme)
(defparameter *tool-back-color* 15658734)

(defun make-point (x y)
  (make-instance 'easygui::eg-point :x x :y y))

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

(defmethod radio-button-unpush ((item radio-button-dialog-item))
  (easygui:radio-button-deselect item))

(defmethod radio-button-push ((item radio-button-dialog-item))
  (easygui:radio-button-select item))

(defmethod view-position ((view simple-view))
  (easygui:view-position view))

(defmethod set-view-position ((view simple-view) x &optional (y nil))
  (let ((pos (if y
               (make-point x y)
               x)))
    (setf (easygui:view-position view) pos)))

(defmethod set-view-size ((view simple-view) x &optional (y nil))
  (let ((size (if y
                (make-point x y)
                x)))
    (setf (easygui:view-size view) size)))

(defmethod view-size ((view simple-view))
  (easygui:view-size view))

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

(defmethod move-to ((view simple-view) position)
  (setf (pen-position view) position))

(defmethod line-to ((view simple-view) position)
  (destructuring-bind (startx starty) (list (point-x (pen-position view))
                                            (point-y (pen-position view)))
    (destructuring-bind (endx endy) (list (point-x position)
                                          (point-y position))
      (with-fore-color (get-fore-color view)
        (#/strokeLineFromPoint:toPoint:
         ns:ns-bezier-path
         (ns:make-ns-point startx starty) 
         (ns:make-ns-point endx endy))))))

(defmethod part-color ((view easygui:static-text-view) (part (eql :text)))
  (declare (ignore part))
  (get-fore-color view))

(defmethod set-part-color ((view static-text-dialog-item) (part (eql :body)) new-color)
  (set-back-color view new-color))

(defmethod set-part-color ((view static-text-dialog-item) (part (eql :text)) new-color)
  (set-fore-color view new-color))

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

(defmethod view-click-event-handler ((device easygui:view) position)
  (awhen (view-container device) 
    (view-click-event-handler it position)))

(defmethod device-move-cursor-to ((device window) (xyloc vector))
  (setf xyloc (local-to-global device (vpt2p xyloc)))
  (#_CGWarpMouseCursorPosition (ns:make-ns-point (point-h xyloc)
                                                 (point-v xyloc))))

(defmethod view-mouse-position ((view simple-view))
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
  (line-to view (get-end view)))

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

(provide :quickdraw)

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

(defvar *nonhacked-readtable* (copy-readtable))
; Code grabbed from RMCL, since MCL is now open-sourced (yay!)
; For reading #@(h v) as points.
(set-dispatch-macro-character 
  #\# #\@
  (defun |#@-reader| (stream char arg)
    (declare (ignore arg char))
    (let ((list (read stream t nil t)))
      (unless *read-suppress*
        (let ((point (apply #'make-point list)))
          point)))))

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
        (funcall *load-external-function-orig* sym query)))))

; Use the same approach to define foreign constants that MCL uses that no longer exist for CCL

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
        (funcall *load-os-constant-orig* sym query)))))

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

(defconstant $tejustleft :left)
(defconstant $tejustcenter :center)
(defconstant $tejustright :right)

(defconstant X86-Darwin64::|tejustleft| $tejustleft)
(defconstant X86-Darwin64::|tejustcenter| $tejustcenter)
(defconstant X86-Darwin64::|tejustright| $tejustright)

; ----------------------------------------------------------------------
; End file: actr6/devices/ccl/share.lisp
; ----------------------------------------------------------------------


; ----------------------------------------------------------------------
; Begin file: rmcl/lib/ccl-menus.lisp
; ----------------------------------------------------------------------


(defclass string-dialog (dialog)
  ((allow-empty-strings :initform nil :initarg :allow-empty-strings)))

(defclass get-string-dialog (string-dialog)())

(defmethod update-default-button ((obj string-dialog)) ())

(defmethod set-view-size ((dialog get-string-dialog) h &optional v)
  (declare (ignore h v))
  (let* ((old-size (view-size dialog)))
    (call-next-method)
    (let* ((new-size (view-size dialog))
           (hdelta (make-point (- (point-h old-size)(point-h new-size)) 0))
           (subs (view-subviews dialog))
           (len (length subs)))
      (dotimes (i len)
        (let ((sub (elt subs i)))
          (if (typep sub 'button-dialog-item)
            (set-view-position sub (subtract-points (view-position sub) hdelta))
            (if (typep sub 'editable-text-dialog-item)
              (set-view-size sub (subtract-points (view-size sub) hdelta)))))))))

;; could be prettier, need a set-view-size method - move buttons, resize editable-text - done
; 140 x 80 is about minumum useful size - neg size is invisible
(with-continue
(defun get-string-from-user (message 
                             &key
                             initial-string
                             (size #@(365 100))
                             (position #@(140 140))
                             (ok-text "OK")
                             (cancel-text "Cancel")
                             (modeless nil)
                             (window-title "")
                             (window-type :document-with-grow)
                             (back-color *tool-back-color*)
                             (allow-empty-strings nil)
                             (action-function #'identity)
                             cancel-function
                             (theme-background t)
                             &aux dialog (delta 0) (message-len 0) message-item)
  (when (not initial-string) (setq initial-string ""))
  (if t (setq delta 20)(setq delta 10))  
  (when message 
    (setq message-item (make-instance 'static-text-dialog-item
                         :text-truncation :end
                         :view-position (make-point 6 (- (point-v size) 54 delta))
                         :dialog-item-text message))
    (let* ((msize (view-default-size message-item))
           (mh (point-h msize)))  ;; would be nice if static text had a truncate option -now it does
      (setq mh (min mh (- (point-h size) 100)))
      (set-view-size message-item (make-point mh (point-v msize))))
    (setq message-len (+ 6 (point-h (view-size message-item)))))
  (flet ((act-on-text (item)
           (let ((e-item
                  (find-subview-of-type (view-container item)
                                        'editable-text-dialog-item)))
             (funcall action-function (dialog-item-text e-item)))))    
    (setq dialog (make-instance 
                   'get-string-dialog
                   :view-position position
                   :view-size size
                   :close-box-p (if modeless t nil)
                   :grow-box-p t
                   :window-type window-type
                   :window-title window-title
                   :window-show nil
                   :back-color back-color
                   :theme-background theme-background
                   :allow-empty-strings allow-empty-strings
                   :view-subviews
                   (list
                     (make-dialog-item
                       'default-button-dialog-item
                       (make-point (- (point-h size) 74)
                                   (- (point-v size) 20 delta))
                       #@(62 20)
                       ok-text
                       (if (not modeless)
                         #'(lambda (item)
                             (return-from-modal-dialog (act-on-text item)))
                         #'act-on-text))                     
                     (make-dialog-item 'button-dialog-item
                                       (make-point (- (point-h size) 154)
                                                   (- (point-v size) 20 delta))
                                       #@(62 20)
                                       cancel-text
                                       (or cancel-function
                                       #'(lambda (item)
                                           (if (not modeless) 
                                             (return-from-modal-dialog :cancel)
                                             (window-close (view-window item)))))
                                       :cancel-button t)
                     (make-dialog-item 'editable-text-dialog-item
                                        (make-point (+ 6 message-len) (- (point-v size) 54 delta))
                                        (make-point (- (point-h size) delta message-len) 16)
                                        initial-string))))
    (when message (add-subviews  dialog  message-item))
    
    (update-default-button dialog)
    (cond ((not modeless)         
           (modal-dialog dialog))
          (t (window-show dialog)
             dialog)))))


; ----------------------------------------------------------------------
; End file: rmcl/lib/ccl-menus.lisp
; ----------------------------------------------------------------------


; ----------------------------------------------------------------------
; Begin file: rmcl/lib/dialogs.lisp
; ----------------------------------------------------------------------


(defparameter *ret* nil)

(defun return-from-modal-dialog (form)
  (setf *ret* (cons :return form)))

(defmethod modal-dialog ((dialog window) &optional (close-on-exit t))
  (while (null *ret*) 
         (format t "waiting for user to finish with dialog~%")
         (sleep 1))
  (unwind-protect (cdr *ret*)
    (when close-on-exit
      (window-close dialog))
    (setf *ret* nil)))

(defmethod find-subview-of-type ((view easygui::view) subview-type)
  (dolist (sub (view-subviews view) nil)
    (when (typep sub subview-type)
      (return sub))))
; ----------------------------------------------------------------------
; End file: rmcl/lib/dialogs.lisp
; ----------------------------------------------------------------------


; ----------------------------------------------------------------------
; Begin file: actr6/devices/ccl/device.lisp
; ----------------------------------------------------------------------


;;;  -*- mode: LISP; Package: CL-USER; Syntax: COMMON-LISP;  Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Author      : Mike Byrne
;;; Address     : Rice University, MS-25
;;;             : Psychology Department
;;;             : Houston,TX 77251-1892
;;;             : byrne@acm.org
;;; 
;;; Copyright   : (c)1998-2004 Mike Byrne
;;; Availability: Covered by the GNU LGPL, see LGPL.txt
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Filename    : device.lisp
;;; Version     : 1.0a1
;;; 
;;; Description : MCL-specific functions for RPM.  This consists primarily
;;;             : of stuff for vision (parsing the screen), and output
;;;             : stuff for motor.
;;; 
;;; Bugs        : 
;;; 
;;; --- History ---
;;; 01.09.21 mdb [b2]
;;;             : Fixed an infinte recursion bug in APPROACH-WIDTH.
;;; 2002.04.16 mdb [b6]
;;;             : * Rolled in color text stuff.
;;;             : * Added BUILD-FEATURES-FOR methods for radio buttons and
;;;             : check boxes.
;;; 2002.04.18  mdb
;;;             : Fixed minor glitch created by color text stuff--if the part
;;;             : color was not set, that passed NIL to the color parser.  No.
;;; 2002.05.17 mdb
;;;             : Moved COLOR-SYMBOL->MAC-COLOR here.
;;; 2002.06.05 mdb
;;;             : Grr, fixed what is hopefully the last vector bug issue.
;;; 
;;; 2002.06.21 Dan [b7]
;;;             : Changed the rpm-window class to rpm-real-window and
;;;             : updated the methods accordingly.
;;; 2002.06.30 Dan
;;;             : Changed the COLOR-SYMBOL->MAC-COLOR and MAC-COLOR->SYMBOL
;;;             : function names by replacing MAC with SYSTEM to be a little
;;;             : more consistent (that way there aren't as many 'different'
;;;             : function names floating around in these files).
;;;             : Moved the view-line stuff in here from the separate file and
;;;             : documented it better.
;;;             : Removed all of the UWI code from this file.
;;; 2002.07.03 mdb
;;;             : Makes sure that SPEECH-AVAILABLE-P is defined.
;;; 2002.11.25 mdb [2.1f1]
;;;             : Added DEVICE-MOVE-CURSOR-TO for MCL5.0 on OSX. 
;;; 2003.03.11 mdb [2.1.2]
;;;             : Per DB's suggestion, cut back on EVENT-DISPATCHing. 
;;; 2003.06.18 mdb
;;;             : Turns out static text dialog items support multiple kinds
;;;             : of justifications, though it's hard to get at it.  Now
;;;             : handled properly. 
;;; 2003.06.23 mdb [2.1.3]
;;;             : Under-the-hood addition of RPM-OVERLAY class. 
;;; 2004.03.11 mdb [2.2]
;;;             : Added a VIEW-KEY-EVENT-HANDLER method for editable text dialog
;;;             : items, which used to break.
;;;
;;; 04.10.19 Dan [Moved into ACT-R 6]
;;;             : Reset the version to 1.0a1
;;;             : added the packaging switches
;;;             : changed the name to device to be placed in a folder called mcl
;;;             : removed references to *mp* and other minor
;;;             : ACT-R 6 updates
;;; 2006.09.07 Dan
;;;             : * Removed the fill-default-dimensions method because it's
;;;             :   now defined in the vision file.
;;; 2007.07.02 Dan
;;;             : * Converted things over for the new vision module.
;;; 2007.07.05 Dan
;;;             : * Rolled in the multi-line fix Mike made to the old MCL device.
;;; 2010.03.11 mdb
;;;             : Fixed DEVICE-MOVE-CURSOR-TO for (R)MCL 5.2 under OS X.
;;; 2010.06.03 mdb
;;;             : Fixed XSTART for (R)MCL 5.2 under OS X, which uses NIL for 
;;;             : left-justified text as a default (rather than :left).
;;; 2011.11.21 Dan
;;;             : * Using model-generated-action instead of *actr-enabled-p*
;;;             :   in view-key-event-handler  for editable-text-dialog-items
;;;             :   to determine when to hack the output.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+:packaged-actr (in-package :act-r)
#+(and :clean-actr (not :packaged-actr) :allegro-ide) (in-package :cg-user)
#-(or (not :clean-actr) :packaged-actr :allegro-ide) (in-package :cl-user)

#|(defparameter *crosshair-cursor* 
  (#_getcursor #$crosscursor) "Crosshair cursor")|#

(defvar *attn-tracker* nil "Holds the view for the focus ring.")
;(defparameter *last-update* (get-internal-real-time))

(defun loc-avg (x y)
  "Return the 'location' (integer) average of <x> and <y>."
  (declare (fixnum x) (fixnum y))
  (floor (/ (+ x y) 2)))


;;;; ---------------------------------------------------------------------- ;;;;;;;
;;;; MCL screen-to-icon interface
;;;; ---------------------------------------------------------------------- ;;;;;;;



(defmethod build-vis-locs-for ((self window) (vis-mod vision-module))
  (let ((base-ls (flatten
                    (mapcar #'(lambda (obj) (build-vis-locs-for obj vis-mod))
                            (get-sub-objects self)))))
   ; (dolist (feat base-ls)
   ;   (fill-default-dimensions feat))
    base-ls))

(defmethod vis-loc-to-obj ((device window) loc)
  (case (chunk-slot-value-fct loc 'kind)
    (cursor
       (fill-default-vis-obj-slots (car (define-chunks (isa cursor))) loc))))

(defgeneric get-sub-objects (view)
  (:documentation  "Grabbing the sub-objects of a view by default returns the subviews."))

(defmethod get-sub-objects ((v view))
  (subviews v))



(defmethod build-vis-locs-for ((self view) (vis-mod vision-module))
  (let ((subs (get-sub-objects self))
        (outlis nil))
    (dolist (sub subs outlis)
      (push (build-vis-locs-for sub vis-mod) outlis))))


#|
(defmethod build-vis-locs-for ((self dialog-item) (vis-mod vision-module))
  (declare (ignore vis-mod))
  (let ((f (car (define-chunks-fct `((isa visual-location
                                 screen-x ,(px (view-loc self))
                                 screen-y ,(py (view-loc self))
                                 kind visual-object
                                 value unknown))))))
    (setf (chunk-visual-object f) self)))



(defmethod build-vis-locs-for ((self editable-text-dialog-item)
                                  (vis-mod vision-module))
  (let* ((font-spec (view-font self))
         (text (dialog-item-text self))
         (feats 
          (cons
           (car (define-chunks-fct `((isa visual-location
                                          screen-x ,(px (view-loc self))
                                          screen-y ,(py (view-loc self))
                                          kind visual-object
                                          value box
                                          height ,(point-v (view-size self))
                                          width ,(point-h (view-size self))))))
           (unless (equal text "")
             (multiple-value-bind (ascent descent)
                 (font-info font-spec)
               (build-string-feats vis-mod :text text
                                   :start-x (1+ (point-h (view-position self)))
                                   :y-pos (+ (point-v (view-position self))
                                             descent (round ascent 2))
                                   :width-fct #'(lambda (str)
                                                  (string-width str font-spec))
                                   :height ascent :obj self))))))
    (dolist (x feats)
      (setf (chunk-visual-object x) self))
    feats))

|#
(defmethod build-vis-locs-for ((self button-dialog-item)
                                  (vis-mod vision-module))
  (let* ((btn-width (point-h (view-size self)))
         (btn-height (point-v (view-size self)))
         (text (dialog-item-text self))
         (feats (cons
                  (car (define-chunks-fct `((isa visual-location
                                                 screen-x ,(px (view-loc self))
                                                 screen-y ,(py (view-loc self))
                                                 kind oval
                                                 value oval
                                                 height ,(point-v (view-size self))
                                                 width ,(point-h (view-size self))
                                                 color light-gray))))
                  
                  
                  (unless (equal text "")
                    (let* ((font-spec (view-font self))
                           (start-y nil)
                           (accum nil)
                           (textlines (string-to-lines text))
                           (width-fct #'(lambda (str) (string-width str font-spec))))
                      (multiple-value-bind (ascent descent) (font-info font-spec)
                        (setf start-y (+ (point-v (view-position self))
                                         (round (- btn-height (* (length textlines)
                                                                 (+ ascent descent))) 2)))
                        (dolist (item textlines (flatten (nreverse accum)))
                          (push
                           (build-string-feats vis-mod :text item
                                               :start-x 
                                               (+ (point-h (view-position self))
                                                  (round 
                                                   (- btn-width (funcall width-fct item))
                                                   2))
                                               :y-pos 
                                               (+ start-y (round (+ ascent descent) 2))
                                               :width-fct width-fct 
                                               :height (min ascent btn-height)
                                               :obj self)
                           accum)
                          (incf start-y (+ ascent descent)))))))))
    (let ((fun (lambda (x y) (declare (ignore x)) (approach-width (car feats) y))))
      (dolist (x (cdr feats))
        (setf (chunk-visual-approach-width-fn x) fun)
        (set-chunk-slot-value-fct x 'color 'black)))
    
    (dolist (x feats)
      (setf (chunk-visual-object x) self))
    
    feats))

#|
|#


#| Not adding these in at this point - only the basics which are shared
   by acl/mcl/virtual

(defmethod build-features-for ((self radio-button-dialog-item)
                               (vis-mod vision-module))
  (let* ((btn-height (point-v (view-size self)))
         (text (dialog-item-text self)))
    (cons
     (make-instance 'oval-feature 
       :x (+ 7 (point-h (view-position self))) :y (py (view-loc self))
       :width 11 :height 11 :screen-obj self
       :color (if (radio-button-pushed-p self)
                'black
                'light-gray))
     (unless (equal text "")
       (let* ((font-spec (view-font self))
              (start-y nil)
              (accum nil)
              (textlines (string-to-lines text))
              (width-fct #'(lambda (str) (string-width str font-spec))))
         (multiple-value-bind (ascent descent) (font-info font-spec)
           (setf start-y (+ (point-v (view-position self))
                            (round (- btn-height (* (length textlines)
                                                    (+ ascent descent))) 2)))
           (dolist (item textlines (nreverse accum))
             (push
              (build-string-feats vis-mod :text item
                                  :start-x 
                                  (+ (point-h (view-position self))
                                     17)
                                  :y-pos 
                                  (+ start-y (round (+ ascent descent) 2))
                                  :width-fct width-fct 
                                  :height (min ascent btn-height)
                                  :obj self)
              accum)
             (incf start-y (+ ascent descent)))))))))


;;; BUILD-FEATURES-FOR      [Method]
;;; Date        : 02.04.16
;;; Description : Very much like radio buttons, but if checked add an 
;;;             : "X" to the output.

(defmethod build-features-for ((self check-box-dialog-item)
                                  (vis-mod vision-module))
  (let ((btn-height (point-v (view-size self)))
        (text (dialog-item-text self))
        (feats nil))
    (setf feats
          (cons
           (make-instance 'rect-feature 
             :x (+ 8 (point-h (view-position self))) :y (py (view-loc self))
             :width 11 :height 11 :color 'light-gray
             :screen-obj self)
           (unless (equal text "")
             (let* ((font-spec (view-font self))
                    (start-y nil)
                    (accum nil)
                    (textlines (string-to-lines text))
                    (width-fct #'(lambda (str) (string-width str font-spec))))
               (multiple-value-bind (ascent descent) (font-info font-spec)
                 (setf start-y (+ (point-v (view-position self))
                                  (round (- btn-height (* (length textlines)
                                                          (+ ascent descent))) 2)))
                 (dolist (item textlines (nreverse accum))
                   (push
                    (build-string-feats vis-mod :text item
                                        :start-x 
                                        (+ (point-h (view-position self))
                                           17)
                                        :y-pos 
                                        (+ start-y (round (+ ascent descent) 2))
                                        :width-fct width-fct 
                                        :height (min ascent btn-height)
                                        :obj self)
                    accum)
                   (incf start-y (+ ascent descent))))))))
    (when (check-box-checked-p self)
      (setf feats
            (cons
             (make-instance 'icon-feature
               :x (+ 8 (point-h (view-position self)))
               :y (py (view-loc self))
               :kind 'visual-object
               :value 'check
               :screen-obj self
               :height 11
               :width 11)               
             
             feats)))
    feats
    ))

|#

#|
(defmethod button-p (obj)
  (declare (ignore obj))
  nil)

(defmethod button-p ((obj button-dialog-item))
  (declare (ignore obj))
  t)

|#

(defmethod build-vis-locs-for ((self static-text-dialog-item)
                               (vis-mod vision-module))
  (let ((text (dialog-item-text self)))
    (unless (equal text "")
      (let* ((font-spec (view-font self))
             (start-y nil)
             (accum nil)
             (textlines (string-to-lines text))
             (width-fct #'(lambda (str) (string-width str font-spec)))
             (color (system-color->symbol (aif (part-color self :text)
                                           it
                                           *black-color*))))
        (multiple-value-bind (ascent descent) (font-info font-spec)
          (setf start-y (point-v (view-position self)))
          (dolist (item textlines)
            (push
             (build-string-feats vis-mod :text item
                                 :start-x (xstart self)                               
                                 :y-pos 
                                 (+ start-y (round (+ ascent descent) 2))
                                 :width-fct width-fct 
                                 :height ascent
                                 :obj self)
             accum)
            (incf start-y (+ ascent descent))))
        
        (setf accum (flatten (nreverse accum)))
        (dolist (x accum accum)
          (set-chunk-slot-value-fct x 'color color)
          (setf (chunk-visual-object x) self))))))
  

(defmethod xstart ((self static-text-dialog-item))
   (let ((left-x (point-h (view-position self)))
         (text-width (string-width (dialog-item-text self)
                                     (view-font self)))
         (text-justification (text-just self))
         )
     (ecase text-justification
       (#.#$tejustleft (1+ left-x))
       (#.#$tejustcenter (+ 1 left-x (round (/ (- (width self) text-width) 2))))
       (#.#$tejustright (+ 1 left-x (- (width self) text-width))))))
 
#|(defmethod text-just ((self static-text-dialog-item))
   (if (null (slot-value self 'ccl::text-justification))
     #.#$tejustleft
     (or (cdr (assq (slot-value self 'ccl::text-justification)
                        '((:left . #.#$tejustleft)
                          (:center . #.#$tejustcenter)
                          (:right . #.#$tejustright))))
             (require-type (slot-value self 'ccl::text-justification) 'fixnum))))|#

#|

(defmethod cursor-to-vis-loc ((the-window window))
  (let ((pos (view-mouse-position the-window))
        (shape (window-cursor the-window)))
    (when (cursor-in-window-p the-window)
      (car (define-chunks-fct `((isa visual-location kind cursor 
                                   screen-x ,(point-h pos)
                                   screen-y ,(point-v pos)
                                     value ,(case shape
                                              (*i-beam-cursor* 'i-beam)
                                              (*crosshair-cursor* 'crosshair)
                                              (*watch-cursor* 'watch)
                                              (otherwise 'pointer)))))))))

(defgeneric cursor-in-window-p (wind)
  (:documentation  "Returns T if the cursor is over <wind>, NIL otherwise."))

(defmethod cursor-in-window-p ((tw window))
  (when (window-shown-p tw)
    (rlet ((the-rect rect))
      (points-to-rect (view-position tw)
                      (add-points (view-position tw) (view-size tw))
                      the-rect)
      (point-in-rect-p the-rect 
                       (local-to-global tw (view-mouse-position tw))))))


|#

(defmethod view-loc ((self view))
  (let ((pos (view-position self))
        (size (view-size self)))
    (vector (round (+ (point-h pos) (/ (point-h size) 2)))
            (round (+ (point-v pos) (/ (point-v size) 2))))))


(defmethod view-loc ((self simple-view))
  (let ((pos (view-position self))
        (size (view-size self)))
    (vector (round (+ (point-h pos) (/ (point-h size) 2)))
            (round (+ (point-v pos) (/ (point-v size) 2))))))


(defmethod view-loc ((self symbol))
  (if (eq self :cursor)
    ;DAN (get-mouse-coordinates (device (device-interface *mp*)))
    (get-mouse-coordinates (current-device))
    (error "!! Can't find location of ~S" self)))

#|
(defmethod width ((self simple-view))
  (point-h (view-size self)))


(defmethod height ((self simple-view))
  (point-v (view-size self)))
|#
#|

;;;; ---------------------------------------------------------------------- ;;;;;;;
;;;; The view based line drawing classes and methods
;;;; ---------------------------------------------------------------------- ;;;;;;;

;;; LINER      [Class]
;;; Description : The base class for the view based lines.  
;;;             : All it adds to a simple-view is a color slot that defaults
;;;             : to black.

(defclass liner (simple-view)
  ((color :accessor color :initarg :color :initform *black-color*)))

;;; POINT-IN-CLICK-REGION-P      [Method]
;;; Description : Override this method so that lines don't handle mouse clicks.

(defmethod point-in-click-region-p ((self liner) where)
  (declare (ignore where))
  nil)


;;; TD-LINER      [Class]
;;; Description : A view that represents a line which is drawn top-down 
;;;             : i.e. from the view-position (upper-left) to the 
;;;             : [view-size - (1,1)] (lower-right) in the container window

(defclass td-liner (liner)
  ())

;;;  A view that represents a line which is drawn bottom-up i.e. from the
;;;  view's lower-left to the view's upper-right in the container window.

;;; BU-LINER      [Class]
;;; Description : A view that represents a line which is drawn bottom-up 
;;;             : i.e. from the view's lower-left to the view's upper-rignt
;;;             : in the container window

(defclass bu-liner (liner)
  ())

;;; VIEW-DRAW-CONTENTS [Method]
;;; Description : Draw a top-down line on it's container window.

(defmethod view-draw-contents ((lnr td-liner))
  "Draws the line on the view-container window using the color specified
   and restoring the previous draw color and pen position"
  (let* ((parent (view-container lnr))
         (old-point (pen-position parent))
         (old-color (get-fore-color parent))
         (other-end (add-points (view-size lnr) (view-position lnr))))
    (set-fore-color parent (color lnr))
    (move-to parent (view-position lnr))
    (line-to parent (make-point (1- (point-h other-end))
                                (1- (point-v other-end))))
    (set-fore-color parent old-color)
    (move-to parent old-point)))

;;; VIEW-DRAW-CONTENTS [Method]
;;; Description : Draw a bottom-up line on it's container window.

(defmethod view-draw-contents ((lnr bu-liner))
  "Draws the line on the view-container window using the color specified
   and restoring the previous draw color and pen position"
  (let* ((parent (view-container lnr))
         (old-point (pen-position parent))
         (old-color (get-fore-color parent)))
    (set-fore-color parent (color lnr))
    (move-to parent (make-point (point-h (view-position lnr))
                                (1- (point-v (add-points (view-position lnr) (view-size lnr))))))
    (line-to parent (make-point (1- (point-h (add-points (view-size lnr) (view-position lnr))))
                                (point-v (view-position lnr))))
    (set-fore-color parent old-color)
    (move-to parent old-point)))


|#

;;; VIEW-DRAW-CONTENTS [Method]
;;; Description : A td-liner is just a line-feature located "at" it's mid-point.

(defmethod build-vis-locs-for ((lnr td-liner) (vis-mod vision-module))
  "Convert the view to a feature to be placed into the visual icon"
  (let* ((start-pt (view-position lnr))
         (end-pt (subtract-points (add-points (view-position lnr) (view-size lnr)) 
                                  (make-point 1 1)))
         (f (car (define-chunks-fct `((isa visual-location
                                           color ,(system-color->symbol (color lnr))
                                           value line
                                           kind line
                                           screen-x ,(loc-avg (point-h start-pt) (point-h end-pt))
                                           screen-y ,(loc-avg (point-v start-pt) (point-v end-pt))
                                           width ,(abs (- (point-h start-pt) (point-h end-pt)))
                                           height ,(abs (- (point-v start-pt) (point-v end-pt)))))))))
    
    (setf (chunk-visual-object f) lnr)
    f))

#|(defmethod vis-loc-to-obj ((lnr td-liner) loc)
  (let ((start-pt (view-position lnr))
        (end-pt (subtract-points (add-points (view-position lnr) (view-size lnr)) 
                                 (make-point 1 1)))
        (v-o (fill-default-vis-obj-slots (car (define-chunks (isa line))) loc)))
    (set-chunk-slot-value-fct v-o 'end1-x (point-h start-pt))
    (set-chunk-slot-value-fct v-o 'end1-y (point-v start-pt))
    (set-chunk-slot-value-fct v-o 'end2-x (point-h end-pt))
    (set-chunk-slot-value-fct v-o 'end2-y (point-v end-pt))
    v-o))|#

;;; VIEW-DRAW-CONTENTS [Method]
;;; Description : A bu-liner is just a line-feature located "at" it's mid-point.

(defmethod build-vis-locs-for ((lnr bu-liner) (vis-mod vision-module))
  "Convert the view to a feature to be placed into the visual icon"
  (let* ((start-pt (add-points (view-position lnr)
                               (make-point 0 (1- (point-v (view-size lnr))))))
         (end-pt (add-points (view-position lnr) 
                             (make-point (1- (point-h (view-size lnr))) 0)))
         (f (car (define-chunks-fct `((isa visual-location
                                           color ,(system-color->symbol (color lnr))
                                           value line
                                           kind line
                                           screen-x ,(loc-avg (point-h start-pt) (point-h end-pt))
                                           screen-y ,(loc-avg (point-v start-pt) (point-v end-pt))
                                           width ,(abs (- (point-h start-pt) (point-h end-pt)))
                                           height ,(abs (- (point-v start-pt) (point-v end-pt)))))))))
    
    (setf (chunk-visual-object f) lnr)
    f))

#|(defmethod vis-loc-to-obj ((lnr bu-liner) loc)
  (let ((start-pt (add-points (view-position lnr)
                               (make-point 0 (1- (point-v (view-size lnr))))))
        (end-pt (add-points (view-position lnr) 
                             (make-point (1- (point-h (view-size lnr))) 0)))
        (v-o (fill-default-vis-obj-slots (car (define-chunks (isa line))) loc)))
    (set-chunk-slot-value-fct v-o 'end1-x (point-h start-pt))
    (set-chunk-slot-value-fct v-o 'end1-y (point-v start-pt))
    (set-chunk-slot-value-fct v-o 'end2-x (point-h end-pt))
    (set-chunk-slot-value-fct v-o 'end2-y (point-v end-pt))
    v-o))|#


#|

;;; RPM-VIEW-LINE [Function]
;;; Description : Add a view to the window that displays a line defined by
;;;             : the start and end points in the color supplied (an MCL
;;;             : system style color).

(defun rpm-view-line (wind start-pt end-pt &optional (color *black-color*))
  "Adds a view in the specified window which draws a line from the start-pt to the end-pt
   using the optional color specified (defaulting to black).  This view will add features 
   to the icon on PM-PROC-DISPLAY."
  (let* ((gx (> (point-h end-pt) (point-h start-pt)))
         (gy (> (point-v end-pt) (point-v start-pt)))
         (vs (subtract-points start-pt end-pt)))
    (setf vs (make-point (+ 1 (abs (point-h vs)))
                         (+ 1 (abs (point-v vs)))))
    (add-subviews wind (cond ((and gx gy)
                              (make-instance 'td-liner
                                :color color
                                :view-position start-pt 
                                :view-size vs))
                             ((and (not gx) (not gy))
                              (make-instance 'td-liner
                                :color color
                                :view-position end-pt 
                                :view-size vs))
                             ((and gx (not gy))
                              (make-instance 'bu-liner
                                :color color
                                :view-position (make-point (point-h start-pt) (point-v end-pt))
                                :view-size vs))
                             (t
                              (make-instance 'bu-liner
                                :color color
                                :view-position (make-point (point-h end-pt) (point-v start-pt))
                                :view-size vs))))))

|#

;;;; ---------------------------------------------------------------------- ;;;;
;;;; Utilities
;;;; ---------------------------------------------------------------------- ;;;;

;;; XY->POINT      [Function]
;;; Description : Converts an (X Y) list into an MCL/Quickdraw point.

(defun xy->point (xy)
  "(x y) to point converstion function. Deprecated, use vpt2p instead."
  (declare (list xy))
  (make-point (first xy) (second xy)))


;;; P2XY      [Function]
;;; Description : Takes an MCL/Quickdraw point and returns an XY list

(defun p2xy (p)
  "Coverts an MCL/Quickdraw point to an XY list.  Deprecated, use p2vpt instead."
  ;  (declare (point p))
  (list (point-h p) (point-v p)))


(defun p2vpt (p)
  "Convert an MCL/Quickdraw point to #(x y) format."
  (declare (inline p2vpt))
  (vector (point-h p) (point-v p)))


(defun vpt2p (mpt)
  "Convert an #(x y) format point to MCL/Quickdraw format."
  (declare (vector mpt) (inline vpt2p))
  (make-point (px mpt) (py mpt)))


;;;; ---------------------------------------------------------------------- ;;;;
;;;; RPM device methods.
;;;; ---------------------------------------------------------------------- ;;;;

;;; DEVICE-HANDLE-KEYPRESS      [Method]
;;; Description : Just call VIEW-KEY-EVENT-HANDLER and make sure that the 
;;;             : event gets dealt with.

(defmethod device-handle-keypress ((device window) key)
  (view-key-event-handler device key)
  (event-dispatch))

#|
;;; VIEW-KEY-EVENT-HANDLER      [Method]
;;; Description : ACT-R couldn't actually type into editiable text dialog
;;;             : items because the default method required that 
;;;             : *current-event* be bound, which of course it wouldn't be.
;;;             : So hack around it.

(defmethod view-key-event-handler ((view editable-text-dialog-item) 
                                       (char character))
  (if (not (model-generated-action))
    (call-next-method)
    (progn
      (cond ((graphic-char-p char) (ed-insert-char view char))
            ((char= char #\backspace) (ed-rubout-char view))
            )
      (view-draw-contents view)))
  )

|#
;;; DEVICE-HANDLE-CLICK      [Method]
;;; Description : Again, just call the base MCL method and dispatch.

(defmethod device-handle-click ((device window))
  (left-mouse-click
    (local-to-global device (view-mouse-position device))))

#|
;;; DEVICE-MOVE-CURSOR-TO      [Method]
;;; Date        : 97.02.18 [revised 98.10.29]
;;; Description : Since moving the mouse is considered a Bad Thing by 
;;;             : Apple's HI police, you can't just make a simple call
;;;             : to do it.  First, there's moving the cursor, which
;;;             : involves blasting into low memory.  Then, if the cursor
;;;             : is being tracked by the system, we have to make sure that 
;;;             : the cursor move has really been registered (#$CrsrNew 
;;;             : changes from -1 to 255 when this happens) by the OS.  Then 
;;;             : make sure it's been registered by MCL with UPDATE-CURSOR.

#-ccl-4.3.1
(defmethod device-move-cursor-to ((device window) (xyloc vector))
  (let ((absloc (local-to-global device (px xyloc) (py xyloc))))
    (without-interrupts
     (ccl::%put-point (%int-to-ptr #$mtemp) absloc)
     (ccl::%put-point (%int-to-ptr #$rawmouse) absloc)
     (%put-word (%int-to-ptr #$crsrnew) -1))
    (while (eql (%get-signed-word (%int-to-ptr #$crsrnew)) -1))
    (update-cursor)
    (while (not (vpt= xyloc (p2vpt (view-mouse-position device))))
      (event-dispatch))))

#+(and :ccl-4.3.1 (not :ccl-5.0))
(defmethod device-move-cursor-to ((device window) (xyloc vector))
  (let ((absloc (local-to-global device (px xyloc) (py xyloc))))
    (without-interrupts
     ;(ccl::%put-point (%int-to-ptr #$MTemp) absloc)
     (#_lmsetmousetemp absloc)
     ;(ccl::%put-point (%int-to-ptr #$RawMouse) absloc)
     (#_lmsetrawmouselocation absloc)
     ;(%put-word (%int-to-ptr #$CrsrNew) -1)
     (#_lmsetcursornew -1)
     )
    ;(while (eql (%get-signed-word (%int-to-ptr #$CrsrNew)) -1))
    (while (eql (#_lmgetcursornew) -1))
    (update-cursor)
    (while (not (vpt= xyloc (p2vpt (view-mouse-position device))))
      (event-dispatch))))


(unless (fboundp 'speech-available-p)
  (defun speech-available-p () nil))



;;; DEVICE-SPEAK-STRING      [Method]
;;; Description : If the Mac Speech Manager is installed, actually speak the
;;;             : string.

(defmethod device-speak-string ((device window) string)
  (when (speech-available-p)
    (speak-string string)
    ))

|#

;;; GET-MOUSE-COORDINATES      [Method]
;;; Description : Return the current mouse loc in #(x y) format.

(defmethod get-mouse-coordinates ((device window))
  (p2vpt (view-mouse-position device)))

#|
;;; DEVICE-UPDATE      [Method]
;;; Date        : 03.03.11
;;; Description : Rather than calling EVENT-DISPATCH on every cycle, call it
;;;             : only at about 10Hz.

(defmethod device-update :after ((wind window) time)
  (declare (ignore wind time))
  (when (< 100 (- (get-internal-real-time) *last-update*))
    (event-dispatch)
    (setf *last-update* (get-internal-real-time)))
  )


|#

#|
(defmethod do-update :after ((mstr-proc master-process) current-time 
                               &key (real-wait nil))
  (declare (ignore current-time real-wait))
  (event-dispatch))
|#


#|

(defmethod populate-loc-to-key-array ((ar array))
  "Sets all the keys in the array that need to be set"
  ;; function key row
  (setf (aref ar 0 0) #\esc)
  (setf (aref ar 2 0) #\2061)
  (setf (aref ar 3 0) #\2062)
  (setf (aref ar 4 0) #\2063)
  (setf (aref ar 5 0) #\2064)
  (setf (aref ar 7 0) #\2065)
  (setf (aref ar 8 0) #\2066)
  (setf (aref ar 9 0) #\2067)
  (setf (aref ar 10 0) #\2070)
  (setf (aref ar 12 0) #\2071)
  (setf (aref ar 13 0) #\2101)
  (setf (aref ar 14 0) #\2102)
  (setf (aref ar 15 0) #\2103)
  (setf (aref ar 17 0) #\2014)
  (setf (aref ar 18 0) #\2015)
  (setf (aref ar 19 0) #\2016)
  ;; numeric key row
  (setf (aref ar 0 2) #\tab)
  (setf (aref ar 1 2) #\1)
  (setf (aref ar 2 2) #\2)
  (setf (aref ar 3 2) #\3)
  (setf (aref ar 4 2) #\4)
  (setf (aref ar 5 2) #\5)
  (setf (aref ar 6 2) #\6)
  (setf (aref ar 7 2) #\7)
  (setf (aref ar 8 2) #\8)
  (setf (aref ar 9 2) #\9)
  (setf (aref ar 10 2) #\0)
  (setf (aref ar 11 2) #\-)
  (setf (aref ar 12 2) #\=)
  (setf (aref ar 13 2) #\delete)
  (setf (aref ar 15 2) #\help)
  (setf (aref ar 16 2) #\home)
  (setf (aref ar 17 2) #\pageup)
  (setf (aref ar 19 2) #\esc)
  (setf (aref ar 20 2) #\=)
  (setf (aref ar 21 2) #\/)
  (setf (aref ar 22 2) #\*)
  ;; qwerty row
  (setf (aref ar 0 3) #\tab)
  (setf (aref ar 1 3) #\q)
  (setf (aref ar 2 3) #\w)
  (setf (aref ar 3 3) #\e)
  (setf (aref ar 4 3) #\r)
  (setf (aref ar 5 3) #\t)
  (setf (aref ar 6 3) #\y)
  (setf (aref ar 7 3) #\u)
  (setf (aref ar 8 3) #\i)
  (setf (aref ar 9 3) #\o)
  (setf (aref ar 10 3) #\p)
  (setf (aref ar 11 3) #\[)
  (setf (aref ar 12 3) #\])
  (setf (aref ar 13 3) #\\)
  (setf (aref ar 15 3) #\del)
  (setf (aref ar 16 3) #\end)
  (setf (aref ar 17 3) #\page)
  (setf (aref ar 19 3) #\7)
  (setf (aref ar 20 3) #\8)
  (setf (aref ar 21 3) #\9)
  (setf (aref ar 22 3) #\-)
  ;; ASDF row
  (setf (aref ar 0 4) 'caps-lock)
  (setf (aref ar 1 4) #\a)
  (setf (aref ar 2 4) #\s)
  (setf (aref ar 3 4) #\d)
  (setf (aref ar 4 4) #\f)
  (setf (aref ar 5 4) #\g)
  (setf (aref ar 6 4) #\h)
  (setf (aref ar 7 4) #\j)
  (setf (aref ar 8 4) #\k)
  (setf (aref ar 9 4) #\l)
  (setf (aref ar 10 4) #\;)
  (setf (aref ar 11 4) #\')
  (setf (aref ar 12 4) #\newline)
  (setf (aref ar 13 4) #\newline)
  (setf (aref ar 19 4) #\4)
  (setf (aref ar 20 4) #\5)
  (setf (aref ar 21 4) #\6)
  (setf (aref ar 22 4) #\+)
  ;; Z row
  (setf (aref ar 0 5) 'shift)
  (setf (aref ar 1 5) #\z)
  (setf (aref ar 2 5) #\x)
  (setf (aref ar 3 5) #\c)
  (setf (aref ar 4 5) #\v)
  (setf (aref ar 5 5) #\b)
  (setf (aref ar 6 5) #\n)
  (setf (aref ar 7 5) #\m)
  (setf (aref ar 8 5) #\,)
  (setf (aref ar 9 5) #\.)
  (setf (aref ar 10 5) #\/)
  (setf (aref ar 11 5) 'shift)
  (setf (aref ar 12 5) 'shift)
  (setf (aref ar 16 5) #\uparrow)
  (setf (aref ar 19 5) #\1)
  (setf (aref ar 20 5) #\2)
  (setf (aref ar 21 5) #\3)
  (setf (aref ar 22 5) #\enter)
  ;; space bar row
  (setf (aref ar 0 6) 'control)
  (setf (aref ar 1 6) 'option)
  (setf (aref ar 2 6) 'command)
  (setf (aref ar 3 6) #\space)
  (setf (aref ar 4 6) #\space)
  (setf (aref ar 5 6) #\space)
  (setf (aref ar 6 6) #\space)
  (setf (aref ar 7 6) #\space)
  (setf (aref ar 8 6) #\space)
  (setf (aref ar 9 6) #\space)
  (setf (aref ar 10 6) #\space)
  (setf (aref ar 11 6) 'command)
  (setf (aref ar 12 6) 'option)
  (setf (aref ar 13 6) 'control)
  (setf (aref ar 15 6) #\backarrow)
  (setf (aref ar 16 6) #\downarrow)
  (setf (aref ar 17 6) #\forwardarrow)
  (setf (aref ar 19 6) #\0)
  (setf (aref ar 20 6) #\0)
  (setf (aref ar 21 6) #\.)
  (setf (aref ar 22 6) #\enter)
  ar)

|#

;;;; ---------------------------------------------------------------------- ;;;;
;;;; RPM overlay and Focus ring stuff


;;; RPM-OVERLAY      [Class]
;;; Description : If you want a view to be superimposed on a window, but not
;;;             : be visible to RPM, use this class.  The focus ring in RPM
;;;             : is a subclass.
;;;
;;;             : The OFFSET slot is for the difference between the center of
;;;             : the view and the upper-left corner, as a QuickDraw point.
;;;             : For example, for the focus ring its #@(-10 -10).

(defclass rpm-overlay (simple-overlay-view)
  ((offset :accessor offset :initarg :offset :initform nil)))


(defgeneric update-me (olay wind xyloc)
  (:documentation "Call this to move the overlay to a specific location within a window."))

(defmethod update-me ((olay rpm-overlay) (wind window) (xyloc vector))
  (set-view-position olay (add-points (offset olay) (vpt2p xyloc)))
  (unless (equal (view-window olay) wind) (add-subviews wind olay))
  (event-dispatch)
  (when (wptr (view-window olay)) (view-draw-contents olay)))


;;; BUILD-FEATURES-FOR      [Method]
;;; Description : We don't want icon features for the focus ring, and since 
;;;             : it'll be a subview a null BUILD-FEATURES-FOR method is 
;;;             : necessary.

(defmethod build-vis-locs-for ((olay rpm-overlay) (vm vision-module))
  (declare (ignore olay vm))
  nil)

;;; POINT-IN-CLICK-REGION-P      [Method]
;;; Description : The focus ring will generally be the "front" view, but 
;;;             : having it receive clicks is a Bad Thing (tm) so it's 
;;;             : necessary to override the POINT-IN-CLICK-REGION-P method
;;;             : for this view class.

(defmethod point-in-click-region-p ((olay rpm-overlay) where)
  (declare (ignore olay where))
  nil)


;;; here's the actual focus ring itself

(defclass focus-ring (rpm-overlay)
  ((color :accessor color :initarg :color :initform *red-color*))
  (:default-initargs 
    :view-size #@(19 19)
    :offset #@(-10 -10)))


(defmethod view-draw-contents ((self focus-ring))
  (let ((oldmode (pen-mode self))
        (oldpat (pen-pattern self))
        (oldsize (pen-size self)))
    (set-pen-mode self :pator)
    (set-pen-pattern self *light-gray-pattern*)
    (set-pen-size self 4 4)
    (with-focused-view self
      (with-fore-color (color self)
        (frame-oval self #@(0 0) (view-size self))))
    (set-pen-mode self oldmode)
    (set-pen-pattern self oldpat)
    (set-pen-size self (point-h oldsize) (point-v oldsize))
    ))


;;; DEVICE-UPDATE-ATTENDED-LOC      [Method]
;;; Date        : 00.07.11
;;; Description : When the attended location is updated, update the focus
;;;             : ring.

(defmethod device-update-attended-loc ((wind window) xyloc)
  (update-me *attn-tracker* wind xyloc))


;;; make the fous ring

(eval-when (load eval)
  (setf *attn-tracker* (make-instance 'focus-ring)))

;;;; ---------------------------------------------------------------------- ;;;;
;;;; color text stuff

#|
(defun system-color->symbol (color)
  "Given an MCL color code, return a symbol representing that color.  Unknown colors get mapped to COLOR-RRRRR-GGGGG-BBBBB."
  (if (null color)
    'black
    (case color
      (#.*black-color* 'black)
      (#.*green-color* 'green)
      (#.*red-color* 'red)
      (#.*blue-color* 'blue)
      (#.*brown-color* 'brown)
      (#.*purple-color* 'purple)
      (#.*pink-color* 'pink)
      (#.*orange-color* 'orange)
      (#.*dark-gray-color* 'dark-gray)
      (#.*light-blue-color* 'light-blue)
      (#.*white-color* 'white)
      (#.*light-gray-color* 'light-gray)
      (#.*dark-green-color* 'dark-green)
      (#.*tan-color* 'tan)
      (#.*yellow-color* 'yellow)
      (otherwise (intern (format nil "COLOR-~5,'0d-~5,'0d-~5,'0d" 
                                 (color-red color) 
                                 (color-green color) 
                                 (color-blue color)))))))

|#

;;;; ---------------------------------------------------------------------- ;;;;
;;;; handling mouse movement under MCL 5.0 and OS X.

#+(and :ccl-5.0 (not :ccl-5.2))
(when (osx-p)
(progn

(defparameter *warp* (lookup-function-in-framework 
                       "CGWarpMouseCursorPosition"))

(defmethod device-move-cursor-to ((device window) (xyloc vector))
  (when (and device (wptr device)) (window-select device))
  (setf xyloc (local-to-global device (vpt2p xyloc)))
  (ccl::ppc-ff-call *warp* 
                    :single-float (coerce (point-h xyloc) 'short-float)
                    :single-float (coerce (point-v xyloc) 'short-float)
                    :unsigned-fullword)
  )))


;;; under MCL 5.2, can't use CFBundle, but there are alternate ways to 
;;; deal with framework calls, so use that.

#+ccl-5.2
(when (osx-p)
  (progn
    (defparameter *warp* (ccl::lookup-function-in-bundle 
                          "CGWarpMouseCursorPosition"
                          (ccl::load-framework-bundle "ApplicationServices.framework")))

    (defmethod device-move-cursor-to ((device window) (xyloc vector))
      (when (and device (wptr device)) (window-select device))
      (setf xyloc (local-to-global device (vpt2p xyloc)))
      (ccl::ppc-ff-call *warp* 
                        :single-float (coerce (point-h xyloc) 'short-float)
                        :single-float (coerce (point-v xyloc) 'short-float)
                        :unsigned-fullword))
    ))




#|
This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
|#
; ----------------------------------------------------------------------
; End file: actr6/devices/ccl/device.lisp
; ----------------------------------------------------------------------


; ----------------------------------------------------------------------
; Begin file: actr6/devices/ccl/uwi.lisp
; ----------------------------------------------------------------------


;;;  -*- mode: LISP; Package: CL-USER; Syntax: COMMON-LISP;  Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Author      : Dan Bothell 
;;; Address     : Carnegie Mellon University
;;;             : Psychology Department
;;;             : Pittsburgh,PA 15213-3890
;;;             : db30+@andrew.cmu.edu
;;; 
;;; Copyright   : (c)2002-2004 Dan Bothell
;;; Availability: Covered by the GNU LGPL, see LGPL.txt
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Filename    : uwi.lisp
;;; Version     : 1.0a1
;;; 
;;; Description : MCL-specific functions to implement the UWI.
;;;             : NOTE: The UWI is only still around to support the 
;;;             :       ACT-R GUI interface. I don't advocate using it directly.      
;;; 
;;; Bugs        : 
;;; --- History ---
;;; 2002.06.30 Dan
;;;             : Added this header.
;;;             : Moved all of the UWI code from mcl-interface
;;;             : to this file where it belongs.
;;;             : Actually documented the code!
;;; 2002.12.19 Dan
;;;             : Modified the window class and make-static-text-...
;;;             : so that it can handle the color attribute.
;;; 04.04.13 Dan [2.2] (previous change is "new" as of 2.2 as well)
;;;             : Changed the copyright notice and added the LGPL stuff.
;;;
;;; 04.10.19 Dan [Moved into ACT-R 6]
;;;             : Reset the version to 1.0a1
;;;             : added the packaging switches
;;;             : changed the name to uwi to be placed in a folder called mcl
;;; 2007.07.13 Dan
;;;             : * Added the color keyword to make-button-for-rpm-window
;;;             :   though it's not actually used at this point.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+:packaged-actr (in-package :act-r)
#+(and :clean-actr (not :packaged-actr) :ALLEGRO-IDE) (in-package :cg-user)
#-(or (not :clean-actr) :packaged-actr :ALLEGRO-IDE) (in-package :cl-user)

;;; RPM-REAL-WINDOW  [Class]
;;; Description : This is the UWI's window class to produce an MCL.
;;;             : It inherits from the MCL dialog class (a real window) and
;;;             : the rpm-window class which is an abstract class used by the
;;;             : ACT-R GUI interface.

(defclass rpm-real-window (rpm-window color-dialog)
  ())

;;; VIEW-KEY-EVENT-HANDLER  [Method]
;;; Description : The method called when a key is pressed.  It
;;;             : just calls the rpm-window-key-event-handler which is
;;;             : to be defined by the modeler.

(defmethod view-key-event-handler ((device rpm-real-window) key)
  (rpm-window-key-event-handler device key))

;;; RPM-WINDOW-KEY-EVENT-HANDLER  [Method]
;;; Description : The UWI method called when a key is pressed.  
;;;             : This is just a default that does nothing because the
;;;             : modeler is supposed to define this.

(defmethod rpm-window-key-event-handler ((device rpm-real-window) key)
  (declare (ignore device key))
  (call-next-method))

;;; VIEW-CLICK-EVENT-HANDLER  [Method]
;;; Description : The method called when a mouse click occurs.  It
;;;             : just calls the rpm-window-click-event-handler with the
;;;             : mouse position converted to a list. 
;;;             : The rpm-window-click-event-handler is supposed 
;;;             : to be defined by the modeler.

(defmethod view-click-event-handler ((device rpm-real-window) position)
  (rpm-window-click-event-handler device 
                                  (list (point-h position) (point-v position)))
  (call-next-method))

;;; RPM-WINDOW-CLICK-EVENT-HANDLER  [Method]
;;; Description : The UWI method called when the mouse is clicked.  
;;;             : This is just a default that does nothing because the
;;;             : modeler is supposed to define this.

(defmethod rpm-window-click-event-handler ((device rpm-real-window) position)
  (declare (ignore device position))
  (call-next-method))


;;;; ---------------------------------------------------------------------- ;;;;
;;;; These are the UWI Methods.
;;;; ---------------------------------------------------------------------- ;;;;

;;; OPEN-RPM-WINDOW?  [Method]
;;; Description : Returns t if the window is open and nil if not.

(defmethod open-rpm-window? ((win rpm-real-window))
  (wptr win))

;;; CLOSE-RPM-WINDOW  [Method]
;;; Description : Closes the window.

(defmethod close-rpm-window  ((win rpm-real-window))
  (window-close win))

;;; SELECT-RPM-WINDOW  [Method]
;;; Description : Brings the specified window to the foreground.

(defmethod select-rpm-window ((win rpm-real-window))
  (window-select win))

;;; ADD-VISUAL-ITEMS-TO-RPM-WINDOW  [Method]
;;; Description : Makes the specified items subviews of the window and
;;;             : calls view-draw-contents and event-dispatch to make sure
;;;             : that they show up.

(defmethod add-visual-items-to-rpm-window ((win rpm-real-window) &rest items )
  (dolist (item items)
    (add-subviews win item))
  (view-draw-contents win)
  (event-dispatch))

;;; REMOVE-VISUAL-ITEMS-FROM-RPM-WINDOW  [Method]
;;; Description : Take the specified items out of the subviews of the
;;;             : window and make it redraw.

(defmethod remove-visual-items-from-rpm-window ((win rpm-real-window) 
                                                &rest items)
  (dolist (item items)
    (remove-subviews win item))
  (view-draw-contents win)
  (event-dispatch))

;;; REMOVE-ALL-ITEMS-FROM-RPM-WINDOW  [Method]
;;; Description : Remove all the subvies of the window and redisplay it.

(defmethod remove-all-items-from-rpm-window ((win rpm-real-window))
  (apply #'remove-subviews win (subviews win))
  (view-draw-contents win)
  (event-dispatch))

;;; RPM-WINDOW-TITLE  [Method]
;;; Description : Return the title of the window.

(defmethod rpm-window-title ((win rpm-real-window))
  (window-title win))

;;; RPM-WINDOW-VISIBLE-STATUS  [Method]
;;; Description : Return t to indicate that this is a visible window.

(defmethod rpm-window-visible-status ((win rpm-real-window))
  t)

;;; MAKE-RPM-WINDOW  [Function]
;;; Description : Make and return a window based on the parameters supplied.
;;;             : Visible determines wheter or not it should be a real or
;;;             : virtual and if the environment is connected it will use a 
;;;             : visible-virtual for the real window unless the user explicitly
;;;             : specifies the class to use.

(defun make-rpm-window (&key (visible nil) (class nil) (title "RPM Window") 
                             (width 100) (height 100) (x 0 ) (y 0))
  "Make and return a window for use with the UWI"
  (if visible
    (if (and (visible-virtuals-available?) (null class))
      (make-instance 'visible-virtual-window :window-title title 
                     :width width :height height :x-pos x :y-pos y)
      (make-instance (if class class 'rpm-real-window) 
                     :window-title title :view-size (make-point width height) 
                     :view-position (make-point x y)))
    (make-instance (if class class 'rpm-virtual-window) :window-title title 
                   :width width :height height :x-pos x :y-pos y)))


;;; MAKE-BUTTON-FOR-RPM-WINDOW  [Method]
;;; Description : Build and return a button-dialog-item based on the
;;;             : parameters supplied.

(defmethod make-button-for-rpm-window ((win rpm-real-window) &key (x 0) (y 0) 
                                                             (text "Ok") (action nil) (height 18)  
                                                             (width 60) (color 'gray))
  (make-dialog-item 'button-dialog-item
                    (make-point x y)
                    (make-point width height)
                    text
                    action
                    :default-button nil))

;;; MAKE-STATIC-TEXT-FOR-RPM-WINDOW  [Method]
;;; Description : Build and return a static-text-dialog-item based on the
;;;             : parameters supplied.

(defmethod make-static-text-for-rpm-window ((win rpm-real-window) 
                                            &key (x 0) (y 0) (text "") 
                                            (height 20) (width 80) (color 'black))
  (let ((item (make-dialog-item 'static-text-dialog-item
                                (make-point x y)
                                (make-point width height)
                                text
                                )))
    (set-part-color item :text (color-symbol->system-color color))
    item))


;;; MAKE-LINE-FOR-RPM-WINDOW  [Method]
;;; Description : Build and return the appropriate liner object for the
;;;             : window based on the parameters supplied.

(defmethod make-line-for-rpm-window ((wind rpm-real-window) start-pt end-pt &optional (color 'black))
  (destructuring-bind (startx starty) start-pt
    (destructuring-bind (endx endy) end-pt
      (unless (> endx startx)
        (rotatef startx endx)
        (rotatef starty endy))
      (let ((vs (make-point (+ 1 (abs (- endx startx)))
                            (+ 1 (abs (- endy starty)))))
            (vp (make-point startx (min starty endy))))
        (make-instance (if (> endy starty) 'td-liner 'bu-liner)
                       :position vp
                       :size vs
                       :fore-color (color-symbol->system-color color))))))

;;; ALLOW-EVENT-MANAGER  [Method]
;;; Description : Call event-dispatch.  This is used while waiting for
;;;             : a person to respond.

(defmethod allow-event-manager ((win rpm-real-window))
  (event-dispatch))

#|
This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
|#
; ----------------------------------------------------------------------
; End file: actr6/devices/ccl/uwi.lisp
; ----------------------------------------------------------------------


