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
   (easygui::foreground :initarg :color)
   (temp-view-subviews :initarg :view-subviews)))

; Try to keep the class hierarchy of the public interface the same as it is for MCL.
; So, simple-view is top; then view (allows subviews); then types that inherit from view,
; like, window, dialog stuff, etc.

(defclass simple-view (easygui::simple-view view-mixin) ())

(defclass view (simple-view)
  ((pen-position :accessor pen-position :initarg :pen-position :initform (make-point 0 0)))
  (:documentation "Top-level class for views"))

(defclass window (view easygui:window view-text-via-title-mixin) ())

(defclass simple-overlay-view (view easygui::drawing-overlay-view) 
  ()
  (:documentation "Top-level class for views that do not monitor mouse clicks and mouse movement"))

(defclass color-dialog (window)
  ()
  (:documentation "Top-level class for windows"))

(defclass liner (view easygui:drawing-view) ())

(defclass td-liner (liner) ())

(defclass bu-liner (liner) ())

(defclass button-dialog-item (easygui:push-button-view view view-text-via-title-mixin easygui::text-fonting-mixin)
  ((easygui::default-button-p :initarg :default-button)))

(defclass static-text-dialog-item (easygui:static-text-view view view-text-via-stringvalue-mixin)
  ((text-justification :accessor text-justification :initarg :text-justification)
   (part-color-list :accessor part-color-list :initarg :part-color-list)))

(defclass check-box-dialog-item (easygui:check-box-view view view-text-via-title-mixin) ())

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
                    :action (lambda () (funcall action obj))
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

(defconstant $tejustleft :left)
(defconstant $tejustcenter :center)
(defconstant $tejustright :right)

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

(defmethod view-named (name (view view))
  (easygui:view-named name view))

(defmethod window-select ((win window))
  (easygui:window-show win))

(defmethod window-close ((win window))
  (easygui:perform-close win))

(defmethod window-title ((view window))
  ;TODO: Maybe use easygui:view-text method here?
  (easygui::window-title view))

(defmethod dialog-item-text ((view easygui::view-text-mixin))
  (easygui:view-text view))

(defmethod set-dialog-item-text ((view easygui::view-text-mixin) text)
  (setf (easygui:view-text view) text))

(defmethod dialog-item-enable ((view easygui::action-view-mixin))
  (easygui:set-dialog-item-enabled-p view t))

(defmethod dialog-item-disable ((view easygui::action-view-mixin))
  (easygui:set-dialog-item-enabled-p view nil))

(defmethod view-position ((view simple-view))
  (easygui:view-position view))

(defmethod set-view-position ((view simple-view) x &optional (y nil))
  (let ((pos (if y
               (make-point x y)
               x)))
    (setf (easygui:view-position view) pos)))

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

(defmethod get-fore-color ((view simple-view))
  (easygui:get-fore-color view))

(defmethod part-color ((view easygui:static-text-view) part)
  (declare (ignore part))
  (get-fore-color view))

(defmethod color ((view simple-view))
  (get-fore-color view))

(defmethod set-fore-color ((view simple-view) new-color)
  (easygui:set-fore-color view new-color))

(defmethod set-part-color ((view simple-view) part new-color)
  (declare (ignore part))
  (set-fore-color view new-color))

; Handling mouse movement/interaction

(defmethod easygui::mouse-down ((view easygui::drawing-view) &key location &allow-other-keys)
  (view-click-event-handler view location))

(defmethod view-click-event-handler ((device simple-view) position)
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

(defmethod easygui::initialize-view :after ((window color-dialog))
  (let ((view (make-instance 'easygui::drawing-view :accept-key-events-p t)))
    (setf (slot-value view 'easygui::parent) window)
    (setf (easygui::content-view window) view)
    (easygui::window-show window)))

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
  (declare (ignore view))
  ())

(defmethod view-draw-contents ((view easygui:drawing-view))
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

; Handling fonts and string width/height in pixels

; easygui expects the font slot to be initialized with an ns-font type. However, MCL uses the
; same slot name and expects the font slot to be initialized with a font spec as a list.
; So in order to hack it so that the font slot is correct for easygui, overwrite the initialize-view
; :after on the text-fonting-mixin class, and canonicalize the font slot at that point.
; I would have liked to have writtin an initialize-instance :after on a class named here, but because
; the slot name for the MCL spec and easygui are the same, I saw no way to do this other than to just write
; over easygui's method. Kinda' hacky, but it seems to work.

(defun convert-font (font)
  (etypecase font
    (ns:ns-font font)
    (list 
      (destructuring-bind (name pt &rest rest) font
        (#/fontWithName:size: ns:ns-font
         (objc:make-nsstring name)
         pt)))))

(defmethod easygui::initialize-view :after ((view easygui::text-fonting-mixin))
  (when (slot-value view 'easygui::font)
    (setf (slot-value view 'easygui::font) (convert-font (slot-value view 'easygui::font)))
    (easygui:dcc (#/setFont: (easygui:cocoa-ref view) (slot-value view 'easygui::font)))))

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

(defun open-resource-file (arg-0 &key if-does-not-exist errorp direction perm data-fork-p)
  (print arg-0))
  
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

(defun color-symbol->system-color (symb)
    (destructuring-bind (red green blue) (color-symbol->rgb symb)
          (easygui:make-rgb :red red :green green :blue blue)))

(defun system-color->symbol (color)
    (let ((red (easygui:rgb-red color))
                  (green (easygui:rgb-green color))
                          (blue (easygui:rgb-blue color)))
          (rgb->color-symbol (list red green blue))))

(defparameter *black-color* (color-symbol->system-color 'black))
(defparameter *red-color* (color-symbol->system-color 'red))
(defparameter *light-gray-pattern* (color-symbol->system-color 'gray))

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
