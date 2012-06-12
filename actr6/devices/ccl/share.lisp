(require :cocoa)
(require :easygui)

(shadowing-import 'easygui:window)
(shadowing-import 'easygui:view)

(setf (symbol-function 'point-v) #'easygui:point-y)
(setf (symbol-function 'point-h) #'easygui:point-x)
(import 'easygui:point-x)
(import 'easygui:point-y)

(defun make-point (x y)
  (make-instance 'easygui::eg-point :x x :y y))

(defmethod add-points ((p1 easygui::eg-point) (p2 easygui::eg-point))
  (make-point
    (+ (point-x p1) (point-x p2))
    (+ (point-y p1) (point-y p2))))

(defmethod add-subviews ((view easygui:view) &rest subviews)
  (when subviews
    (apply #'easygui:add-subviews view subviews)))

(defmethod remove-subviews ((view easygui:view) &rest subviews)
  (when subviews
    (apply #'easygui:remove-subviews view subviews)))

(defmethod subviews ((view view) &optional subview-type)
  (declare (ignore subview-type))
  (easygui:view-subviews view))

(defmethod local-to-global ((view view) local-pos)
  (let ((view-pos (easygui:view-position view)))
    (make-point
      (+ (point-h view-pos) (point-h local-pos))
      (+ (point-v view-pos) (point-v local-pos)))))

(defmethod window-select ((win window))
  (easygui:window-show win))

(defmethod window-close ((win window))
  (easygui:perform-close win))

(defmethod view-mouse-position ((view view))
  (easygui:view-mouse-position view))

(defmethod wptr ((view view))
  (#/isVisible (easygui::cocoa-ref view)))

(defmethod device-move-cursor-to ((device window) (xyloc vector))
  (setf xyloc (local-to-global device (vpt2p xyloc)))
  (#_CGWarpMouseCursorPosition (ns:make-ns-point (point-h xyloc)
                                                 (point-v xyloc))))

(defmethod view-draw-contents ((view view))
  (declare (ignore view))
  ())

(defun event-dispatch ()
  ())

(defmethod window-title ((view view))
  ;TODO: Maybe use easygui:view-text method here?
  (easygui::window-title view))

(defparameter *black-color* 'black)
(defparameter *red-color* 'red)
(defparameter *light-gray-pattern* 'gray)


(defclass view-text-via-title-mixin (easygui::view-text-via-title-mixin)
  ((easygui::text :initarg :window-title)))

(defclass view-text-via-stringvalue-mixin (easygui::view-text-via-stringvalue-mixin)
  ((easygui::text :initarg :text)))

(defclass view-mixin (easygui:view)
  ((easygui::size :initarg :view-size)
   (easygui::position :initarg :view-position)
   (easygui::foreground :initarg :color)))

(defclass simple-view (view-mixin easygui:drawing-view)
  ((pen-position :accessor pen-position :initarg :pen-position :initform (make-point 10 0))))

(defclass color-dialog (view-text-via-title-mixin view-mixin easygui:window) ())

(defclass liner (simple-view) ())

(defclass td-liner (liner) ())

(defclass bu-liner (liner) ())

(defclass button-dialog-item (view-text-via-stringvalue-mixin view-mixin easygui:push-button-view)
   ((easygui::default-button-p :initarg :default-button)))

(defclass static-text-dialog-item (view-text-via-stringvalue-mixin view-mixin easygui:static-text-view) ())

#|
(defmethod move-to ((view simple-view) position)
  (setf (pen-position view) position))

(defmethod line-to ((view simple-view) position)
  (destructuring-bind (startx starty) (list (point-x (pen-position view))
                                            (point-y (pen-position view)))
    (destructuring-bind (endx endy) (list (point-x position)
                                          (point-y position))
      (with-focused-view view
        (#/set (slot-value view 'easygui::foreground))
        (#/strokeLineFromPoint:toPoint:
         ns:ns-bezier-path
         (ns:make-ns-point startx starty) 
         (ns:make-ns-point endx endy))))))
|#

(defmethod get-start ((view td-liner))
  (list 0 (easygui:point-y (view-size view))))

(defmethod get-start ((view bu-liner))
  (list 0 0))

(defmethod get-end ((view td-liner))
  (list (easygui:point-x (view-size view)) 0))

(defmethod get-end ((view bu-liner))
  (list (easygui:point-x (view-size view))
        (easygui:point-y (view-size view))))

(defmethod view-draw-contents ((view liner))
  ; TODO Use with-fore-color instead of set explicitly here
  (#/set (get-fore-color view))
  (destructuring-bind (startx starty) (get-start view)
    (destructuring-bind (endx endy) (get-end view)
      (#/strokeLineFromPoint:toPoint:
       ns:ns-bezier-path
       (ns:make-ns-point startx starty) 
       (ns:make-ns-point endx endy)))))

(defun make-dialog-item (class position size text &optional action &rest attributes)
  (apply #'make-instance 
         class
         :view-position position
         :view-size size
         :text text
         :action action
         attributes))

(defmethod get-fore-color ((view easygui:view))
  (easygui:get-fore-color view))

(defmethod part-color ((view easygui:static-text-view) part)
  (declare (ignore part))
  (get-fore-color view))

(defmethod set-fore-color ((view easygui:view) new-color)
  (easygui:set-fore-color view new-color))

(defmethod set-part-color ((view easygui:view) part new-color)
  (declare (ignore part))
  (set-fore-color view new-color))

(defmethod easygui::mouse-down ((view easygui::drawing-view) &key location &allow-other-keys)
  (view-click-event-handler view location))

(defmethod view-click-event-handler ((device easygui:view) position)
  (awhen (easygui:view-container device) 
    (view-click-event-handler it position)))

(defmethod easygui::view-key-event-handler ((device color-dialog) key)
  (view-key-event-handler device key))

(defmethod easygui::initialize-view :after ((window color-dialog))
  (let ((view (make-instance 'simple-view :accept-key-events-p t)))
    (setf (slot-value view 'easygui::parent) window)
    (setf (easygui::content-view window) view)
    (easygui::window-show window)))

(defun font-info (font-spec)
  (values 0 0))

(defun string-width (&rest args)
  1)

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

(defmacro with-fore-color (color &body body)
  `(progn
     ,@body))

(defmacro with-focused-view (view &body body)
  `(easygui:with-focused-view (easygui:cocoa-ref ,view)
     ,@body))

(defmethod dialog-item-text ((view easygui:static-text-view))
  (easygui:view-text view))

(defmethod view-font ((view easygui:static-text-view))
  (easygui:view-font view))

(objc:defmethod (#/drawRect: :void) ((self easygui::cocoa-drawing-view)
                                     (rect :<NSR>ect))
                (easygui::dcc (view-draw-contents (easygui::easygui-view-of self))))

