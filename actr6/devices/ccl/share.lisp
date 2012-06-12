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

(defmethod subviews ((view easygui:view) &optional subview-type)
  (declare (ignore subview-type))
  (easygui:view-subviews view))

(defmethod local-to-global ((view easygui:view) local-pos)
  (let ((view-pos (easygui:view-position view)))
    (make-point
      (+ (point-h view-pos) (point-h local-pos))
      (+ (point-v view-pos) (point-v local-pos)))))

(defmethod window-select ((win easygui:window))
  (easygui:window-show win))

(defmethod window-close ((win easygui:window))
  (easygui:perform-close win))

(defmethod view-mouse-position ((view easygui:view))
  (easygui:view-mouse-position view))

(defmethod wptr ((view easygui:view))
  (#/isVisible (easygui::cocoa-ref view)))

(defmethod device-move-cursor-to ((device window) (xyloc vector))
  (setf xyloc (local-to-global device (vpt2p xyloc)))
  (#_CGWarpMouseCursorPosition (ns:make-ns-point (point-h xyloc)
                                                 (point-v xyloc))))

(defmethod view-draw-contents ((win easygui:view))
  (declare (ignore win))
  ())

(defun event-dispatch ()
  ())

(defmethod window-title ((view easygui:view))
  ;TODO: Maybe use easygui:view-text method here?
  (easygui::window-title view))

(defparameter *black-color* 'black)
(defparameter *red-color* 'red)
(defparameter *light-gray-pattern* 'gray)

#|
(load-os-constant 'e)
(print #$TEJUSTLEFT)
(ccl::%LOAD-VAR 'X86-DARWIN64::TEJUSTLEFT NIL)
(maphash #'print-hash-entry (ccl::fvs))
(gethash "NSKernAttributeName" *t*)
(gethash "tejustleft" *t*)
(print #$NSLinkAttributeName)
(ccl::extract-db-type 0 ccl::*target-ftd*)
(print (svref ccl::*signed-integer-types* 64))
(ccl::%cons-foreign-variable "tejustleft" 'integer)
(setf *fv* *)
(print ccl::*foreign-type-classes*)
(maphash #'print-hash-entry ccl::*foreign-type-classes*)
;(ccl::%cons-foreign-variable "tejustleft" (gethash 'INTEGER ccl::*foreign-type-classes*)))
(setf (gethash "tejustleft" (ccl::fvs))
      (ccl::%cons-foreign-variable "tejustleft" (svref ccl::*signed-integer-types* 64) ))
(ccl::resolve-foreign-variable "tejustleft")
(defun print-hash-entry (key value)
  (format t "The value associated with the key ~S is ~S~%" key value))
(alexandria:maphash-keyskccl::fvs)
(defconstant os::TEJUSTLEFT 0)
(print os::TEJUSTLEFT)
|#

(defclass view-text-via-title-mixin (easygui::view-text-via-title-mixin)
  ((easygui::text :initarg :window-title)))

(defclass view-text-via-stringvalue-mixin (easygui::view-text-via-stringvalue-mixin)
  ((easygui::text :initarg :text)))

(defclass view-mixin (easygui:view)
  ((easygui::size :initarg :view-size)
   (easygui::position :initarg :view-position)
   (easygui::foreground :initarg :color)))

(defclass color-dialog (view-text-via-title-mixin view-mixin easygui:window) ())

(defclass td-liner (view-mixin easygui::td-liner) ())

(defclass bu-liner (view-mixin easygui::bu-liner) ())

(defclass button-dialog-item (view-text-via-stringvalue-mixin view-mixin easygui:push-button-view)
   ((easygui::default-button-p :initarg :default-button)))

(defclass static-text-dialog-item (view-text-via-stringvalue-mixin view-mixin easygui:static-text-view) ())

(defclass simple-view (view-mixin easygui:drawing-view) ())

;(defclass simple-view (td-liner) ())

(defun make-dialog-item (class position size text &optional action &rest attributes)
  (apply #'make-instance 
         class
         :view-position position
         :view-size size
         :text text
         :action action
         attributes))

(defmethod part-color ((view easygui:static-text-view) part)
  (declare (ignore part))
  (easygui:get-fore-color view))

(defmethod set-part-color ((view easygui:view) part new-color)
  (declare (ignore part))
  (easygui:set-fore-color view new-color))

(defmethod easygui::mouse-down ((view easygui::drawing-view) &key location &allow-other-keys)
  (view-click-event-handler view location))

(defmethod view-click-event-handler ((device easygui:view) position)
  (awhen (easygui:view-container device) 
    (view-click-event-handler it position)))

(defmethod easygui::view-key-event-handler ((device color-dialog) key)
  (view-key-event-handler device key))

(defmethod easygui::initialize-view :after ((window color-dialog))
  (let ((view (make-instance 'easygui::drawing-view :accept-key-events-p t)))
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
  (assert left)
  (assert top)
  (assert (not right))
  (assert (not bottom)))

(defmacro with-fore-color (color &body body)
  `(progn
     ,@body))

(defmacro with-focused-view (view &body body)
  `(progn
     ,@body))

(defmethod dialog-item-text ((view easygui:static-text-view))
  (easygui:view-text view))

(defmethod view-font ((view easygui:static-text-view))
  (easygui:view-font view))

(defmethod draw-view-rectangle ((view focus-ring))
  (frame-oval view (make-point 0 0) (view-size view)))
