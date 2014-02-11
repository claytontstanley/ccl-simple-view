(eval-when (:compile-toplevel :load-toplevel :execute)
  (shadowing-import 'easygui:cocoa-ref)
  (shadowing-import 'easygui:dcc)
  (shadowing-import 'easygui::running-on-main-thread))

(defclass easygui::cocoa-extension-mixin ()
  ((easygui::easygui-view :initarg :eg-view :reader easygui::easygui-view-of)
   (easygui::flipped :initarg :flipped :initform easygui::*screen-flipped*)))

; Providing a keyword argument to allow negative points; used for mouse coordinates
(defun easygui::point (x y &key (allow-negative-p nil))
  (unless allow-negative-p
    (assert (>= x 0))
    (assert (>= y 0)))
  (make-instance 'easygui::eg-point :x x :y y))

; Patching the function to provide a keyword argument that allows for negative mouse coordinates
(defun easygui::view-mouse-position (view &key (allow-negative-position-p nil))
  (let* ((w (cocoa-ref (easygui::easygui-window-of view)))
         (mouselocation (dcc (#/mouseLocationOutsideOfEventStream w)))
         (cview (if (typep view 'window) (easygui::content-view view) view))
         (nspt (dcc (#/convertPoint:fromView: (cocoa-ref cview) mouselocation NIL))))
    ;; TODO: check point is inside bounds, lest negative coordinates
    (easygui:point (ns:ns-point-x nspt)
                   (ns:ns-point-y nspt)
                   :allow-negative-p allow-negative-position-p)))

; I think I found a bug in these two methods in the easygui package, so redefining them here with correct setNeedsDisplay: call
(defmethod (setf easygui::view-position) (point (self easygui::view))
  (running-on-main-thread ()
    (setf (slot-value self 'easygui::position) point)
    (when (slot-value self 'easygui::frame-inited-p)
      (dcc (#/setFrame: (cocoa-ref self) (easygui::view-content-rect self)))
      (dcc (#/setNeedsDisplay: (cocoa-ref self) t)))))

(defmethod (setf easygui::view-size) (point (self easygui::view))
  (running-on-main-thread ()
    (setf (slot-value self 'easygui::size) point)
    (when (slot-value self 'easygui::frame-inited-p)
      (dcc (#/setFrame: (cocoa-ref self) (easygui::view-content-rect self)))
      (dcc (#/setNeedsDisplay: (cocoa-ref self) t)))))

; I wanted to instantiate my own extended contained-view class, but I didn't see an easy way to do this given the current
; easygui code. So adding a contained-view-specifically slot to the mixin class, defaulting it to the contained-view class
; defined in easygui. If you want to instantiate a different class for the contained view, just overwrite this default.
(defclass easygui::content-view-mixin ()
  ((easygui::content-view :initarg :content-view)
   (easygui::objc-content-view-accessor :reader easygui::objc-content-view-accessor :initarg :objc-content-view-accessor :initform #'#/contentView)
   (easygui::flipped :initarg :flipped :initform easygui::*screen-flipped*)
   (easygui::contained-view-specifically :initarg :contained-view-specifically :initform 'easygui::contained-view)))

(defmethod easygui::content-view ((view easygui::content-view-mixin))
  (assert (eql (cocoa-ref (slot-value view 'easygui::content-view))
               (dcc (funcall (easygui::objc-content-view-accessor view) (cocoa-ref view)))))
  (slot-value view 'easygui::content-view))

; Added code to instantiate the contained view class that is stored as a slot on the mixin object
(defmethod easygui::initialize-view :after ((view easygui::content-view-mixin))
  (unless (slot-boundp view 'easygui::content-view)
    (let ((containee (make-instance (slot-value view 'easygui::contained-view-specifically) 
                                    :cocoa-ref (dcc (#/contentView (cocoa-ref view)))
                                    :view-nick-name 'easygui::%CONTENT-OF-CONTENT-VIEW%
                                    :flipped (slot-value view 'easygui::flipped))))
      (setf (slot-value view 'easygui::content-view) containee
            (slot-value containee 'easygui::parent) view))))

; Redefining to use the &body body pairing instead of &rest body, so that Slime auto indenting works properly
(defmacro easygui::running-on-this-thread ((&key (waitp t)) &body body)
  ;; The purpose of this trivial macro is to mark places where it is thought possible that
  ;; it may be preferable to use running-on-main-thread.
  (declare (ignore waitp))
  `(progn ,@body))

; Radio buttons in 10.8 require being enclosed within an NSMatrix.
; If not (default easygui implementation), each one is assigned to the
; same virtual NSMatrix, which means they are all part of the same cluster,
; which means that the common lisp clustering implementation breaks since
; Cocoa is now forcing all radio buttons to be part of the same cluster.
; The fix is to use NSSwitchButton Cocoa functionality for radio buttons,
; but use NSRadioButton images (for selected and deselected) for the implementation.
; Another approach would have been to embed each radio button in its own NSMatrix,
; but this required much more code than the image switching technique, and it messed up the overall
; easygui design, since the cocoa-ref of a radio-button object would have been an NSMatrix,
; which breaks the view-text and (setf view-text) mixin methods, as well as auto sizing the
; view to the text it contains on init, etc. So, I went with the simple image switching hack.

(let ((alternate-radio-button-image)
      (radio-button-image)
      (radio-button))
  (defmethod easygui::initialize-view :after ((self easygui::radio-button-view))
    (labels ((init-images ()
               (setf radio-button (make-instance 'easygui::cocoa-button))
               (#/setButtonType: radio-button #$NSRadioButton)
               (setf radio-button-image (#/image radio-button))
               (setf alternate-radio-button-image (#/alternateImage radio-button))))
      (when (cocoa-ref self)
        (unless radio-button
          (init-images))
        (dcc (#/setButtonType: (cocoa-ref self) #$NSSwitchButton))
        (dcc (#/setImage: (cocoa-ref self) radio-button-image))
        (dcc (#/setAlternateImage: (cocoa-ref self) alternate-radio-button-image))
        (when (slot-value self 'easygui::selected) (easygui::radio-button-select self))
        (setf (slot-value (cocoa-ref self) 'easygui::easygui-view) self)))))
