; Bootstrap all needed packages (loads ACT-R, Cocoa framework, etc.)
(load (format nil "~a~a" (directory-namestring *load-truename*) "bootstrap.lisp"))

(defclass custom-window (rpm-real-window) ())
(defclass custom-etdi (editable-text-dialog-item) ())

(defmethod initialize-instance :after ((view custom-etdi) &key)
  (#/setHasVerticalScroller: (cocoa-ref view) #$YES))

(defparameter *fired-scroll-handler-p* nil)

(defmethod rpm-window-scroll-event-handler ((win custom-window))
  (setf *fired-scroll-handler-p* t))

(defun test-scroll-handlers ()
  (make-instance 
    'custom-window
    :view-subviews
    (list (make-instance
            'custom-etdi
            :view-nick-name :etdi
            :view-size (make-point 100 100)
            :scrollee (make-instance 'view :view-size (make-point 100 500)))))
  (device-move-cursor-to
    (front-window)
    (p2vpt (local-to-global (view-named :etdi (front-window))
                            (view-center (view-named :etdi (front-window))))))
  (setf *fired-scroll-handler-p* nil)
  (device-handle-scroll-down (front-window) 5)
  (check *fired-scroll-handler-p*)
  (setf *fired-scroll-handler-p* nil)
  (device-handle-scroll-up (front-window) 5)
  (check *fired-scroll-handler-p*)
  (window-close (front-window)))

(test-scroll-handlers)
