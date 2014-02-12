; Bootstrap all needed packages (loads ACT-R, Cocoa framework, etc.)
(load (format nil "~a~a" (directory-namestring *load-truename*) "bootstrap.lisp"))

(defclass my-scroll-bar-content-view (view)
  ())

(defclass my-scroll-bar-dialog-item (scroll-bar-dialog-item) ()) 

(defmethod initialize-instance :after ((view my-scroll-bar-dialog-item) &key)
  (#/setHasVerticalScroller: (cocoa-ref view) #$YES))

(defmethod initialize-instance :after ((view my-scroll-bar-content-view) &key)
  (add-subviews
    view
    (make-instance
      'static-text-dialog-item
      :view-position (make-point 0 0)
      :view-size (make-point 50 50)
      :dialog-item-text "foo")
    (make-instance
      'static-text-dialog-item
      :view-position (make-point 0 50)
      :view-size (make-point 50 50)
      :dialog-item-text "bar")))

(defun new-proc-display ()
  (clear-all)
  (define-model foo ())
  (install-device (front-window))
  (proc-display))

(defun test-vis-locs-for-scroll-bar-dialog-item ()
  (make-instance
    'rpm-real-window
    :view-subviews
    (list (make-instance
            'my-scroll-bar-dialog-item
            :view-nick-name :sbdi
            :view-size (make-point 50 50)
            :view-position (make-point 0 0)
            :scrollee (make-instance 'my-scroll-bar-content-view :view-size #@(50 100)))))
  (new-proc-display)
  (check (equalp (list "foo") 
                 (loop for chunk in (visicon-chunks (get-module :vision) t)
                       collect (chunk-real-visual-value chunk))))
  (device-move-cursor-to (front-window) (p2vpt (view-global-center (view-named :sbdi (front-window)))))
  (scroll-mouse-down 500)
  (sleep 1)
  (new-proc-display)
  (check (equalp (list "bar")
                 (loop for chunk in (visicon-chunks (get-module :vision) t)
                       collect (chunk-real-visual-value chunk)))))

(test-vis-locs-for-scroll-bar-dialog-item)


