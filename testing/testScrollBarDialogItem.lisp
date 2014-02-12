; Bootstrap all needed packages (loads ACT-R, Cocoa framework, etc.)
(load (format nil "~a~a" (directory-namestring *load-truename*) "bootstrap.lisp"))

(make-instance
  'window
  :view-subviews (list (make-instance
                         'scroll-bar-dialog-item
                         :view-size (make-point 20 20)
                         :view-nick-name :sbdi
                         :scrollee (make-instance 'view :view-size (make-point 20 100)))))

(check (#/hasVerticalScroller (cocoa-ref (view-named :sbdi (front-window)))))
(check (v-scroll-p (view-named :sbdi (front-window))))
