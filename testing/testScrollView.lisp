; Bootstrap all needed packages (loads ACT-R, Cocoa framework, etc.)
(load (format nil "~a~a" (directory-namestring *load-truename*) "bootstrap.lisp"))

(defun test-content-view-mixin ()
  (make-instance
    'window
    :view-subviews
    (list (make-instance
            'editable-text-dialog-item
            :view-nick-name :etdi
            :view-subviews (list (make-instance 'view :view-size (make-point 2 2)))
            :view-size (make-point 40 40))))
  (check (not (null (view-subviews (view-named :etdi (front-window))))))
  (window-close (front-window)))

(test-content-view-mixin)

