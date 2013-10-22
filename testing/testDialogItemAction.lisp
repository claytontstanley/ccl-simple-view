; Bootstrap all needed packages (loads ACT-R, Cocoa framework, etc.)
(load (format nil "~a~a" (directory-namestring *load-truename*) "bootstrap.lisp"))

(make-instance 'window
               :view-subviews
               (list
                 (make-instance 'button-dialog-item
                                :view-nick-name :click
                                :dialog-item-action (lambda (obj)
                                                      (remove-subviews (front-window) obj)))))
(left-mouse-click (view-position (front-window)))
(left-mouse-click (add-points
                    (view-position (front-window))
                    (view-center (view-named :click (front-window)))))

(check (null (view-subviews (front-window))))
