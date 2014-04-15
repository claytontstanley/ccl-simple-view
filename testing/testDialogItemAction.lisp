; Bootstrap all needed packages (loads ACT-R, Cocoa framework, etc.)
(load (format nil "~a~a" (directory-namestring *load-truename*) "bootstrap.lisp"))

(make-instance 'window
               :view-subviews
               (list
                 (make-instance 'button-dialog-item
                                :view-nick-name :click
                                :dialog-item-action (lambda (obj)
                                                      (remove-subviews (front-window) obj)))
                 (make-instance 'view
                                :view-position (make-point 40 40)
                                :view-size (make-point 80 80)
                                :view-nick-name :view
                                :view-subviews
                                (list (make-instance
                                        'button-dialog-item
                                        :view-nick-name :click-nested
                                        :dialog-item-action (lambda (obj)
                                                              (remove-subviews (front-window) (view-named :view (view-window obj)))))))))

(left-mouse-click (view-position (front-window)))
(left-mouse-click (add-points
                    (view-position (front-window))
                    (view-center (view-named :click (front-window)))))
(left-mouse-click (add-points
                    (add-points
                      (view-position (front-window))
                      (view-position (view-named :view (front-window))))
                    (view-center (view-named :click-nested (view-named :view (front-window))))))

(event-dispatch)
(check (null (view-subviews (front-window))))
