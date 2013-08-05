; Bootstrap all needed packages (loads ACT-R, Cocoa framework, etc.)
(load (format nil "~a~a" (directory-namestring *load-truename*) "bootstrap.lisp"))

(process-run-function "foo"
                      (lambda ()
                        (sleep 2)
                        (left-mouse-click (view-position (front-window)))
                        (let ((button
                                (first
                                  (remove-if-not
                                    (lambda (view)
                                      (inherit-from-p view 'button-dialog-item))
                                    (view-subviews (front-window))))))
                          (left-mouse-click (local-to-global (front-window) (view-center button))))))
(message-dialog "so" :position (list :left 0))
