

(make-instance 'window
               :view-subviews
               (list
                 (make-instance 'button-dialog-item
                                :view-nick-name :click
                                :dialog-item-action (lambda (obj)
                                                      (remove-subviews (front-window) obj)))))
(sleep .5)
(left-mouse-click (view-position (front-window)))
(left-mouse-click (add-points
                    (view-position (front-window))
                    (view-center (view-named :click (front-window)))))

(check (null (view-subviews (front-window))))
