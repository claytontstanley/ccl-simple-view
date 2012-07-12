(setf *win* (make-instance 
              'window
              :view-subviews
              (list
                (make-instance 'button-dialog-item
                               :view-position (make-point 10 10)
                               :action (lambda () (modal-dialog (make-instance
                                                                  'window
                                                                  :view-position (make-point 10 10)
                                                                  :view-subviews
                                                                  (list
                                                                    (make-instance 'button-dialog-item
                                                                                   :action (lambda () (beep)))))))))))

(return-from-modal-dialog 5)

(modal-dialog *win*) 

(print 5)

