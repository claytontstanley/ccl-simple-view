; Bootstrap all needed packages (loads ACT-R, Cocoa framework, etc.)
(load (format nil "~a~a" (directory-namestring *load-truename*) "bootstrap.lisp"))

(defparameter *win* nil)
(defparameter *t* nil)

(setf *win* 
      (make-instance 
        'window
        :view-subviews
        (list
          (make-instance 
            'button-dialog-item
            :view-position (make-point 10 10)
            :action (lambda () 
                      (setf *t* 
                            (modal-dialog 
                              (make-instance
                                'window
                                :view-position (add-points
                                                 (make-point 10 10)
                                                 (view-position *win*))
                                :view-subviews
                                (list
                                  (make-instance 
                                    'button-dialog-item
                                    :view-position (make-point 10 10)
                                    :action 
                                    (lambda ()
                                      (beep)
                                      (return-from-modal-dialog 5))))))))))))

(while (not *t*)
  (spin-for-fct 100))

(check (eq *t* 5))

#|
*modal-dialog-ret*
*t*
(setf *modal-dialog-ret* nil)
(return-from-modal-dialog 5)
|#
