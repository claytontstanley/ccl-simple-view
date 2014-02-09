; Bootstrap all needed packages (loads ACT-R, Cocoa framework, etc.)
(load (format nil "~a~a" (directory-namestring *load-truename*) "bootstrap.lisp"))

(make-instance
  'window
  :view-position (make-point 10 60)
  :view-subviews
  (list (make-instance
          'view
          :view-nick-name :view
          :view-position (make-point 10 10)
          :view-size (make-point 50 50)
          :view-subviews
          (list (make-instance
                  'view
                  :view-nick-name :nested-view
                  :view-position (make-point 20 20)
                  :view-size (make-point 30 30))))))

(check (equalp (list 40 60)
               (as-list (local-to-global (view-named :nested-view (view-named :view (front-window))) (make-point 10 30)))))
(check (equalp (list 20 20) (as-list (local-to-global (view-named :view (front-window)) (make-point 10 10)))))
(check (equalp (list 20 70) (as-list (local-to-global (front-window) (make-point 10 10)))))

(window-close (front-window))

(defun test-view-global-position ()
  (make-instance 'window
                 :view-position (make-point 10 50)
                 :view-subviews
                 (list (make-instance
                         'view
                         :view-size (make-point 5 50)
                         :view-nick-name :view
                         :view-position (make-point 20 20)
                         :view-subviews
                         (list (make-instance
                                 'view
                                 :view-size (make-point 1 1)
                                 :view-nick-name :nested-view
                                 :view-position (make-point 10 10))))))
  (check (equalp (list 10 50) (as-list (view-global-position (front-window)))))
  (check (equalp (list 20 20) (as-list (view-global-position (view-named :view (front-window))))))
  (check (equalp (list 30 30)
                 (as-list (view-global-position
                            (view-named :nested-view (view-named :view (front-window)))))))
  (window-close (front-window)))

(test-view-global-position)

