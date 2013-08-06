; Bootstrap all needed packages (loads ACT-R, Cocoa framework, etc.)
(load (format nil "~a~a" (directory-namestring *load-truename*) "bootstrap.lisp"))

(progn
  (process-run-function
    "foo"
    (lambda ()
      (sleep 1)
      (left-mouse-click (view-position (front-window)))
      (let ((cancel
              (first (remove-if-not
                       (lambda (x)
                         (string-equal "cancel" (dialog-item-text x)))
                       (view-subviews (front-window))))))
        (left-mouse-click (local-to-global (front-window) (view-center cancel))))))
  (setf *str* (get-string-from-user
                "prompt"
                :position (list :left 0)))
  (check (eq *str* :cancel)))

(progn
  (process-run-function
    "foo"
    (lambda ()
      (sleep 1)
      (left-mouse-click (view-position (front-window)))
      (sleep .3)
      (keypress #\a)
      (sleep .3)
      (left-mouse-click
        (add-points
          (view-position (front-window))
          (view-center
            (first (remove-if-not
                     (lambda (x)
                       (equalp (class-name (class-of x))
                               'default-button-dialog-item))
                     (subviews (front-window)))))))))
  (setf *str* (get-string-from-user
                "prompt"
                :position (make-point 10 10)
                :initial-string "foo"))
  (check (string= *str* "a")))



