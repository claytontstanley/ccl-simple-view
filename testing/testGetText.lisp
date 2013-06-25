; Bootstrap all needed packages (loads ACT-R, Cocoa framework, etc.)
(load (format nil "~a~a" (directory-namestring *load-truename*) "bootstrap.lisp"))

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

(check (string= *str* "a"))



