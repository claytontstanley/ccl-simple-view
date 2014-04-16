; Bootstrap all needed packages (loads ACT-R, Cocoa framework, etc.)
(load (format nil "~a~a" (directory-namestring *load-truename*) "bootstrap.lisp"))

(defun test-floats ()
  (let ((win (make-instance
               'rpm-real-window
               :view-position (make-point 200 200))))
    (add-subviews
      win
      (make-static-text-for-rpm-window
        win :x 10 :y 20 :text "foo")
      (make-button-for-rpm-window
        win :x 10 :y 40 :text "bar")
      (make-line-for-rpm-window
        win (list 10 80) (list 10 120)))
    (clear-all)
    (define-model foo ())
    (install-device win)
    (proc-display)
    (print-visicon)
    (let ((res
            (mapcar (lambda (chunk)
                      (print chunk)
                      (list 
                        (chunk-slot-value-fct chunk 'screen-x) 
                        (chunk-slot-value-fct chunk 'screen-y)))
                    (visicon-chunks (get-module :vision) t))))
      (print res)
      (assert (equal `((10 100) (21 28) (40 52) (40 52))
                     res))
      )))

(test-floats)

