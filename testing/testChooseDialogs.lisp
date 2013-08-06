; Bootstrap all needed packages (loads ACT-R, Cocoa framework, etc.)
(load (format nil "~a~a" (directory-namestring *load-truename*) "bootstrap.lisp"))

(defun test-dialog (fun)
  (process-run-function "foo"
                        (lambda ()
                          (sleep 1)
                          (let ((ns-panel (#/modalWindow (#/sharedApplication ns:ns-application))))
                            (#/abortModal (#/sharedApplication ns:ns-application))
                            )))
  (check
    (search "error code -1001"
            (easygui::running-on-main-thread ()
              (handler-case (funcall fun) 
                (error (condition)
                       (apply #'format nil
                              (simple-condition-format-control condition)
                              (simple-condition-format-arguments condition))))))))

(test-dialog #'choose-file-dialog)
(test-dialog #'choose-new-file-dialog)
(test-dialog #'choose-directory-dialog)
