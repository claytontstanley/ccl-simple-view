; Bootstrap all needed packages (loads ACT-R, Cocoa framework, etc.)
(load (format nil "~a~a" (directory-namestring *load-truename*) "bootstrap.lisp"))

(defun test-dialog (fun &optional title)
  (process-run-function "foo"
                        (lambda ()
                          (sleep 1)
                          (let ((ns-panel (#/modalWindow (#/sharedApplication ns:ns-application))))
                            (when title
                              (check (string-equal title (objc:lisp-string-from-nsstring (#/title ns-panel)))))
                            (#/abortModal (#/sharedApplication ns:ns-application))
                            )))
  (check
    (search "error code -1001"
            (easygui::running-on-main-thread ()
              (handler-case (apply fun (when title (list :prompt title)))
                (error (condition)
                       (apply #'format nil
                              (simple-condition-format-control condition)
                              (simple-condition-format-arguments condition))))))))

(test-dialog #'choose-file-dialog)
(test-dialog #'choose-file-dialog "foo")
(test-dialog #'choose-new-file-dialog)
(test-dialog #'choose-new-file-dialog "bar")
(test-dialog #'choose-directory-dialog)
(test-dialog #'choose-directory-dialog "baz")
