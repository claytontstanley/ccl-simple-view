; Bootstrap all needed packages (loads ACT-R, Cocoa framework, etc.)
(load (format nil "~a~a" (directory-namestring *load-truename*) "bootstrap.lisp"))

(defun delayed-click-dialog ()
  (let ((win (front-window)))
    (process-run-function
      "foo"
      (lambda ()
        (while (eq win (front-window))
          (sleep .1))
        (left-mouse-click (view-position (front-window)))
        (let ((button
                (first
                  (remove-if-not
                    (lambda (view)
                      (inherit-from-p view 'button-dialog-item))
                    (view-subviews (front-window))))))
          (left-mouse-click (local-to-global (front-window) (view-center button))))))))

(defun test-message-dialog-custom-lambda ()
  (let ((did-fire-lambda nil))
    (delayed-click-dialog)
    (message-dialog
      "so"
      :position (list :left 0)
      :on-ok-click (lambda (item)
                     (declare (ignore item))
                     (setf did-fire-lambda t)
                     (return-from-modal-dialog t)))
    (check did-fire-lambda)))

(defun test-message-dialog ()
  (delayed-click-dialog)
  (message-dialog "so" :position (list :left 0)))

(dotimes (i 10)
  (test-message-dialog)
  (test-message-dialog-custom-lambda))

