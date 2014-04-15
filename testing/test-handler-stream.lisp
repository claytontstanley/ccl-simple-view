; Bootstrap all needed packages (loads ACT-R, Cocoa framework, etc.)
(load (format nil "~a~a" (directory-namestring *load-truename*) "bootstrap.lisp"))

(defclass my-rpm-win (rpm-real-window)
  ())

(defparameter *so-test* nil)

(defmethod rpm-window-click-event-handler ((win my-rpm-win) (loc t))
  (check (eq ccl::*initial-process* *current-process*))
  (setf *so-test* *standard-output*)
  (beep))

(make-instance 'my-rpm-win)
(running-on-main-thread ()
  (let ((*listener-output* *listener-output*))
    (post-view-click-event-handler (front-window) (make-point 1 1))))
(check (eq *so-test* (get-listener-output-stream)))
(running-on-main-thread ()
  (let ((*listener-output* *standard-output*))
    (post-view-click-event-handler (front-window) (make-point 1 1))))
(check (not (eq *so-test* (get-listener-output-stream))))

(window-close (front-window))

