; Bootstrap all needed packages (loads ACT-R, Cocoa framework, etc.)
(load (format nil "~a~a" (directory-namestring *load-truename*) "bootstrap.lisp"))

(setf *win* (make-instance 'window))

(let ((win (make-instance 'windoid)))
  (sleep 1)
  (window-close win))

(#/level (cocoa-ref *win*))

(move-to *win* 0 0)

(with-focused-view *win*
  (with-fore-color (get-fore-color *win*)
    (line-to *win* 10 10)))

(set-view-size (content-view *win*) (view-size *win*)) 

(add-subviews *win* (make-instance 'static-text-dialog-item
                                   :text "hello"
                                   :view-position (make-point 50 50)))


(add-subviews *win* (make-instance 'static-text-dialog-item
                                   :text "hello"
                                   :view-nick-name :remove
                                   :view-position (make-point 0 0)))

(remove-subviews *win* (view-named :remove *win*))
(apply #'remove-subviews *win* (subviews *win*))
(print 5)

(window-close *win*)

(setf *win* (make-instance 'window :view-position (make-point -30 50)))
(check (equalp (as-list (view-position *win*))
              (list -30 50)))
(window-close *win*)

