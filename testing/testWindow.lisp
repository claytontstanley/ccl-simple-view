; Bootstrap all needed packages (loads ACT-R, Cocoa framework, etc.)
(load (format nil "~a~a" (directory-namestring *load-truename*) "bootstrap.lisp"))

(setf *win* (make-instance 'window))

(let ((win (make-instance 'windoid)))
  (window-close win))

(#/level (cocoa-ref *win*))

(move-to *win* 0 0)

(with-focused-view *win*
  (with-fore-color (get-fore-color *win*)
    (line-to *win* 10 10)))

(set-view-size (content-view *win*) (view-size *win*)) 

(add-subviews *win* (make-instance 'static-text-dialog-item
                                   :dialog-item-text "hello"
                                   :view-position (make-point 50 50)))


(add-subviews *win* (make-instance 'static-text-dialog-item
                                   :dialog-item-text "hello"
                                   :view-nick-name :remove
                                   :view-position (make-point 0 0)))

(remove-subviews *win* (view-named :remove *win*))
(apply #'remove-subviews *win* (subviews *win*))
(print 5)

(add-subviews *win*
              (make-instance 'static-text-dialog-item
                             :dialog-item-text "foo"
                             :view-nick-name :one)
              (make-instance 'static-text-dialog-item
                             :dialog-item-text "bar"
                             :view-nick-name :two))

(check (eq (view-named :one *win*)
           (find-named-sibling (view-named :two *win*) :one)))

(window-close *win*)

(setf *win* (make-instance 'window :view-position (make-point -30 50)))
(check (equalp (as-list (view-position *win*))
               (list -30 50)))
(window-close *win*)

(defun test-window-state ()
  (let ((win
          (make-instance 'window)))
    (check (window-shown-p win))
    (window-hide win)
    (sleep 3)
    (check (not (window-shown-p win)))
    (window-show win)
    (sleep 3)
    (check (window-shown-p win))
    (window-close win)))

(test-window-state)

(defclass foo-window (window)
  ())

(defun test-front-window ()
  (setf *win* (make-instance 'foo-window))
  (setf *win2* (make-instance 'window))
  (setf *win3* (make-instance 'windoid))
  (check (eq *win2* (front-window)))
  (check (eq *win3* (front-window :include-windoids t)))
  (check (eq *win3* (front-window :class 'windoid)))
  (check (eq *win* (front-window :class 'foo-window)))
  (window-close *win*)
  (window-close *win2*)
  (window-close *win3*))

(test-front-window)

(defun test-find-window ()
  (let ((foo-win (make-instance 'foo-window :window-title "3")))
    (check (null (find-window "3" 'window)))
    (let ((win (make-instance 'window :window-title "3")))
      (check (eq win (find-window "3" 'window)))
      (check (eq foo-win (find-window "3" 'foo-window)))
      (check (find-window "3"))
      (window-close win)
      (window-close foo-win))))

(test-find-window)

(defun test-close-box-p ()
  (labels ((is-closable (win)
             (logtest #$NSClosableWindowMask (#/styleMask (cocoa-ref win)))))
    (let ((win (make-instance 'window)))
      (check (is-closable win))
      (window-close win)
      (let ((win (make-instance 'window :close-box-p nil)))
        (check (not (is-closable win))) 
        (window-close win)
        (let ((win (make-instance 'borderless-window)))
          (check (not (is-closable win))) 
          (window-close win))))))

(test-close-box-p)
