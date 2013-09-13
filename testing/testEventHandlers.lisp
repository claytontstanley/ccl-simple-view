; Bootstrap all needed packages (loads ACT-R, Cocoa framework, etc.)
(load (format nil "~a~a" (directory-namestring *load-truename*) "bootstrap.lisp"))

(load-as-lst ".." "bincarbon" "direct-event-calls.lisp")

(defclass foo (window)
  ())

(defparameter *chain* ())
(defparameter *keychain* ())

(defmethod view-key-event-handler ((view foo) key)
  (push-to-end 'window *chain*)
  (push-to-end key *keychain*))

(defmethod view-key-event-handler ((view editable-text-dialog-item) key)
  (push-to-end 'view *chain*))

(progn
  (make-instance
    'foo
    :view-subviews
    (list
      (make-instance 'editable-text-dialog-item
                     :view-size (make-point 100 50)
                     :view-nick-name :et)))
  (setf *chain* nil)
  (setf *keychain* nil)
  (sleep .1)
  (left-mouse-click (view-center (front-window)))
  (left-mouse-click (add-points
                      (view-center (view-named :et (front-window)))
                      (view-position (front-window))))
  (let ((str "*^(&(#AKab"))
    (loop for key across str 
          do (keypress key))
    (print *chain*)
    (loop for char from 1 upto (length str) 
          do (loop for event in (list 'view 'window)
                   do (check (eq (pop *chain*) event))))
    (loop for key-pressed across str
          for key-registered in *keychain*
          do (check (eq key-pressed key-registered)))))


(progn
  (setf *keychain* nil)
  (setf *chain* nil)
  (make-instance 'foo)
  (sleep .1)
  (left-mouse-click (view-center (front-window)))
  (keypress "b")
  (check (eq (pop *keychain*) #\b))
  (check (equal *chain* (list 'window)))
  )

