; Bootstrap all needed packages (loads ACT-R, Cocoa framework, etc.)
(load (format nil "~a~a" (directory-namestring *load-truename*) "bootstrap.lisp"))

(defclass foo (window)
  ())

(defclass my-etdi (editable-text-dialog-item)
  ())

(defparameter *chain* ())
(defparameter *keychain* ())

(defmethod view-key-event-handler ((view foo) key)
  (push-to-end 'window *chain*)
  (push-to-end key *keychain*))

(defmethod view-key-event-handler ((view my-etdi) key)
  (push-to-end 'view *chain*))

(progn
  (make-instance
    'foo
    :view-subviews
    (list
      (make-instance 'my-etdi
                     :view-size (make-point 100 50)
                     :view-nick-name :et)))
  (event-dispatch)
  (setf *chain* nil)
  (setf *keychain* nil)
  (left-mouse-click (view-center (front-window)))
  (left-mouse-click (add-points
                      (view-center (view-named :et (front-window)))
                      (view-position (front-window))))
  (let ((str "*^(&(#AKab"))
    (loop for key across str 
          do (keypress key))
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
  (event-dispatch)
  (left-mouse-click (view-center (front-window)))
  (keypress "b")
  (check (eq (pop *keychain*) #\b))
  (check (equal *chain* (list 'window)))
  )

(defclass bar (window) ())
(defclass m-etdi (editable-text-dialog-item) ())
(defclass m-bdi (button-dialog-item) ())
(defclass m-cdi (check-box-dialog-item) ())
(defmethod view-key-event-handler ((view bar) key)
  (push-to-end :bar *chain*)) 
(defmethod view-key-event-handler ((view m-bdi) key)
  (push-to-end :bdi *chain*))
(defmethod view-key-event-handler ((view m-cdi) key)
  (push-to-end :cdi *chain*)) 
(defmethod view-key-event-handler ((view m-etdi) key)
  (push-to-end :etdi *chain*))

(progn
  (make-instance
    'bar
    :view-subviews
    (list (make-instance 'm-etdi
                         :view-position (make-point 0 0)
                         :view-nick-name :etdi
                         :view-size (make-point 20 40))
          (make-instance 'm-bdi
                         :view-nick-name :bdi
                         :dialog-item-action (lambda (obj) (push-to-end :bdi-action *chain*))
                         :view-position (make-point 20 50))
          (make-instance 'm-cdi
                         :view-nick-name :cdi
                         :dialog-item-action (lambda (obj) (push-to-end :cdi-action *chain*))
                         :view-position (make-point 40 100))))
  (event-dispatch)
  (left-mouse-click (view-position (front-window)))
  (left-mouse-click (add-points
                      (view-position (front-window))
                      (view-center (view-named :etdi (front-window)))))
  (setf *chain* nil)
  (let ((nicks (list :etdi :bdi :cdi :etdi :cdi :bdi)))
    (loop for nick in nicks
          for count from 1
          for state = (if (<= count 3) #\tab #\^Y)
          for view = (view-named nick (front-window))
          for cocoa-ref = (if (eq nick :etdi) (cocoa-text-view view) (cocoa-ref view))
          for responder = (#/firstResponder (cocoa-ref (front-window)))
          do (check (equal cocoa-ref responder))
          do (keypress state)
          finally (check (equal (mapcan #'list nicks (make-list (length nicks) :initial-element :bar))
                                *chain*))))
  (setf *chain* nil)
  (let ((nicks (list :etdi :bdi :cdi)))
    (loop for nick in nicks
          do (keypress #\space)
          do (keypress #\tab)
          finally (check (equal (list :etdi :bar :etdi :bar :bdi :bar :bdi-action :bdi :bar :cdi :bar :cdi-action :cdi :bar)
                                *chain*)))))

(progn
  (make-instance 'window
                 :view-subviews (list (make-instance 'editable-text-dialog-item
                                                     :view-nick-name :etdi
                                                     :view-size (make-point 100 20))))
  (event-dispatch)
  (check (equal (#/firstResponder (cocoa-ref (front-window))) (cocoa-ref (front-window))))
  (left-mouse-click (view-center (front-window)))
  (check (equal (#/firstResponder (cocoa-ref (front-window))) (cocoa-ref (front-window))))
  (keypress #\tab)
  (check (not (equal (#/firstResponder (cocoa-ref (front-window))) (cocoa-ref (front-window)))))
  (check (equal (#/firstResponder (cocoa-ref (front-window))) (cocoa-text-view (view-named :etdi (front-window)))))
  (keypress #\tab)
  (check (equal (#/firstResponder (cocoa-ref (front-window))) (cocoa-text-view (view-named :etdi (front-window))))))

(progn
  (make-instance 'window
                 :view-subviews (list (make-instance 'editable-text-dialog-item
                                                     :view-nick-name :etdi
                                                     :allow-tabs t
                                                     :view-size (make-point 100 20))
                                      (make-instance 'editable-text-dialog-item
                                                     :view-size (make-point 100 20)
                                                     :view-position (make-point 0 30))))
  (event-dispatch)
  (left-mouse-click (view-center (front-window)))
  (keypress #\tab)
  (keypress #\a)
  (keypress #\tab)
  (check (string-equal (dialog-item-text (view-named :etdi (front-window)))
                       (format nil "a~a" #\tab)))
  (check (equal (#/firstResponder (cocoa-ref (front-window)))
                (cocoa-text-view (view-named :etdi (front-window))))))



#|
(inspect *)
(inspect (front-window))
(#/initialFirstResponder (cocoa-ref (front-window)))
(#/firstResponder (cocoa-ref (front-window)))
(make-instance 'window :view-subviews (list (make-instance 'my-etdi :view-size (make-point 100 30))))
|#
