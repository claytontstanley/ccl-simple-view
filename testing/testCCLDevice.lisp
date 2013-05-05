; Bootstrap all needed packages (loads ACT-R, Cocoa framework, etc.)
(load (format nil "~a~a" (directory-namestring *load-truename*) "bootstrap.lisp"))

(defparameter *key* nil)

(defun flash-text (&optional (text "!!!!"))
  (process-run-function
    "flash"
    (lambda ()
      (let ((view (make-static-text-for-rpm-window
                    *win*
                    :x 10 
                    :y 10 
                    :text text
                    :color 'orange)))
        (add-visual-items-to-rpm-window *win* view)
        (sleep .1)
        (remove-visual-items-from-rpm-window *win* view)))))


(defclass test-rpm-real-window (rpm-real-window)
  ())

(defmethod rpm-window-click-event-handler ((device test-rpm-real-window) position)
  (declare (ignore device position))
  (flash-text "mouse down clicked")
  (call-next-method))

(defmethod rpm-window-key-event-handler ((device test-rpm-real-window) key)
  (declare (ignorable device key))
  (setf *key* key)
  (flash-text (format nil "key ~a pressed" key))
  (call-next-method))

(defparameter *win* nil)
(defparameter *view* nil)
(defparameter *win-title* "exp window")

(check (null (open-rpm-window? *win*)))

(setf *win* (make-rpm-window
              :class 'test-rpm-real-window
              :visible t
              :width 400
              :height 400
              :x 20
              :y 600
              :title *win-title*
              ))

(view-mouse-position *win*)

(select-rpm-window *win*)
(check (string-equal
         (rpm-window-title *win*)
         *win-title*))
(check (open-rpm-window? *win*))
(sleep .5)

;:action (lambda (obj) (print "hello"))))
(setf *view* (make-button-for-rpm-window 
               *win*
               :x 10
               :y 100
               :text "bute"
               :action (lambda (obj) (sleep .5))))
(add-visual-items-to-rpm-window *win* *view*)
(sleep .5)

(setf *view* (make-static-text-for-rpm-window
               *win*
               :x 200
               :y 100
               :text "hello, world"
               :color 'orange))
(add-visual-items-to-rpm-window *win* *view*)
(sleep .5)

#|
(cocoa-ref *win*)
(easygui::cocoa-ref-valid-p *win*)
(inspect *win*)
(all-processes)
(wptr *win*)
|#

(setf *view* (make-line-for-rpm-window
               *win*
               (list 100 200)
               (list 200 220)
               'green))
(add-visual-items-to-rpm-window *win* *view*)

(setf *view* (make-instance 'check-box-dialog-item
                            :view-position (make-point 20 20)))
(add-visual-items-to-rpm-window *win* *view*)
(check (> (point-h (view-size *view*))
          0))

(setf *view* (make-line-for-rpm-window
               *win*
               (list 100 220)
               (list 200 200)
               'yellow))
(add-visual-items-to-rpm-window *win* *view*)

(setf *view* (make-line-for-rpm-window
               *win*
               (list 300 200)
               (list 200 220)
               'black))
(add-visual-items-to-rpm-window *win* *view*)

(setf *view* (make-line-for-rpm-window
               *win*
               (list 300 220)
               (list 200 200)
               'blue))
(add-visual-items-to-rpm-window *win* *view*)

(sleep .5)

(setf *view* (make-line-for-rpm-window
               *win*
               (list 200 300)
               (list 300 300)
               'black))
(add-visual-items-to-rpm-window *win* *view*)
(sleep .1)
(remove-visual-items-from-rpm-window *win* *view*)
(sleep .5)

(setf *view* (make-instance 'editable-text-dialog-item
                            :view-position (make-point 10 300)
                            :view-nick-name :et
                            :dialog-item-text "here"))
(add-visual-items-to-rpm-window *win* *view*)

(set-selection-range (view-named :et *win*) 1 2)

(setf *view* (make-instance 'radio-button-dialog-item
                            :view-position (make-point 100 300)
                            :view-nick-name :rb1))
(add-visual-items-to-rpm-window *win* *view*)
(setf *view* (make-instance 'radio-button-dialog-item
                            :view-position (make-point 150 300)
                            :view-nick-name :rb2))
(add-visual-items-to-rpm-window *win* *view*)

#|
#$NSBorderlessWindowMask
(make-instance 'windoid)
(#/setStyleMask: (cocoa-ref *)
 #$NSBorderlessWindowMask)
(#/close (cocoa-ref *))
***
(window-close *)
(cocoa-ref (view-named :et *win*))
(window-close *win*)

(#/isEnabled *)
(#/isSelected *)
(#/selectedRanges
 (#/fieldEditor:forObject: (cocoa-ref *win*) #$YES (cocoa-ref (view-named :et *win*))))

(setf *cocoa-win* (easygui:cocoa-ref *win*))
(compute-class-precedence-list (find-class 'contained-view))
(inspect *win*)
(defun create-left-mouse-click (window)
  "Returns an NSEvent object that describes a left mouse click at the current mouse location"
  (#/mouseEventWithType:location:modifierFlags:timestamp:windowNumber:context:eventNumber:clickCount:pressure: ns:ns-event
   #$NSLeftMouseDown
   (#/mouseLocationOutsideOfEventStream window) ;Mouse location
   0 ;No flags
   (coerce 0 'double-float) ;Occured at time 0.0; is this OK?
   (#/windowNumber window) 
   (#/graphicsContext window)
   0 ;eventNumber 0; is this OK?
   1 ;clickCount
   (float 1)))
(setf *the-event* (create-left-mouse-click *cocoa-win*))
(inspect *win*)
(class-precedence-list (find-class 'easygui::contained-view))

(#/sendEvent: (#/sharedApplication ns:ns-application)
 *the-event*)

;(remove-all-items-from-rpm-window *win*)
(front-window)
(mapcar '#/close (ns-array->list (#/windows (#/sharedApplication ns:ns-application))))
(menubar-show)
;(close-rpm-window *win*)

;(check (null (open-rpm-window? *win*)))

|#
