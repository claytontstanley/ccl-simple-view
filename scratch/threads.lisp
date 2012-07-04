
#|
(setf *foo*
      (make-instance 'easygui::cocoa-drawing-overlay-view))
(#/fooBar *foo*)
;(easygui::running-on-main-thread (:waitp nil)
(objc:defmethod #/fooBar ((self easygui::cocoa-drawing-overlay-view))
  (process-run-function "foo"
                        (lambda ()
                          (get-string-from-user "help")))
  ccl:+null-ptr+)
|#


