
#|
(defparameter *shared-app* (#/sharedApplication ns:ns-application))
*shared-app*
+null-ptr+
(make-instance 'easygui::cocoa-window)
(setf *t* *)
(defparameter *timer* (#/retain 
                       (#/timerWithTimeInterval:target:selector:userInfo:repeats:
                        ns:ns-timer
                        (coerce 1 'double-float)
                        *t*
                        (gui::@selector #/aintenanceForWindows:)
                        ccl:+null-ptr+
                        t)))
(#/fireDate *timer*)
*modal-dialog-ret*
*timer*
(objc:defmethod (#/aintenanceForWindows: :void) ((self easygui::cocoa-window) timer)
                (setf *modal-dialog-ret* 5)
                ())
(#/addTimer:forMode: 
 (#/currentRunLoop ns:ns-run-loop)
 *timer*
 #$NSDefaultRunLoopMode)
(#/run *)
(#/currentMode *)
(#/currentRunLoop ns:ns-run-loop)
(#/mainRunLoop ns:ns-run-loop)
(#/aintenanceForWindows: *t* *timer*)
(all-processes)
|#
