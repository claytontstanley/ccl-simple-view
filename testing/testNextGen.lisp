;#+:clozure (with-continue  (setf (macro-function 'rlet) *ccl-rlet*))

; Bootstrap all needed packages (loads ACT-R, Cocoa framework, etc.)
(load (format nil "~a~a" (directory-namestring *load-truename*) "bootstrap.lisp"))

(load-file-list "file-lists" "NextGen" "testNextGen.txt")

(simulate-xplane 1 1000)

#|
;(view-draw-contents *win*)
(view-draw-contents *speed*)
(easygui::set-needs-display *speed* t)
(air-spd (view-window *speed*))

(#/currentContext ns:ns-graphics-context)
(#/flushWindow (cocoa-ref *win*))
(setf (air-spd (view-window *speed*)) 10)

(easygui:invalidate-view *win*)
(make-instance 'simple-view
               :view-size (make-point 100 100)
               :view-position (make-point 200 200))
(setf *v* *)
(ns:ns-rect-x (#/bounds *v*))
*v*
(#/bounds (cocoa-ref *v*))
(origin *v*)

(set-origin *v* 10 10)

(setf *win* (view-window (first *svs*)))
internal-time-units-per-second
(read-line)
(get-xplane "call")
(open-stream)
(simulate-xplane 1 200)

(inspect *win*)

(invalidate-view *win*)

(easygui::running-on-main-thread ()
  (print 5)
  (easygui::invalidate-view *win*)
  (print 4))

(all-processes)


(easygui::running-on-main-thread () (invalidate-view *win*))

(dcc (#/flushWindow (cocoa-ref *win*)))

(setf *cc* (first (subviews *win*)))

(dcc (#/flushGraphics (#/currentContext ns:ns-graphics-context)))

(make-instance 'window)

(#/flushWindow (cocoa-ref *))

(with-focused-view *cc*
  (dcc (#/flushWindow (#/window (cocoa-ref *cc*)))))


(invalidate-view *win*)
(#/flushWindow (cocoa-ref *win*))

(setf easygui::*suppress-window-flushing* t)

(running-on-main-thread ()
  (mapc #'invalidate-view 
        (print 
          (subseq (subviews *win2*) 0 2))))

(mapcar #'#/bounds
        (mapcar #'cocoa-ref (subviews *win2*)))

             (inspect *win2*) 
              
(setf *rv*
      (make-instance 'rectangle-view
                             :view-position #@(600 600)
                             :view-size #@(200 200)))

(defmethod view-draw-contents ((view rectangle-view))
  (with-focused-view view
    (with-fore-color *red-color*
      (paint-rect view #@(0 0) (view-size view)))))

(remove-subviews *win2* *sky*)
(remove-subviews *win2* *fr*)
(remove-subviews *win2* *fr2*)
(remove-subviews *win2* *rv*)

(setf *fr2* (make-instance 'focus-ring
                           :view-size #@(20 20)
                           :view-position #@(10 500)))

(setf *sky* (second (subviews *win2*)))
(setf *fr* (second (subviews *win2*)))

(add-subviews *win2* *fr*)
(add-subviews *win2* *fr2*)
(add-subviews *win2* *sky*)
(add-subviews *win2* *rv*)

(setf *svs* (subviews *win*))

(setf *temp* (list 
               (first *svs*)
               (second *svs*)))

(setf *win2* (make-instance 'window
                            :view-size (view-size *win*)
                            :view-subviews *temp*))

(apply #'remove-subviews *win* *temp*)

(mapc #'invalidate-view
      (list 
        (third *svs*)
        (fourth *svs*)
        ))

(mapcar #'#/frame (list (cocoa-ref *win*)))
(set-origin (third (subviews *win*)) 0 0)
(origin (second (subviews *win*)))

(setf *gray* (third (subviews *win*)))

(defclass rectangle-view (simple-view) ())


(invalidate-view *win2*)

(mapc #'invalidate-view (subviews *win2*))

(setf *win2*
      (make-instance 
        'window
        :view-subviews
        (list
          (make-instance
            'rectangle-view
            :view-size (make-point 20 20)
            :view-position (make-point 0 0))
          (make-instance
            'rectangle-view
            :view-size (make-point 20 20)
            :view-position (make-point 10 10)))))
            
            






(remove-subviews *win* *gray*)

(mapcar #'view-position (subviews *win*))
(mapcar #'view-size (subviews *win*))

(mapcar #'origin
        (flatten 
          (mapcar #'subviews
                  (subviews *win*))))

(set-view-position (third (subviews *win*)) 0 201)
(set-view-size (third (subviews *win*)) 1300 294) 

(invalidate-view *win*)

(setf (air-spd (view-window *speed*)) 30)
(dolist (view (subviews *win*))
  (print view) 
  (sleep .11)
  (invalidate-view view))

(subviews *win*)

(invalidate-view 
  (fifth (subviews *win*)))

(#/isFlushWindowDisabled (cocoa-ref *win*))
(#/flushWindow (cocoa-ref *win*))
(#/display (cocoa-ref *win*))

(updatevc)

(get-stream)

(print 5)

(with-fore-color *red-color*
                 (format *speed* "hello"))

(view-draw-contents *speed*)

(mapc (lambda (x) (view-named :speed x)) (subviews *win*))

|#

