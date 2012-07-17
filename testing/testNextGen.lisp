;#+:clozure (with-continue  (setf (macro-function 'rlet) *ccl-rlet*))

; Bootstrap all needed packages (loads ACT-R, Cocoa framework, etc.)
#-:ccl-simple-view (load (format nil "~a~a" (directory-namestring *load-truename*) "bootstrap.lisp"))

(load-file-list "file-lists" "NextGen" "testNextGen.txt")




#|
(make-instance 'simple-view
               :view-size (make-point 100 100)
               :view-position (make-point 200 200))
(setf *v* *)
(ns:ns-rect-x (#/bounds *v*))
*v*
(#/bounds (cocoa-ref *v*))
(origin *v*)
(set-origin *v* 10 10)

(open-stream)
(simulate-xplane 1 20)

(updatevc)

(get-stream)

(print 5)

(with-fore-color *red-color*
                 (format *speed* "hello"))

(view-draw-contents *speed*)

(mapc (lambda (x) (view-named :speed x)) (subviews *win*))

|#

