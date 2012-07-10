; Bootstrap all needed packages (loads ACT-R, Cocoa framework, etc.)
#-:ccl-simple-view (load (format nil "~a~a" (directory-namestring *load-truename*) "bootstrap.lisp"))

(break)

(setf *win* (make-instance 'window))

(inspect *win*)

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

(print 5)


(inspect *win*)

(make-instance 'contained-view)

(inspect *)



