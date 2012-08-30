; Bootstrap all needed packages (loads ACT-R, Cocoa framework, etc.)
(load (format nil "~a~a" (directory-namestring *load-truename*) "bootstrap.lisp"))

(setf *image-path*
      (format nil "~a~a/~a" (directory-namestring *load-truename*) "data" "voteboxbg.aif"))

(add-resource (create-resource 'image *image-path*) "voteboxbg")

(setf *win*
      (make-instance 'window))

(add-subviews *win* 
              (make-instance 'icon-dialog-item
                             :icon "voteboxbg"
                             :view-size (make-point 20 20)
                             :view-nick-name :image))

(inspect *win*)

(view-window (view-named :image *win*))

(defmethod view-click-event-handler ((view icon-dialog-item) location)
  (beep))

#|
(make-instance 'image-view)
(#/sizeToFit (cocoa-ref (view-named :image *win*)))
(set-view-size (view-named :image *win*) (make-point 30 30))
(view-size (view-named :image *win*))
(#/size (#/image (cocoa-ref (view-named :image *win*))))
(open-resource-folder (choose-directory-dialog))

|#
