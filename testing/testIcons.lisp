; Bootstrap all needed packages (loads ACT-R, Cocoa framework, etc.)
(load (format nil "~a~a" (directory-namestring *load-truename*) "bootstrap.lisp"))

(setf *win*
      (make-instance 'window
                     :view-subviews
                     (list (make-instance 'icon-dialog-item
                                          :icon 501
                                          :view-nick-name :image))))

(inspect *win*)

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
