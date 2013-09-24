; Bootstrap all needed packages (loads ACT-R, Cocoa framework, etc.)
(load (format nil "~a~a" (directory-namestring *load-truename*) "bootstrap.lisp"))

(setf *image-path*
      (format nil "~a~a/~a" (directory-namestring *load-truename*) "data" "voteboxbg.tiff"))

(add-resource (create-resource 'image *image-path*) "voteboxbg")

(setf *win*
      (make-instance 'window))

(add-subviews *win* 
              (make-instance 'icon-dialog-item
                             :icon "voteboxbg"
                             :view-size (make-point 20 20)
                             :view-nick-name :image))


(view-window (view-named :image *win*))

(defparameter *clicked* nil)

(defmethod view-click-event-handler ((view icon-dialog-item) location)
  (setf *clicked* t))

(destructuring-bind (x y) (as-list (add-points (view-position *win*) (make-point 10 10)))
  (left-mouse-click (make-point x y))
  (left-mouse-click (make-point x y)))

(check *clicked*)

#|
(make-instance 'image-view)
(set-view-size (view-named :image *win*) (make-point 30 30))
(view-size (view-named :image *win*))
(#/size (#/image (cocoa-ref (view-named :image *win*))))

|#
