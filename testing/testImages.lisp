; Bootstrap all needed packages (loads ACT-R, Cocoa framework, etc.)
#-:ccl-gui (load (format nil "~a~a" (directory-namestring *load-truename*) "bootstrap.lisp"))

(setf *pool* (init-pool))

(defclass test-image (image-view-mixin view)
  ())

(defparameter *win* 
  (make-instance 'window
                 :view-size (make-point 1024 768)
                 :view-position (make-point 100 100)))

(defparameter *view*
  (make-instance 'test-image
                 :view-size (make-point 1024 768)
                 :view-position (make-point 0 0)))

(defparameter *image-path*
  (format nil "~a~a/~a" (directory-namestring *load-truename*) "data" "voteboxbg.tiff"))

(current-directory)
(defparameter *image* (create-resource 'image *image-path*))

(assert (not (slot-boundp *image* 'val)))


(add-resource *image* "image")

(assert (not (slot-boundp *image* 'val)))

(print-pool *pool*)
(let ((res (get-resource-val "image")))
  (assert (eq res (val *image*))))

(assert (slot-boundp *image* 'val))

(print-pool *pool*)
(print *pool*)

(add-subviews *win* *view*)

(sleep 1)

(setf (pict-id *view*) "image")



