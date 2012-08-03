; Bootstrap all needed packages (loads ACT-R, Cocoa framework, etc.)
(load (format nil "~a~a" (directory-namestring *load-truename*) "bootstrap.lisp"))

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

(defparameter *image* (create-resource 'image *image-path*))

(check (not (slot-boundp *image* 'val)))
(add-resource *image* "voteboxbg")
(check (not (slot-boundp *image* 'val)))

(add-resource *image* "image")

(setf *image-path*
      (format nil "~a~a/~a" (directory-namestring *load-truename*) "data" "voteboxbg.aif"))

(add-resource (create-resource 'sound *image-path*) "voteboxbg")

(let ((res (get-resource-val "voteboxbg" 'image)))
  (check (eq res (val *image*))))

(check (errors-p
         (get-resource "voteboxbg")))

(check (not (errors-p
              (get-resource "voteboxbg" 'image))))

(check (not (errors-p
              (get-resource "image"))))

(check (slot-boundp *image* 'val))

(capture-output nil (print-pool *pool*))
;(print *pool*)

(add-subviews *win* *view*)

(sleep 1)

(setf (pict-id *view*) "voteboxbg")



