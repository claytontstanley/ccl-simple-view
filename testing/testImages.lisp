; Bootstrap all needed packages (loads ACT-R, Cocoa framework, etc.)
(load (format nil "~a~a" (directory-namestring *load-truename*) "bootstrap.lisp"))

(defclass test-image (back-image-view)
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

(let ((res))
  (with-shadow (sv-log (lambda (&rest args)
                         (apply fun-orig args)
                         (destructuring-bind (str &rest args) args
                           (declare (ignore args))
                           (when (search "already present" str)
                             (setf res t)))))
    (add-resource (create-resource 'sound *image-path*) "voteboxbg"))
  (check res))

(check (= (length (get-pool-as-lst)) 3))

(let ((res (get-resource-val "voteboxbg" 'image)))
  (check (eq res (val *image*))))

(check (errors-p
         (get-resource "voteboxbg")))

(check (errors-p
         (get-resource "foobar")))

(check (not (errors-p
              (get-resource "voteboxbg" 'image))))

(check (not (errors-p
              (get-resource "image"))))

(check (slot-boundp *image* 'val))

(capture-output nil (print-pool *resource-pool*))
;(print *resource-pool*)

(add-subviews *win* *view*)

(sleep 1)

(setf (pict-id *view*) "voteboxbg")

(make-instance
  'window
  :view-size (make-point 1044 788)
  :view-subviews
  (list
    (make-instance
      'test-image
      :view-nick-name :tv
      :view-position (make-point 0 10)
      :pict-id "voteboxbg")))

(event-dispatch)
(let ((view (view-named :tv (front-window))))
  (check (equalp (as-list (view-size view))
                 (list 1024 768)))
  (check (equalp (as-list (#/frame (cocoa-ref view)))
                 (list 1024 768 0 10))))


(defclass foo-svm (pict-svm window) ())

(progn
  (make-instance
    'foo-svm
    :view-size (make-point 250 250)
    :pict-id "voteboxbg")
  (event-dispatch)
  (let ((view (get-image-view (front-window))))
    (set-view-pict (front-window) "voteboxbg")
    (check (eq view (get-image-view (front-window))))
    (check (equalp (list 250 250 0 0) (as-list (#/frame (cocoa-ref view))))))
  (sleep .5)
  (window-close (front-window))
  (sleep .5)
  (make-instance 'foo-svm)
  (check (not (get-image-view (front-window))))
  (set-view-pict (front-window) "voteboxbg")
  (sleep .5)
  (let ((view (get-image-view (front-window))))
    (set-view-pict (front-window) "image")
    (check (not (eq view (get-image-view (front-window)))))))

