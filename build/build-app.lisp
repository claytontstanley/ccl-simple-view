(require :sv-utilities)

(defparameter *app-name* nil)

(with-continue
  (with-shadow (save-application
                 (lambda (name &rest rest)
                   (declare (ignore rest))
                   (setf *app-name* name)))
    (require :cocoa-application)))

(load (format nil "~a~a" (directory-namestring *load-truename*) "../testing/bootstrap.lisp"))

(save-application *app-name*)
