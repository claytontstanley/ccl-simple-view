(require :sv-utilities)

(defparameter *cpath* (directory-namestring *load-truename*))

(load (format nil "~a~a" *cpath* "../bincarbon/sv-language-layer.lisp"))
(load (format nil "~a~a" *cpath* "../bincarbon/lol-subset.lisp"))
(load (format nil "~a~a" *cpath* "../bincarbon/sv-utilities.lisp"))

(defparameter *app-name* nil)

(with-continue
  (with-shadow (save-application
                 (lambda (name &rest rest)
                   (declare (ignore rest))
                   (setf *app-name* name)))
    (require :cocoa-application)))

(load (format nil "~a~a" *cpath* "../testing/bootstrap.lisp"))

(save-application *app-name*)
