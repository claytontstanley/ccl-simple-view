(defparameter *cpath* (directory-namestring *load-truename*))

(load (format nil "~a~a" *cpath* "../bincarbon/sv-language-layer.lisp"))
(load (format nil "~a~a" *cpath* "../bincarbon/lol-subset.lisp"))
(load (format nil "~a~a" *cpath* "../bincarbon/sv-utilities.lisp"))
(load (format nil "~a~a" *cpath* "../bincarbon/defsystem-patched.lisp"))

(defparameter *app-name* nil)

(with-continue
  (with-shadow (save-application
                 (lambda (name &rest rest)
                   (declare (ignore rest))
                   (setf *app-name* name)))
    (require :cocoa-application)))

(save-application *app-name*)
