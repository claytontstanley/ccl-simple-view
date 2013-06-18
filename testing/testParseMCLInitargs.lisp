; Bootstrap all needed packages (loads ACT-R, Cocoa framework, etc.)
(load (format nil "~a~a" (directory-namestring *load-truename*) "bootstrap.lisp"))

(parse-mcl-initargs
  (list :dialog-item-action (lambda (obj) (print obj)) :view 'view)
  (list :view-position (list :top 20) :size (make-point 50 50))
  (list :view-position 1203)
  (list :view-position (make-point 50 50))
  (list :action (lambda () ()) :view 'view)
  (list :text-truncation nil)
  (list :text-truncation :end)
  (list :action nil)
  (list :dialog-item-action nil))
