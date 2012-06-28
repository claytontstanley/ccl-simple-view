(defmacro with-continue (&body body)
  `(handler-bind ((error #'continue))
     ,@body))

(with-continue
  (defconstant #$tejustleft :left)
  (defconstant $tejustleft :left)
  (defconstant #$tejustcenter :center)
  (defconstant $tejustcenter :center)
  (defconstant #$tejustright :right)
  (defconstant $tejustright :right))
