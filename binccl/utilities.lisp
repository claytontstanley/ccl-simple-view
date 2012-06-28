
(defmacro with-continue (&body body)
  `(handler-bind ((error #'continue))
     ,@body))
