(defmacro %ensure-defined (form)
  (destructuring-bind (symb name (&rest arglist) &body body) form
    (declare (ignore arglist body))
    (unless
      (funcall
        (case symb
          (defun #'fboundp)
          (defmacro #'macro-function)
          (defvar #'boundp))
        name)
      form)))

(defmacro ensure-defined (&body body)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     ,@(mapcar (lambda (form)
                 `(%ensure-defined ,form))
               body)))

(defmacro with-continue (&body body)
  `(handler-bind ((error #'continue))
     ,@body))

(ensure-defined
  (defmacro push-to-end (item place)
    "analogous to the push macro; just places 'item' at the end of 'place', instead of the front"
    `(setf ,place (nconc ,place (list ,item)))))

(provide :chil-utilities)
