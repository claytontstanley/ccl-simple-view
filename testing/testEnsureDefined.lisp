
(defparameter *defun-orig* (macro-function 'defun))
(defparameter *defmacro-orig* (macro-function 'defmacro))
(defmacro ensure-defined (&body body)
  `(macrolet ((defun (name args &body body)
                (unless (fboundp name)
                  (funcall *defun-orig*
                           `(defun ,name ,args ,@body) nil)))
              (defmacro (name args &body body)
                (unless (macro-function name)
                  (funcall *defmacro-orig*
                           `(defmacro ,name ,args ,@body) nil))))
     (progn
        ,@body)))

(ensure-defined
  (defun foo (a)
    4)
  (defun kdkd (b a)
    3)
  (defmacro lsls ()
    ())
  (defun a (b)
    ())
  (defmacro aaaaa ()
    ()))
