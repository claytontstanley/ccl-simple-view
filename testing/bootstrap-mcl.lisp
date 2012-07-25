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

(set-dispatch-macro-character 
  #\# #\/
  (defun |#/-reader| (stream char arg)
    nil))

(defclass editable-text-dialog-item (fred-dialog-item)
  ((dialog-item-enabled-p :initarg :enabled-p)))

