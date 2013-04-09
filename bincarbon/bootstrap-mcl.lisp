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

(in-package "CCL")

(defclass editable-text-dialog-item (fred-dialog-item)
  ((dialog-item-enabled-p :initarg :enabled-p)))

(defclass basic-editable-text-dialog-item (key-handler-mixin dialog-item)
  ((width-correction :allocation :class :initform 4)
   (text-justification :allocation :class :initform 0)
   (draw-outline :initarg :draw-outline :initform t)   ;; not used today
   ;(line-height :initform nil)   ;; not used
   ;(font-ascent :initform nil)
   ))

(provide :bootstrap-mcl)