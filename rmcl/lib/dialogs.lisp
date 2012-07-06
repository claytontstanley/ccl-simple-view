(defparameter *ret* nil)

(defmacro return-from-modal-dialog (form)
  `(setf *ret* (cons :return (multiple-value-list ,form))))

(defmethod modal-dialog ((dialog window) &optional (close-on-exit t))
  (setf *modal-dialog-on-top* t)
  (process-wait
    "waiting for user to finish with dialog"
    (lambda ()
         ;(format t "waiting for user to finish with dialog~%")
         *ret*))
  (unwind-protect (apply #'values (cdr *ret*))
    (when close-on-exit
      (window-close dialog))
    (setf *modal-dialog-on-top* nil)
    (setf *ret* nil)))

(defmethod find-subview-of-type ((view easygui::view) subview-type)
  (dolist (sub (view-subviews view) nil)
    (when (typep sub subview-type)
      (return sub))))
