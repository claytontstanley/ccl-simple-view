(defparameter *ret* nil)

(defmacro return-from-modal-dialog (form)
  `(setf *ret* (cons :return (multiple-value-list ,form))))

(defmethod modal-dialog ((dialog window) &optional (close-on-exit t))
  (let ((*modal-dialog-on-top* t))
    (while (null *ret*) 
           (format t "waiting for user to finish with dialog~%")
           (sleep 1))
    (unwind-protect (apply #'values (cdr *ret*))
      (when close-on-exit
        (window-close dialog))
      (setf *ret* nil))))

(defmethod find-subview-of-type ((view easygui::view) subview-type)
  (dolist (sub (view-subviews view) nil)
    (when (typep sub subview-type)
      (return sub))))
