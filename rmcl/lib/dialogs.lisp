(defparameter *ret* nil)

(defmacro return-from-modal-dialog (form)
  `(progn
     (setf *ret* (cons :return (multiple-value-list ,form)))
     (#/abortModal (#/sharedApplication ns:ns-application))))

(defmethod modal-dialog ((dialog window) &optional (close-on-exit t))
  (setf *modal-dialog-on-top* dialog)
  (#/runModalForWindow: (#/sharedApplication ns:ns-application) (cocoa-ref dialog))
  (unwind-protect (apply #'values (cdr *ret*))
    (when close-on-exit
      (window-close dialog))
    (setf *modal-dialog-on-top* nil)
    (setf *ret* nil)))

(defmethod find-subview-of-type ((view easygui::view) subview-type)
  (dolist (sub (view-subviews view) nil)
    (when (typep sub subview-type)
      (return sub))))
