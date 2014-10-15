
; FIXME: Is there any way not to use a global for this? The problem is that *modal-dialog-ret* is 
; set on a different thread than the modal dialog thread, so the cleanest way that I've
; found to communicate between threads is to use a shared global
(defparameter *modal-dialog-ret* nil)

(defun stop-modal ()
  (#/abortModal (#/sharedApplication ns:ns-application))
  )

; Form could be (values ...), which is why this is a macro. Don't eval form
; until it's wrapped in a multiple-value-list call
(defmacro return-from-modal-dialog (form)
  `(progn
     (sv-log "returning from modal dialog with form ~a" ',form)
     (guard ((null *modal-dialog-ret*)
             "modal dialog system on thread ~a in inconsistent state; about to set val; but it's already set as ~a"
             *current-process*
             *modal-dialog-ret*) ())
     (setf *modal-dialog-ret* (cons :return (multiple-value-list ,form)))
     (stop-modal)
     (pop *modal-dialog-on-top*)
     (values)))

(defmethod modal-dialog ((dialog window) &optional (close-on-exit t))
  (push dialog *modal-dialog-on-top*)
  (guard ((null *modal-dialog-ret*)
          "modal dialog system on thread ~a in inconsistent state; val should be clear at this point, but it's set as ~a" 
          *current-process*
          *modal-dialog-ret*) ())
  (guard ((null *modal-dialog-ret*) "modal dialog system in inconsistent state; aborting"))
  (#/runModalForWindow: (#/sharedApplication ns:ns-application) (cocoa-ref dialog))
  (unwind-protect (apply #'values (cdr *modal-dialog-ret*))
    (when close-on-exit
      (window-close dialog))
    (guard (*modal-dialog-ret* "modal dialog system in inconsistent state; ret should be nil but it's ~a; aborting" *modal-dialog-ret*))
    (setf *modal-dialog-ret* nil)))

(defmethod find-subview-of-type ((view easygui::view) subview-type)
  (dolist (sub (view-subviews view) nil)
    (when (typep sub subview-type)
      (return sub))))
