
(defclass foo-button-dialog-item (button-dialog-item) ())

(defmethod build-vis-locs-for :around ((self foo-button-dialog-item) (vis-mod vision-module))
  (let ((feats (call-next-method)))
    (list
      nil
      (list 
        (first feats)))))

(defun run-experiment ()
  (let ((window (make-instance 'rpm-real-window)))
    (reset)
    (install-device window)
    (add-visual-items-to-rpm-window
      window
      (make-instance 'foo-button-dialog-item))
    (proc-display)
    (print-visicon)))

(clear-all)

(define-model foo)

#|
(run-experiment)
|#
