(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :ccl-simple-view))

(defun save-snapshot (fname)
  "Saves snapshot of current window as a tiff stored in fname"
  (let ((captured-image
          (#_CGWindowListCreateImage
           #$CGRectNull
           #$kCGWindowListOptionIncludingWindow
           (#/windowNumber (cocoa-ref (get-front-window)))
           #$kCGWindowImageBoundsIgnoreFraming)))
    (let ((ns-image
            (#/initWithCGImage:size: (#/alloc ns:ns-image)
             captured-image
             #$NSZeroSize)))
      (let ((tiff-rep 
              (#/TIFFRepresentation ns-image)))
        (#/writeToFile:atomically: tiff-rep
         (objc:make-nsstring fname)
         #$NO)))))

(defun schedule-for-event-process (f time-in-secs)
  "Fires lambda f on main run loop after time-in-secs seconds"
  (ccl::call-in-event-process
    (lambda ()
      (#/performSelector:withObject:afterDelay:
       gui::*NSApp*
       (objc:@selector #/lispInterrupt:)
       (make-instance 'ns:ns-number :with-int (ccl::assign-id-map-id ccl::*interrupt-id-map* f))
       (coerce time-in-secs 'double-float)))))

(defun print-objc-arglists (message)
  (mapcar (lambda (obj)
            (list obj (ccl::objc-method-info-arglist obj)))
          (ccl::objc-message-info-methods
            (ccl::get-objc-message-info message))))

(provide :chil-ccl-utilities)
