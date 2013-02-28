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

; Class definitions for ns-text-view base cocoa class

(defclass easygui::cocoa-text-view (easygui::cocoa-extension-mixin ns:ns-text-view)
  ()
  (:metaclass ns:+ns-object))

(defclass easygui::view-text-via-text-view-string-mixin ()
  ())

(defmethod easygui::view-text ((view easygui::view-text-via-text-view-string-mixin))
  (objc:lisp-string-from-nsstring (#/string (cocoa-ref view))))

(defmethod (setf easygui::view-text) (new-text (view easygui::view-text-via-text-view-string-mixin))
  (#/setString: (cocoa-ref view) (objc:make-nsstring new-text)))

(defclass text-view (dialog-item easygui::view-text-via-text-view-string-mixin)
  ((text-truncation :initform nil))
  (:default-initargs :specifically 'easygui::cocoa-text-view))

; Relay keypresses to the window, to match behavior for ns-text-field base cocoa class 
; (see mcl-migration/easygui/extensions.lisp)

(objc:defmethod (#/keyUp: :void) ((cocoa-self easygui::cocoa-text-view) the-event)
  (call-next-method the-event)
  (#/keyDown: (#/window cocoa-self) the-event))

(provide :chil-ccl-utilities)
