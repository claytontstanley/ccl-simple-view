(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :ccl-simple-view))

; Class definitions for ns-text-view base cocoa class

(defclass easygui::cocoa-text-view (easygui::cocoa-extension-mixin ns:ns-text-view)
  ()
  (:metaclass ns:+ns-object))

(defclass easygui::cocoa-scroll-view (easygui::cocoa-extension-mixin ns:ns-scroll-view)
  ((cocoa-text-view-specifically :reader cocoa-text-view-specifically :initform 'easygui::cocoa-text-view))
  (:metaclass ns:+ns-object))

(defclass easygui::view-text-via-text-view-string-mixin ()
  ())

(defmethod easygui::view-text ((view easygui::view-text-via-text-view-string-mixin))
  (objc:lisp-string-from-nsstring (#/string (cocoa-text-view view))))

(defmethod (setf easygui::view-text) (new-text (view easygui::view-text-via-text-view-string-mixin))
  (#/setString: (cocoa-text-view view) (objc:make-nsstring new-text))
  new-text)

(defclass text-view (dialog-item easygui::view-text-via-text-view-string-mixin)
  ((text-truncation :initform nil))
  (:default-initargs :specifically 'easygui::cocoa-scroll-view))

(objc:defmethod #/initWithFrame: ((self easygui::cocoa-scroll-view) (frame #>NSRect))
  (unwind-protect (call-next-method frame)
    (#/setBorderType: self #$NSBezelBorder)
    (#/setDocumentView: self
     (cocoa-ref (make-instance 'dialog-item
                               :text-truncation nil
                               :specifically (cocoa-text-view-specifically self)
                               :view-size (make-point
                                            (ns:ns-size-width (#/contentSize self))
                                            (ns:ns-size-height (#/contentSize self))))))))

(defmethod cocoa-text-view ((view text-view))
  (#/documentView (cocoa-ref view)))

(defmethod set-text-justification ((view text-view) justification)
  (#/setAlignment: (cocoa-text-view view)
   (convert-justification justification))
  (setf (text-justification view) justification))

; Relay keypresses to the window, to match behavior for ns-text-field base cocoa class 
; (see mcl-migration/easygui/extensions.lisp)

(objc:defmethod (#/keyUp: :void) ((cocoa-self easygui::cocoa-text-view) the-event)
  (call-next-method the-event)
  (#/keyDown: (#/window cocoa-self) the-event))

(defmethod cursor-at-end-of-text-p ((view text-view))
  (let ((cocoa-text-view (cocoa-text-view view)))
    (awhen (#/selectedRanges cocoa-text-view)
      (when (eq (#/count it) 1)
        (awhen (#/rangeValue (#/objectAtIndex: it 0))
          (let ((pos (ns:ns-range-location it)))
            (let ((length (ns:ns-range-length it)))
              (when (eq length 0)
                (when (eq pos (#/length (#/string cocoa-text-view)))
                  t)))))))))

; Interface for programmatically adding/deleting text

(defmethod keypress-on-view :around ((view text-view) key)
  (declare (ignore key))
  (easygui::running-on-main-thread ()
    (call-next-method)))

(defmethod keypress-on-view ((view text-view) key)
  (format view "~a" key))

(defmethod stream-write-string ((view text-view) string &optional start end)
  (#/insertText: (cocoa-text-view view) (objc:make-nsstring (subseq string (aif start it 0) (aif end it (length string))))))

(defmethod keypress-on-view ((view text-view) (key (eql #\rubout)))
  (let* ((range (#/selectedRange (cocoa-text-view view)))
         (pos (ns:ns-range-location range))
         (length (ns:ns-range-length range)))
    (when (eq length 0)
      (when (> pos 0)
        (#/setSelectedRange: (cocoa-text-view view) (ns:make-ns-range (1- pos) (1+ length))))))
  (#/delete: (cocoa-text-view view) ccl:+null-ptr+))

(defmethod backspace-on-view ((view text-view))
  (keypress-on-view view #\rubout))

(provide :text-view)
