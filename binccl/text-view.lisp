(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :ccl-simple-view))

; Class definitions for ns-text-view base cocoa class

(defclass easygui::cocoa-text-view (easygui::cocoa-extension-mixin ns:ns-text-view)
  ()
  (:metaclass ns:+ns-object))

(defclass easygui::view-text-via-text-view-string-mixin ()
  ())

(defmethod easygui::view-text ((view easygui::view-text-via-text-view-string-mixin))
  (objc:lisp-string-from-nsstring (#/string (cocoa-ref view))))

(defmethod (setf easygui::view-text) (new-text (view easygui::view-text-via-text-view-string-mixin))
  (#/setString: (cocoa-ref view) (objc:make-nsstring new-text))
  new-text)

(defclass text-view (dialog-item easygui::view-text-via-text-view-string-mixin)
  ((text-truncation :initform nil))
  (:default-initargs :specifically 'easygui::cocoa-text-view))

; Relay keypresses to the window, to match behavior for ns-text-field base cocoa class 
; (see mcl-migration/easygui/extensions.lisp)

(objc:defmethod (#/keyUp: :void) ((cocoa-self easygui::cocoa-text-view) the-event)
  (call-next-method the-event)
  (#/keyDown: (#/window cocoa-self) the-event))

(defmethod cursor-at-end-of-text-p ((view text-view))
  (let ((cocoa-self (cocoa-ref view)))
    (awhen (#/selectedRanges cocoa-self)
      (when (eq (#/count it) 1)
        (awhen (#/rangeValue (#/objectAtIndex: it 0))
          (let ((pos (ns:ns-range-location it)))
            (let ((length (ns:ns-range-length it)))
              (when (eq length 0)
                (when (eq pos (#/length (#/string cocoa-self)))
                  t)))))))))

; Interface for programmatically adding/deleting text

(defmethod keypress-on-view :around ((view text-view) key)
  (declare (ignore key))
  (easygui::running-on-main-thread ()
    (call-next-method)))

(defmethod keypress-on-view :before ((view text-view) key)
  (handle-keypress-on-view view key))

(defmethod keypress-on-view :after ((view text-view) key)
  (easygui::view-key-event-handler (view-window view) key))

(defmethod keypress-on-view ((view text-view) key)
  (format view "~a" key))

(defmethod stream-write-string ((view text-view) string &optional start end)
  (#/insertText: (cocoa-ref view) (objc:make-nsstring (subseq string (aif start it 0) (aif end it (length string))))))

(defmethod keypress-on-view ((view text-view) (key (eql #\rubout)))
  (let* ((range (#/selectedRange (cocoa-ref view)))
         (pos (ns:ns-range-location range))
         (length (ns:ns-range-length range)))
    (when (eq length 0)
      (when (> pos 0)
        (#/setSelectedRange: (cocoa-ref view) (ns:make-ns-range (1- pos) (1+ length))))))
  (#/delete: (cocoa-ref view) ccl:+null-ptr+))

(defmethod backspace-on-view ((view text-view))
  (keypress-on-view view #\rubout))

(provide :text-view)
