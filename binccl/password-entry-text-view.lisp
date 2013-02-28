;;;  -*- mode: LISP; Package: CL-USER; Syntax: COMMON-LISP;  Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Author      : Clayton Stanley 
;;; Copyright   : (c)2003-7 CMU/Rice U./Mike Byrne, All Rights Reserved
;;; Availability: public domain
;;; Address     : Rice University
;;;             : Psychology Department
;;;             : Houston,TX 77251-1892
;;;             : byrne@acm.org
;;;             : clayton.stanley@rice.edu 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Filename    : password-entry-text-view.lisp
;;; Version     : 1.0 
;;; 
;;; Description : Mimics iOS password entry field for OS X
;;;
;;;		  OS X's NSSecureTextField implementation does not display the last
;;;		  character for a set amount of time before hiding it. All characters
;;;		  are hidden immediately after typing. So this class extends a standard
;;;		  dialog-item (using NSTextView for the cocoa class) and hides
;;;		  the characters after a set amount of time, like iOS. 
;;;
;;; Design     :  NSTextField is unsuitable for the cocoa class because it does not
;;;               separate the drawing of the text from the actual text stored in the 
;;;               class. NSTextView does however, (see drawGlyphsForGlyphRange:atPoint:),
;;;               so NSTextView is used as the cocoa class.
;;;
;;;               Because this class inherits dialog-item for the common-lisp class,
;;;               methods like dialog-item-text will work.
;;;
;;;               All keypresses in the view will be relayed to the window, so that the 
;;;               view-key-event-handler methods are called on the window when a key is 
;;;               is either manually or programmatically entered into the view.
;;;
;;;               The view has two main entry points: either manually typing into it, or
;;;               programmatically entering/removing characters. For programmatic entry,
;;;               reference the keypress-in-view methods.
;;;
;;;               Calling the dialog-item-text method will return the text stored in the
;;;               view (no hidden characters). Calling dialog-item-hidden-text will 
;;;               return the text as displayed (hidden characters included) at the time
;;;               of the call.
;;;
;;; Bugs        : ?
;;; 
;;; Todo        : [] 
;;;             : 
;;; 
;;; ----- History -----
;;; 2013.02.28 cts 
;;;             : Creation

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :chil-ccl-utilities))

(defclass easygui::cocoa-password-entry-text-view (easygui::cocoa-text-view)
  ((pending-fun :accessor pending-fun :initform nil)
   (visible-char-time-secs :reader visible-char-time-secs :initform 1))
  (:metaclass ns:+ns-object))

(defclass easygui::cocoa-password-entry-layout-manager (ns:ns-layout-manager)
  ((last-char-vis-p :accessor last-char-vis-p :initform nil))
  (:metaclass ns:+ns-object))

(defclass password-entry-text-view (text-view)
  ()
  (:default-initargs :specifically 'easygui::cocoa-password-entry-text-view))

(objc:defmethod #/initWithFrame: ((self easygui::cocoa-password-entry-text-view) (frame #>NSRect))
  (unwind-protect (call-next-method frame)
    (#/replaceLayoutManager: (#/textContainer self)
     (#/init (#/alloc easygui::cocoa-password-entry-layout-manager)))
    (#/setFont: self
     (convert-font "Courier" 12))))

(objc:defmethod (#/drawGlyphsForGlyphRange:atPoint: :void) ((self easygui::cocoa-password-entry-layout-manager) (glyph-range #>NSRange) (at-point #>NSPoint))
  (let ((glyph-cnt (#/numberOfGlyphs self)))
    (let ((hide-until (if (last-char-vis-p self) (1- glyph-cnt) glyph-cnt)))
      (dotimes (i hide-until)
        (#/replaceGlyphAtIndex:withGlyph: self i 13))))
  (call-next-method glyph-range at-point))

(defmethod dialog-item-hidden-text ((view password-entry-text-view))
  (let ((text (dialog-item-text view)))
    (let ((layout-manager (#/layoutManager (cocoa-ref view))))
      (with-output-to-string (strm)
        (dotimes (i (1- (length text)))
          (format strm "*"))
        (format strm "~a" (if (last-char-vis-p layout-manager)
                            (char text (1- (length text)))
                            "*"))))))

(defmethod cursor-at-end-of-text-p ((cocoa-self easygui::cocoa-password-entry-text-view))
  (awhen (#/selectedRanges cocoa-self)
    (when (eq (#/count it) 1)
      (awhen (#/rangeValue (#/objectAtIndex: it 0))
        (let ((pos (ns:ns-range-location it)))
          (let ((length (ns:ns-range-length it)))
            (when (eq length 0)
              (when (eq pos (#/length (#/string cocoa-self)))
                t))))))))

(objc:defmethod (#/keyDown: :void) ((cocoa-self easygui::cocoa-password-entry-text-view) the-event)
  (call-next-method the-event)
  (labels ((get-keypress (the-event)
             (let* ((chars (#/characters the-event))
                    (str (objc:lisp-string-from-nsstring chars))
                    (char (char str 0)))
               char)))
    (handle-keypress-on-view
      (easygui::easygui-view-of cocoa-self)
      (get-keypress the-event))))

(defmethod handle-keypress-on-view ((view password-entry-text-view) keypress)
  (let ((cocoa-self (cocoa-ref view)))
    (cond ((or (eq keypress #\rubout)
               (not (cursor-at-end-of-text-p cocoa-self)))
           (setf (last-char-vis-p (#/layoutManager cocoa-self)) nil))
          (t
           (setf (last-char-vis-p (#/layoutManager cocoa-self)) t)
           (setf (pending-fun cocoa-self)
                 (alambda ()
                   (when (eq #'self (pending-fun cocoa-self))
                     (setf (last-char-vis-p (#/layoutManager cocoa-self)) nil)
                     (#/setNeedsDisplay: cocoa-self #$YES))))
           (schedule-for-event-process
             (pending-fun cocoa-self)
             (visible-char-time-secs cocoa-self))))))

; Interface for programmatically adding/deleting text

(defmethod keypress-on-view :around ((view password-entry-text-view) key)
  (declare (ignore key))
  (easygui::running-on-main-thread ()
    (call-next-method)))

(defmethod keypress-on-view :before ((view password-entry-text-view) key)
  (handle-keypress-on-view view key))

(defmethod keypress-on-view ((view password-entry-text-view) key)
  (format view "~a" key))

(defmethod stream-write-string ((view password-entry-text-view) string &optional start end)
  (#/insertText: (cocoa-ref view) (objc:make-nsstring (subseq string (aif start it 0) (aif end it (length string))))))

(defmethod keypress-on-view ((view password-entry-text-view) (key (eql #\rubout)))
  (let* ((range (#/selectedRange (cocoa-ref view)))
         (pos (ns:ns-range-location range))
         (length (ns:ns-range-length range)))
    (when (eq length 0)
      (when (> pos 0)
        (#/setSelectedRange: (cocoa-ref view) (ns:make-ns-range (1- pos) (1+ length))))))
  (#/delete: (cocoa-ref view) ccl:+null-ptr+))

(defmethod backspace-on-view ((view password-entry-text-view))
  (keypress-on-view view #\rubout))

#|
(setf *win*
      (make-instance
        'window
        :view-subviews
        (list
          (make-instance
            'password-entry-text-view
            :view-size (make-point 100 30)
            :view-nick-name :pw
            :text "hello, world"
            )
          (make-instance
           'editable-text-dialog-item
           :view-size (make-point 100 30)
           :view-position (make-point 0 100)))))
(dotimes (i 10)
  (keypress-on-view (view-named :pw *win*) #\rubout))
(dotimes (i 10)
  (keypress-on-view (view-named :pw *win*) "ab")
  (sleep .1))
(dialog-item-text (view-named :pw *win*))
(progn
  (keypress-on-view (view-named :pw *win*) "abc")
  (dialog-item-hidden-text (view-named :pw *win*)))
|#
