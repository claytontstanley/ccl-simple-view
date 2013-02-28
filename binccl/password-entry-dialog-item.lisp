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
;;; Filename    : password-entry-dialog-item.lisp
;;; Version     : 1.0 
;;; 
;;; Description : Mimicks iOS password entry field for OS X
;;;
;;;		  OS X's NSSecureTextField implementation does not display the last
;;;		  character for a set amount of time before hiding it. All characters
;;;		  are hidden immediately after typing. So the code extends a standard
;;;		  editable-text-dialog-item (text entry field) and hides the characters
;;;		  after a set amount of time, like iOS. 
;;;
;;; Design     :  The text stored in the cocoa object is the hidden text, so a call to
;;;               dialog-item-text on this view will return the text that is displayed
;;;               on the screen (hidden characters will be part of the text).
;;;               If you want the original text, then call the dialog-item-hidden-text
;;;               method on the view.
;;;
;;;               The keyUp: method was used to hook into a keypress (instead of keyDown:)
;;;               because Cocoa does not allow one to override the default keyDown: method
;;;               on ns-text-field (which easygui::cocoa-text-field inherits).
;;;
;;;               Since keyUp: was used, the keypress is already stored in the object when
;;;               keyUp: is called, so draw calls in keyUp: essentially 'clean up' what 
;;;               previous drawing had done by hiding appropriate characters. Although
;;;               this is definitely a bit of a hack, the view looks and responds just fine
;;;               for my testing.
;;;		  
;;; Bugs        : ?
;;; 
;;; Todo        : [] 
;;;             : 
;;; 
;;; ----- History -----
;;; 2013.02.23 cts 
;;;             : Creation

(defclass easygui::cocoa-password-entry-text-view (easygui::cocoa-extension-mixin ns:ns-text-view)
  ((pending-fun :accessor pending-fun :initform nil)
   (visible-char-time-secs :reader visible-char-time-secs :initform 1))
  (:metaclass ns:+ns-object))

(defclass easygui::cocoa-password-entry-layout-manager (ns:ns-layout-manager)
  ((last-char-vis-p :accessor last-char-vis-p :initform nil))
  (:metaclass ns:+ns-object))

(defclass password-entry-dialog-item (view)
  ((pending-event :accessor pending-event :initform nil)
   (dialog-item-hidden-text :accessor dialog-item-hidden-text :initform ""))
  (:default-initargs :specifically 'easygui::cocoa-password-entry-text-view))

(defmethod initialize-instance :after ((view password-entry-dialog-item) &key)
  (#/replaceLayoutManager: (#/textContainer (cocoa-ref view))
   (#/init (#/alloc easygui::cocoa-password-entry-layout-manager)))
  (#/setFont: (cocoa-ref view)
   (convert-font "Courier" 12)))

(objc:defmethod (#/drawGlyphsForGlyphRange:atPoint: :void) ((self easygui::cocoa-password-entry-layout-manager) (glyph-range #>NSRange) (at-point #>NSRange))
  (let ((glyph-cnt (#/numberOfGlyphs self)))
    (let ((hide-until (if (last-char-vis-p self) (1- glyph-cnt) glyph-cnt)))
      (dotimes (i hide-until)
        (#/replaceGlyphAtIndex:withGlyph: self i 13))))
  (call-next-method glyph-range at-point))

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
  (let ((keypress (get-keypress the-event)))
    (cond ((or (eq keypress #\rubout)
               (not (cursor-at-end-of-text-p cocoa-self)))
           (setf (last-char-vis-p (#/layoutManager cocoa-self)) nil)
           (#/setNeedsDisplay: cocoa-self #$YES))
          (t
           (setf (last-char-vis-p (#/layoutManager cocoa-self)) t)
           (setf (pending-fun cocoa-self)
                 (alambda ()
                   (when (eq #'self (pending-fun cocoa-self))
                     (setf (last-char-vis-p (#/layoutManager cocoa-self)) nil)
                     (#/setNeedsDisplay: cocoa-self #$YES))))
           (schedule-for-event-process (pending-fun cocoa-self) (visible-char-time-secs cocoa-self))))))

(defun get-keypress (the-event)
  (let* ((chars (#/characters the-event))
         (str (objc:lisp-string-from-nsstring chars))
         (char (char str 0)))
    char))

(defun schedule-for-event-process (f time-in-secs)
  (ccl::call-in-event-process
    (lambda ()
      (#/performSelector:withObject:afterDelay:
       gui::*NSApp*
       (objc:@selector #/lispInterrupt:)
       (make-instance 'ns:ns-number :with-int (ccl::assign-id-map-id ccl::*interrupt-id-map* f))
       (coerce time-in-secs 'double-float)))))

#|
(setf *win*
      (make-instance
        'window
        :view-subviews
        (list
          (make-instance
            'password-entry-dialog-item
            :view-size (make-point 100 30)
            :view-nick-name :pw
            )
          (make-instance
           'editable-text-dialog-item
           :view-size (make-point 100 30)
           :view-position (make-point 0 100)))))
|#
