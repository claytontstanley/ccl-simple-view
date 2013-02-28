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

(defclass easygui::cocoa-password-entry-text-field (easygui::cocoa-text-field)
  ()
  (:metaclass ns:+ns-object))

(defclass easygui::cocoa-password-entry-text-field-cell (ns:ns-text-field-cell)
  ()
  (:metaclass ns:+ns-object))

(#/setCellClass: easygui::cocoa-password-entry-text-field easygui::cocoa-password-entry-text-field-cell)

(defclass easygui::cocoa-password-formatter (ns:ns-formatter)
  ()
  (:metaclass ns:+ns-object))

(defclass easygui::cocoa-password-entry-text-view (ns:ns-text-view)
  ()
  (:metaclass ns:+ns-object))

(defclass easygui::cocoa-password-entry-layout-manager (ns:ns-layout-manager)
  ()
  (:metaclass ns:+ns-object))

(#/formatter (#/cell (cocoa-ref (view-named :pw *win*))))

(objc:defmethod #/stringForObjectValue: ((self easygui::cocoa-password-formatter) an-object)
  (format t "#/stringForObjectValue: ~a ~a~%" self an-object)
  an-object)

(objc:defmethod #/editingStringForObjectValue: ((self easygui::cocoa-password-formatter) an-object)
  (format t "#/editingStringForObjectValue: ~a ~a~%" self an-object)
  an-object)


(objc:defmethod (#/getObjectValue:forString:errorDescription: #>BOOL) ((self easygui::cocoa-password-formatter) an-object forString errorDescription)
  (format t "#/getObjectValue:forString:errorDescription: ~a ~a~%" forString an-object)
  (let ((obj (#/initWithString: (#/alloc ns:ns-mutable-string)
              forString)))
  t))

(defclass password-entry-dialog-item (view)
  ((pending-event :accessor pending-event :initform nil)
   (dialog-item-hidden-text :accessor dialog-item-hidden-text :initform ""))
  (:default-initargs :specifically 'easygui::cocoa-password-entry-text-view))

(#/layoutManager (#/textContainer (cocoa-ref (view-named :pw *win*))))

(objc:defmethod (#/drawGlyphsForGlyphRange:atPoint: :void) ((self easygui::cocoa-password-entry-layout-manager) (glyph-range #>NSRange) (at-point #>NSRange))
  (format t "#/drawGlyphsForGlyphRange:atPoint: ~a ~a~%" glyph-range at-point)
  (print (#/numberOfGlyphs self))
  (print
    (loop for i from 0 to (1- (#/numberOfGlyphs self))
          collecting (#/glyphAtIndex: self i)))
  (dotimes (i (#/numberOfGlyphs self))
    (#/replaceGlyphAtIndex:withGlyph: self
     i
     13)
  )
  (call-next-method glyph-range at-point))

(#/glyphWithName: ns:ns-font (objc:make-nsstring "bullet"))


(defmethod initialize-instance :after ((view password-entry-dialog-item) &key)
  (#/replaceLayoutManager: (#/textContainer (cocoa-ref view))
   (#/init (#/alloc easygui::cocoa-password-entry-layout-manager)))
  (#/setFont: (cocoa-ref view)
   (convert-font "Courier" 12))
  ())
  (#/setFormatter: (#/cell (cocoa-ref view))
   (#/init (#/alloc easygui::cocoa-password-formatter))))

(objc:defmethod (#/drawRect: :void) ((self easygui::cocoa-password-entry-text-field) (rect :<NSR>ect))
  (call-next-method rect))

(objc:defmethod (#/drawWithFrame:inView: :void) ((self easygui::cocoa-password-entry-text-field-cell) (frame :<NSR>ect) cocoa-view)
  (format t "#/drawWithFrame:inView: on ~a, ~a~%" self frame)
  (call-next-method frame cocoa-view)
 )

(objc:defmethod (#/drawInteriorWithFrame:inView: :void) ((self easygui::cocoa-password-entry-text-field-cell) (frame :<NSR>ect) cocoa-view)
  (format t "#/drawInteriorWithFrame:inView: on ~a, ~a~%" self frame)
  (let* ((str-val (#/attributedStringValue self))
         (new-val (#/initWithAttributedString: (#/alloc ns:ns-mutable-attributed-string)
                   str-val)))
    (#/replaceCharactersInRange:withString: new-val
     (ns:make-ns-range 0 2)
     (objc:make-nsstring "**"))
    (#/drawInRect: new-val (#/titleRectForBounds: self frame))
    ) 
  (#/updateCell: cocoa-view self)
  (#/updateCellInside: cocoa-view self)
  (#/flushGraphics (#/currentContext ns:ns-graphics-context))
 )
;(#/flushWindow (#/window cocoa-view)))

(objc:defmethod (#/keyUp: :void) ((cocoa-self easygui::cocoa-password-entry-text-field) the-event)
  (call-next-method the-event)
  (let ((view (easygui::easygui-view-of cocoa-self)))
    (schedule-for-event-process 
      (lambda ()
        (#/drawInteriorWithFrame:inView: (#/cell cocoa-self)
         (#/frame cocoa-self)
         cocoa-self)
        ;(#/updateCell: cocoa-self (#/cell cocoa-self))
        ;(#/updateCellInside: cocoa-self (#/cell cocoa-self))
        ;(#/flushGraphics (#/currentContext ns:ns-graphics-context))
        )
      1)))

(defun get-rightmost-char-pos (char str)
  (search (string char) str :from-end t :test #'char=))

(defmethod hide-nth-character ((view view) n)
  (let ((text (dialog-item-text view)))
    (when (and (> (length text) n)
               (>= n 0))
      (set-dialog-item-text
        view
        (concatenate
          'string
          (subseq text 0 n)
          "*"
          (subseq text (1+ n) (length text)))))))

(defmethod hide-last-character ((view view))
  (hide-nth-character view (1- (length (dialog-item-text view)))))

(defmethod hide-next-to-last-character ((view view))
  (hide-nth-character view (1- (1- (length (dialog-item-text view))))))

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
