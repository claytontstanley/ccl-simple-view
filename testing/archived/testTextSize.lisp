(setf *win*
      (make-instance 'window
                     :view-size #@(1024 768)
                     :view-subviews
                     (list
                       (make-dialog-item
                         'static-text-dialog-item
                         #@(26 448)
                         #@(177 49)
                         (format nil "STEP 3~%Review your choices")
                         'NIL
                         :PART-COLOR-LIST  '(:BODY 2463733)
                         :view-nick-name :tv
                         :VIEW-FONT
                         '("Lucida Grande" 18 :SRCOR :PLAIN (:COLOR-INDEX 0)))
                       (make-dialog-item
                         'static-text-dialog-item
                         #@(26 100) 
                         #@(900 49)
                         "The quick brown fox jumped over the lazy dog. The quick brown fox jumped over the lazy dog"
                         'nil
                         :PART-COLOR-LIST  '(:BODY 2463733)
                         :view-nick-name :tv2
                         :VIEW-FONT
                         '("Lucida Grande" 18 :SRCOR :PLAIN (:COLOR-INDEX 0))))))

#|
(view-named :tv *win*)

(#/attributedStringValue (cocoa-ref *))

(setf *as* *)

(#/attribute:atIndex:effectiveRange: 
 *as* 
 #$NSParagraphStyleAttributeName
 0 
 ccl:+null-ptr+ )

(setf *mps* (#/mutableCopy *))

(#/setTighteningFactorForTruncation:
 *mps* 
 .05)

(#/setLineBreakMode:
 *mps*
 #$NSLineBreakByTruncatingTail)
;#$NSLineBreakByCharWrapping)
*mas*
*mps*
*as*
(#/length *as*)
(setf *mas* (#/mutableCopy *as*))
*mas*
(#/addAttribute:value:range:
 *mas*
 #$NSParagraphStyleAttributeName
 *mps*
 (ns:make-ns-range 0 (#/length *mas*)))


(#/cell (cocoa-ref (view-named :tv *win*)))

(#/setLineBreakMode: (#/cell (cocoa-ref (view-named :tv *win*)))
 #$NSLineBreakByTruncatingTail)



(#/setAttributedStringValue: (cocoa-ref (view-named :tv *win*))
 *mas*)

(invalidate-view (view-named :tv *win*))

#$NSParagraphStyleAttributeName

(#/tighteningFactorForTruncation (#/defaultParagraphStyle ns:ns-paragraph-style))
(#/defaultParagraphStyle ns:ns-paragraph-style)

(#/setTighteningFactorForTruncation: 
 ns:ns-paragraph-style
 .5)

(setf *tv* (view-named :tv *win*))

(inspect *tv*)
|#
