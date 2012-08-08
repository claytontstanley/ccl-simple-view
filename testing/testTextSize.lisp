(setf *win*
      (make-instance 'window
                     :view-size #@(1024 768)
                     :view-subviews
                     (list
                       (make-dialog-item
                         'static-text-dialog-item
                         #@(26 448)
                         #@(177 49)
                         "STEP 3
Review your choices"
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
                         

(setf *tv* (view-named :tv *win*))

(inspect *tv*)

(#/font (cocoa-ref *tv*))
