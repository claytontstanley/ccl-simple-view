; Bootstrap all needed packages (loads ACT-R, Cocoa framework, etc.)
(load (format nil "~a~a" (directory-namestring *load-truename*) "bootstrap.lisp"))

(load-as-lst "votebox" "Virtual Cursor" "osx-virtual-cursor.lisp")
(load-as-lst "bincarbon" "misc-lib.lisp")

(load-as-lst "bincarbon" "pict-svm.lisp")

(load-as-lst "votebox" "Emulator Files" "VBEmulator.lisp")


;(defclass votebox-window (window)
;  ((pict-id :accessor pict-id :initarg :pict-id)))

;(setf *pool* (init-pool))
#|
(inspect wind)

(make-instance 'check-box-dialog-item)

(add-subviews wind *)

(add-subviews wind (image-view wind))
(remove-subviews wind (image-view wind))

(objc:defmethod (#/myCustomSubviewComparator :<NSC>omparison<R>esult) $a

  )

(require :cocoa)

:<nsv>iew

(objc:define-objc-method ((:<NSC>omparison<R>esult 
                            :myCustomView (:ns:ns-view view1)
                            :aboveSiblingView (:ns:ns-view view2)
                            :comparator (:void context))
                          :<NSV>IEW)
                         #$NSOrderedAscending
                         
                         )

(objc:defmethod (#/myCustomView:aboveSiblingView:context: ) ((self :) (view1 :id) (view2 :id) (context :void))
  #$NSOrderedAscending)

(objc:defmethod (#/fooOnView:aboveSibling:context: :<NSC>omparison<R>esult) ((self ns:ns-view) (view1 :id) (view2 :id) j
  #$NSOrderedAscending)

                          (objc:defmethod (#/drawRect: :void) ((self demo-view) (rect :<NSR>ect))




;(make-instance 'votebox-window)
;(setf *t* *)
;(view-position *t*)

;(inspect *t*)
|#
