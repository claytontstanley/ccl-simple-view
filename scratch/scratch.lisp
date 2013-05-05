(print (list-all-packages))
(load #P"NebulaSL:Users:claytonstanley:src:mcl-migration:testing:testVotebox.lisp")

(dotimes (i 100)
  (let ((win (make-instance 'window)))
    ;(sleep (/ (random 1000) 1000))
    ;(window-close win)
    ))
(while (front-window)
  (window-close (front-window)))
(all-processes)    
(random 100)
(dotimes (i 100)
  (let ((win (make-instance 'window :view-position #@(10 10))))
    (window-close win)))

(print $tejustleft)
(print #$tejustleft)
(defconstant tejustleft :left)
(require "PARSE-FFI")
(ccl::parse-standard-ffi-files "tmp")
(ccl::load-os-constant 'tejustleft)

;(setf ccl:*compile-code-coverage* nil)
;(print *features*)

;(visible-virtuals-available?)
;(start-environment)
;(stop-environment)
;(setf easygui:*screen-flipped* t)

#|
(load-os-constant 'e)
(print #$TEJUSTLEFT)
(ccl::%LOAD-VAR 'X86-DARWIN64::TEJUSTLEFT NIL)
(maphash #'print-hash-entry (ccl::fvs))
(gethash "NSKernAttributeName" *t*)
(gethash "tejustleft" *t*)
(print #$NSLinkAttributeName)
(ccl::extract-db-type 0 ccl::*target-ftd*)
(print (svref ccl::*signed-integer-types* 64))
(ccl::%cons-foreign-variable "tejustleft" 'integer)
(setf *fv* *)
(print ccl::*foreign-type-classes*)
(maphash #'print-hash-entry ccl::*foreign-type-classes*)
;(ccl::%cons-foreign-variable "tejustleft" (gethash 'INTEGER ccl::*foreign-type-classes*)))
(setf (gethash "tejustleft" (ccl::fvs))
      (ccl::%cons-foreign-variable "tejustleft" (svref ccl::*signed-integer-types* 64) ))
(ccl::resolve-foreign-variable "tejustleft")
(defun print-hash-entry (key value)
  (format t "The value associated with the key ~S is ~S~%" key value))
(alexandria:maphash-keyskccl::fvs)
(defconstant os::TEJUSTLEFT 0)
(print os::TEJUSTLEFT)
|#





(open-shared-library "/Users/claytonstanley/src/mcl-migration/actr6/devices/ccl/libtypetest.dylib")
(inspect ccl::*shared-libraries*)
(#_void_void_test :void)
(external-call "_void_void_test" :void)
(external-call "_sc_sc_test" -128)
(inspect #_?void_void_test)
(#_void_void_test)

(require :cocoa)
(require :easygui)
(load "src/mcl-migration/ccl-examples/cocoa/easygui/example/view-hierarchy.lisp")
(load "src/mcl-migration/ccl-examples/cocoa/easygui/example/tiny.lisp")
(load "src/mcl-migration/ccl-examples/cocoa/easygui/example/currency-converter.lisp")

(make-instance 'easygui-user::view-hierarchy-demo-window)
(make-instance 'easygui-demo::tiny-demo-window)
(make-instance 'easygui-demo::converter-window)

(current-director)

(load "~/src/actr6/load-act-r-6.lisp")

(inspect 'add-visual-items-to-rpm-window)

(make-instance 'rpm-real-window)

(make-instance 'tiny-demo-drawing-view)

(use-package :easygui-demo)

*package*

(inspect 'print)
(in-package :easygui)

(LET ((VIEW
        (MAKE-INSTANCE
          'WINDOW
          :TITLE
          "MCL Generated Clozure UI"
          :POSITION
          (EASYGUI::POINT 446 86)
          :SIZE
          (EASYGUI::POINT 379 200))))
     (ADD-SUBVIEWS VIEW
                   (MAKE-INSTANCE
                     'EASYGUI::STATIC-TEXT-VIEW
                     :POSITION
                     (EASYGUI::POINT 22 154)
                     :SIZE
                     (EASYGUI::POINT 91 16)
                     :TEXT
                     "Static Text")
                   (MAKE-INSTANCE
                     'EASYGUI::CHECK-BOX-VIEW
                     :POSITION
                     (EASYGUI::POINT 23 11)
                     :SIZE
                     (EASYGUI::POINT 70 16)
                     :TITLE
                     "Like It!")
                   (MAKE-INSTANCE
                     'EASYGUI::RADIO-BUTTON-VIEW
                     :POSITION
                     (EASYGUI::POINT 22 86)
                     :SIZE
                     (EASYGUI::POINT 70 16)
                     :TITLE
                     "MCL"
                     :SELECTED
                     T)
                   (MAKE-INSTANCE
                     'EASYGUI::RADIO-BUTTON-VIEW
                     :POSITION
                     (EASYGUI::POINT 23 59)
                     :SIZE
                     (EASYGUI::POINT 70 16)
                     :TITLE
                     "Clozure")
                   (LET ((VIEW
                           (MAKE-INSTANCE
                             'EASYGUI::TEXT-INPUT-VIEW
                             :POSITION
                             (EASYGUI::POINT 145 55)
                             :SIZE
                             (EASYGUI::POINT 218 125)
                             :TEXT
                             "Design the dialog using MCL's 
                              Interface Toolkit, then generate
                              Cocoa user interface code for 
                              Clozure. ")))
                              NIL
                              VIEW)
                           (MAKE-INSTANCE
                             'EASYGUI::PUSH-BUTTON-VIEW
                             :POSITION
                             (EASYGUI::POINT 289 12)
                             :SIZE
                             (EASYGUI::POINT 72 20)
                             :TEXT
                             "Go!")
                           (MAKE-INSTANCE
                             'POP-UP-MENU
                             :POSITION
                             (EASYGUI::POINT 16 121)
                             :SIZE
                             (EASYGUI::POINT 91 20)
                             :TITLE
                             ""
                             :MENU-ITEMS
                             (LIST (MAKE-INSTANCE
                                     'EASYGUI::MENU-ITEM-VIEW
                                     :TITLE
                                     "Pop Up"))))
          VIEW)
