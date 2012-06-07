(print (list-all-packages))


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
