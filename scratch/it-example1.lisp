(print 5)

(setf *win* 
      (MAKE-INSTANCE
        'COLOR-DIALOG
        :WINDOW-TYPE
        :MOVABLE-DIALOG
        :VIEW-POSITION
        #@(932 94)
        :VIEW-SIZE
        #@(300 150)
        :CLOSE-BOX-P
        NIL
        :VIEW-SUBVIEWS
        (LIST (MAKE-DIALOG-ITEM
                'SEQUENCE-DIALOG-ITEM
                #@(73 46)
                #@(14 48)
                "Untitled"
                'NIL
                :CELL-SIZE
                #@(14 16)
                :SELECTION-TYPE
                :SINGLE
                :TABLE-HSCROLLP
                :UNDETERMINED
                :TABLE-VSCROLLP
                :UNDETERMINED
                :ROWS
                3
                :COLUMNS
                1
                :TRACK-THUMB-P
                T
                :SEPARATOR-COLOR
                8421504
                :SEPARATOR-PATTERN
                '*BLACK-PATTERN*
                :SEPARATOR-SIZE
                #@(0 0)
                :TABLE-SEQUENCE
                '(0 1 2))
              (MAKE-INSTANCE
                'POP-UP-MENU
                :VIEW-POSITION
                #@(159 30)
                :VIEW-SIZE
                #@(122 20)
                :MENU-TITLE
                ""
                :MENU-ITEMS
                (LIST)
                :AUTO-UPDATE-DEFAULT
                T
                :ITEM-DISPLAY
                :SELECTION)
              (MAKE-DIALOG-ITEM
                'STATIC-TEXT-DIALOG-ITEM
                #@(183 83)
                #@(54 16)
                "Untitled"
                'NIL
                :DIALOG-ITEM-ENABLED-P
                NIL)
              (MAKE-DIALOG-ITEM
                'BUTTON-DIALOG-ITEM
                #@(121 117)
                #@(72 20)
                "Untitled"
                'NIL
                :DEFAULT-BUTTON
                NIL))))

(sleep 2)

(window-close *win*)
