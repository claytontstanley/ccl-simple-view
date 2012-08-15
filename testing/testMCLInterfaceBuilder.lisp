
(MAKE-INSTANCE
  'COLOR-DIALOG
  :WINDOW-TYPE
  :DOCUMENT
  :VIEW-POSITION
  #@(30 60)
  :VIEW-SIZE
  #@(600 600)
  :VIEW-SUBVIEWS
  (LIST (MAKE-INSTANCE
          'POP-UP-MENU
          :VIEW-POSITION
          #@(276 113)
          :VIEW-SIZE
          #@(92 20)
          :MENU-TITLE
          ""
          :MENU-ITEMS
          (LIST (MAKE-INSTANCE
                  'MENU-ITEM
                  :MENU-ITEM-TITLE
                  "second"
                  :STYLE
                  'NIL)
                (MAKE-INSTANCE
                  'MENU-ITEM
                  :MENU-ITEM-TITLE
                  "first"
                  :STYLE
                  'NIL
                  :MENU-ITEM-CHECKED
                  #\CheckMark)
                (MAKE-INSTANCE
                  'MENU-ITEM
                  :MENU-ITEM-TITLE
                  "third"
                  :STYLE
                  'NIL))
          :AUTO-UPDATE-DEFAULT
          T
          :ITEM-DISPLAY
          :SELECTION
          :DEFAULT-ITEM
          2)
        (MAKE-DIALOG-ITEM
          'RADIO-BUTTON-DIALOG-ITEM
          #@(144 118)
          #@(70 16)
          "Untitled"
          'NIL
          :RADIO-BUTTON-PUSHED-P
          T)
        (MAKE-DIALOG-ITEM
          'RADIO-BUTTON-DIALOG-ITEM
          #@(139 150)
          #@(70 16)
          "Untitled"
          'NIL)
        (MAKE-DIALOG-ITEM
          'RADIO-BUTTON-DIALOG-ITEM
          #@(142 89)
          #@(70 16)
          "Untitled"
          'NIL)
        (MAKE-DIALOG-ITEM
          'RADIO-BUTTON-DIALOG-ITEM
          #@(464 94)
          #@(70 16)
          "Untitled"
          'NIL
          :RADIO-BUTTON-CLUSTER
          1)
        (MAKE-DIALOG-ITEM
          'RADIO-BUTTON-DIALOG-ITEM
          #@(470 123)
          #@(70 16)
          "Untitled"
          'NIL
          :RADIO-BUTTON-PUSHED-P
          T
          :RADIO-BUTTON-CLUSTER
          1)
        (MAKE-DIALOG-ITEM
          'RADIO-BUTTON-DIALOG-ITEM
          #@(459 160)
          #@(70 16)
          "Untitled"
          'NIL
          :RADIO-BUTTON-CLUSTER
          1)
        (MAKE-DIALOG-ITEM
          'BUTTON-DIALOG-ITEM
          #@(448 266)
          #@(72 20)
          "Untitled"
          'NIL
          :DEFAULT-BUTTON
          NIL)
        (MAKE-DIALOG-ITEM
          'STATIC-TEXT-DIALOG-ITEM
          #@(188 258)
          #@(54 16)
          "Untitled"
          'NIL
          :DIALOG-ITEM-ENABLED-P
          NIL)
        (MAKE-DIALOG-ITEM
          'CHECK-BOX-DIALOG-ITEM
          #@(451 342)
          #@(70 16)
          "Untitled"
          'NIL)
        (MAKE-DIALOG-ITEM
          'CHECK-BOX-DIALOG-ITEM
          #@(434 389)
          #@(70 16)
          "Untitled"
          'NIL)
        (MAKE-DIALOG-ITEM
          'EDITABLE-TEXT-DIALOG-ITEM
          #@(139 406)
          #@(81 16)
          "Untitled"
          'NIL
          :PART-COLOR-LIST
          '(:BODY 16777215)
          :ALLOW-RETURNS
          NIL
          :DRAW-OUTLINE
          T)))
