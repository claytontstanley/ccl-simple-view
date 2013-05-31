#+:clozure (require :ccl-simple-view)

(defun warning (string &key (cursor nil) 
                       (size #@(400 200)))
  "Displays a warning dialog, with a cursor."
  (when cursor (#_ShowCursor))
  (message-dialog string :title "Message" :size size :position #@(300 250))
  (when cursor (#_HideCursor)))

(defun cursor-is-visible-p ()
  (ecase (#_CGCursorIsVisible)
    (0 nil)
    (1 t)))

(provide :chil-utilities)
