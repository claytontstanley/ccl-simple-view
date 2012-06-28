;; could be prettier, need a set-view-size method - move buttons, resize editable-text - done
; 140 x 80 is about minumum useful size - neg size is invisible
(with-continue
  (defun get-string-from-user (message 
                                &key
                                initial-string
                                (size (make-point 365 100))
                                (position '(:bottom 140))
                                (ok-text "OK")
                                (cancel-text "Cancel")
                                (modeless nil)
                                (window-title "")
                                (window-type :document-with-grow)
                                (back-color *tool-back-color*)
                                (allow-empty-strings nil)
                                (action-function #'identity)
                                cancel-function
                                (theme-background t)
                                &aux dialog (delta 0) (message-len 0) message-item)
    (when (not initial-string) (setq initial-string ""))
    (if t (setq delta 20)(setq delta 10))  
    (when message 
      (setq message-item (make-instance 'static-text-dialog-item
                                        :text-truncation :end
                                        :view-position (make-point 6 (- (point-v size) 54 delta))
                                        :dialog-item-text message))
      (let* ((msize (view-default-size message-item))
             (mh (point-h msize)))  ;; would be nice if static text had a truncate option -now it does
        (setq mh (min mh (- (point-h size) 100)))
        (set-view-size message-item (make-point mh (point-v msize))))
      (setq message-len (+ 6 (point-h (view-size message-item)))))
    (flet ((act-on-text (item)
             (let ((e-item
                     (find-subview-of-type (view-container item)
                                           'editable-text-dialog-item)))
               (funcall action-function (dialog-item-text e-item)))))    
      (setq dialog (make-instance 
                     'get-string-dialog
                     :view-position position
                     :view-size size
                     :close-box-p (if modeless t nil)
                     :grow-box-p t
                     :window-type window-type
                     :window-title window-title
                     :window-show nil
                     :back-color back-color
                     :theme-background theme-background
                     :allow-empty-strings allow-empty-strings
                     :view-subviews
                     (list
                       (make-dialog-item
                         'default-button-dialog-item
                         (make-point (- (point-h size) 74)
                                     (- (point-v size) 20 delta))
                         #@(62 20)
                         ok-text
                         (if (not modeless)
                           #'(lambda (item)
                               (return-from-modal-dialog (act-on-text item)))
                           #'act-on-text))                     
                       (make-dialog-item 'button-dialog-item
                                         (make-point (- (point-h size) 154)
                                                     (- (point-v size) 20 delta))
                                         #@(62 20)
                                         cancel-text
                                         (or cancel-function
                                             #'(lambda (item)
                                                 (if (not modeless) 
                                                   (return-from-modal-dialog :cancel)
                                                   (window-close (view-window item)))))
                                         :cancel-button t)
                       (make-dialog-item 'editable-text-dialog-item
                                         (make-point (+ 6 message-len) (- (point-v size) 54 delta))
                                         (make-point (- (point-h size) delta message-len) 16)
                                         initial-string))))
      (when message (add-subviews  dialog  message-item))

      (update-default-button dialog)
      (cond ((not modeless)         
              (modal-dialog dialog))
            (t (window-show dialog)
             dialog)))))


