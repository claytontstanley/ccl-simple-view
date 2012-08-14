(defclass string-dialog (dialog)
  ((allow-empty-strings :initform nil :initarg :allow-empty-strings)))

(defclass get-string-dialog (string-dialog)())

(defmethod update-default-button ((obj string-dialog)) ())

(defmethod set-view-size ((dialog get-string-dialog) h &optional v)
  (declare (ignore h v))
  (let* ((old-size (view-size dialog)))
    (call-next-method)
    (let* ((new-size (view-size dialog))
           (hdelta (make-point (- (point-h old-size)(point-h new-size)) 0))
           (subs (view-subviews dialog))
           (len (length subs)))
      (dotimes (i len)
        (let ((sub (elt subs i)))
          (if (typep sub 'button-dialog-item)
            (set-view-position sub (subtract-points (view-position sub) hdelta))
            (if (typep sub 'editable-text-dialog-item)
              (set-view-size sub (subtract-points (view-size sub) hdelta)))))))))

;; could be prettier, need a set-view-size method - move buttons, resize editable-text - done
; 140 x 80 is about minumum useful size - neg size is invisible
(with-continue
  (defun get-string-from-user (message 
                                &key
                                initial-string
                                (size #@(365 100))
                                (position #@(140 140))
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
                         #@(62 25)
                         ok-text
                         (if (not modeless)
                           #'(lambda (item)
                               (return-from-modal-dialog (act-on-text item)))
                           #'act-on-text))                     
                       (make-dialog-item 'button-dialog-item
                                         (make-point (- (point-h size) 154)
                                                     (- (point-v size) 20 delta))
                                         #@(70 25)
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
                                         initial-string
                                         nil
                                         :view-nick-name :et))))
      (when message (add-subviews  dialog  message-item))
      (update-default-button dialog)
      (let ((et (view-named :et dialog)))
        (set-selection-range et 0 (length (dialog-item-text et))))
      (cond ((not modeless)         
              (modal-dialog dialog))
            (t (window-show dialog)
             dialog)))))


; need close box if modal nil 
(defun message-dialog (message &key (ok-text "OK")
                               (size #@(335 100))
                               (modal t)   ; if not modal its callers job to select
                               (title "Warning")
                               window-type
                               (back-color *tool-back-color*)
                               (theme-background t)
                               (position (list :top (+ *menubar-bottom* 10))))
  (let* ((message-width (- (point-h size) 85))
         (new-dialog
           (make-instance
             'dialog
             :view-position position
             :view-size size
             :window-title title
             :window-type (or window-type (if modal :movable-dialog :document))
             :close-box-p (if modal nil t)
             :window-show nil
             :back-color back-color
             :theme-background theme-background
             :view-subviews
             `(,(make-instance
                  'static-text-dialog-item
                  :dialog-item-text message
                  :view-size (make-point
                               message-width
                               (- (point-v size)
                                  30)))
                ,@(if modal
                    (list (make-dialog-item
                            'default-button-dialog-item
                            (subtract-points size #@(75 35))
                            #@(62 25)
                            ok-text
                            #'(lambda (item)
                                (declare (ignore item))
                                (return-from-modal-dialog t))
                            :view-nick-name :db)))))))
    (if modal
      (modal-dialog new-dialog)
      new-dialog)))

