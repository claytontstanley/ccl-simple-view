(defun return-cancel (i)
  (declare (ignore i))
  (return-from-modal-dialog :cancel))

(defclass string-dialog (dialog)
  ((allow-empty-strings :initform nil :initarg :allow-empty-strings)))

(defclass get-string-dialog (string-dialog)())

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
; 140 x 80 is about minimum useful size - neg size is invisible
(with-continue
  (defun get-string-from-user (message 
                                &key
                                initial-string
                                (size #@(365 100))
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
      (let* ((msize (view-size message-item))
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
                                         (make-point (- (point-h size) delta message-len) 23)
                                         initial-string))))
      (when message (add-subviews  dialog  message-item))
      (update-default-button dialog)
      (cond ((not modeless)         
              (modal-dialog dialog))
            (t (window-show dialog)
             dialog)))))


(defmethod update-default-button ((obj string-dialog)) ())

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
                  :text-truncation #$NSLineBreakByWordWrapping
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

(defclass select-dialog (window) ())

(with-continue
  (defun select-item-from-list (the-list &key (window-title "Select an Item")
                                         (selection-type :single)
                                         table-print-function 
                                         (action-function #'identity)
                                         (default-button-text "OK")
                                         (sequence-item-class 'sequence-dialog-item)
                                         (view-size (make-point 400 (+ 80 (* (length the-list) 20))))
                                         (view-position '(:top 90) pos-p)
                                         (theme-background t)
                                         dialog-class
                                         modeless
                                         (help-spec 14086)
                                         (list-spec 14087)
                                         (button-spec 14088))
    "Displays the elements of a list, and returns the item chosen by the user"
    (let (debutton dialog)
      (flet ((act-on-items (item)
               (let ((s-item (find-subview-of-type (view-container item)
                                                   'sequence-dialog-item)))
                 (funcall action-function 
                          (mapcar #'(lambda (c) (cell-contents s-item c))
                                  (selected-cells s-item))))))
        (when (and dialog-class (not pos-p) modeless)
          (let ((w (front-window :class 'select-dialog)))  ; or dialog-class?
            (when w (setq view-position (add-points (view-position w) #@(15 15))))))
        (setq debutton
              (make-instance 
                'default-button-dialog-item
                :dialog-item-text default-button-text
                :dialog-item-enabled-p the-list
                :help-spec button-spec
                :action
                (cond 
                  ((not modeless)
                   #'(lambda ()
                       (return-from-modal-dialog (act-on-items debutton))))
                  (t
                   (lambda () (act-on-items debutton))))))
        (let* ((bsize (view-default-size debutton))
               bpos)
          (setq bsize (make-point (max 60 (point-h bsize)) (point-v bsize))
            bpos (make-point (- (point-h view-size) 25 (point-h bsize))
                             (- (point-v view-size) 7 (point-v bsize))))
          (set-view-size debutton bsize)
          (set-view-position debutton bpos)
          (setq dialog
            (make-instance
              (or dialog-class 'select-dialog)
              :window-type :document-with-grow
              :close-box-p (if modeless t nil)
              :window-title window-title
              :view-size view-size
              :view-position view-position
              :window-show nil ;modeless
              :back-color *tool-back-color*
              :theme-background theme-background
              :help-spec help-spec
              :view-subviews
              (list*
                (make-instance
                  sequence-item-class
                  :view-position #@(4 4)
                  :view-size (make-point (- (point-h view-size) 8)
                                         (+ 50 (- (point-v view-size) (point-v bsize) 20)))
                  ;:table-hscrollp nil
                  :table-sequence the-list
                  :table-print-function table-print-function
                  :selection-type selection-type
                  :help-spec list-spec)
                debutton
                (if (not modeless)
                  (list
                    (make-dialog-item 'button-dialog-item
                                      (make-point (- (point-h bpos) 80)
                                                  (point-v bpos))
                                      (make-point (if t #|(osx-p)|# 74 60) (point-v bsize))
                                      "Cancel"
                                      #'return-cancel
                                      :cancel-button t
                                      :help-spec 15012))
                  nil))))
          ;(when the-list (cell-select sdi (index-to-cell sdi 0))) ; let arrow-dialog-item do this
          (cond (modeless ; select first then show is prettier
                  (window-show dialog)
                  dialog)

                (t ;(#_changewindowattributes (wptr dialog) 0 #$kWindowCollapseBoxAttribute)
                 (modal-dialog dialog))))))))


