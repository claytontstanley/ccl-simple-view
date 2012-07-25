
#+:clozure (defmethod build-vis-locs-for ((view back-image-view) (vm vision-module))
             (declare (ignore view vm))
             nil)

(defmethod build-vis-locs-for ((self radio-button-dialog-item)
                               (vis-mod vision-module))
  (let* ((btn-height (point-v (view-size self)))
         (text (dialog-item-text self)))
    (cons
      (car (define-chunks-fct `((isa visual-location
                                     screen-x ,(+ 7 (point-h (view-position self)))
                                     screen-y ,(py (view-loc self))
                                     kind oval
                                     value oval
                                     height 11
                                     width 11
                                     color ,(if (radio-button-pushed-p self)
                                              'black
                                              'light-gray)))))
      (unless (equal text "")
        (let* ((font-spec (view-font self))
               (start-y nil)
               (accum nil)
               (textlines (string-to-lines text))
               (width-fct #'(lambda (str) (string-width str font-spec))))
          (multiple-value-bind (ascent descent) (font-info font-spec)
            (setf start-y (+ (point-v (view-position self))
                             (round (- btn-height (* (length textlines)
                                                     (+ ascent descent))) 2)))
            (dolist (item textlines (nreverse accum))
              (push
                (set-color-of-feats (system-color->symbol (part-color self :text))
                                    (build-string-feats vis-mod :text item
                                                        :start-x 
                                                        (+ (point-h (view-position self))
                                                           17)
                                                        :y-pos 
                                                        (+ start-y (round (+ ascent descent) 2))
                                                        :width-fct width-fct 
                                                        :height (min ascent btn-height)
                                                        :obj self))
                accum)
              (incf start-y (+ ascent descent)))))))))

;;; BUILD-FEATURES-FOR      [Method]
;;; Date        : 02.04.16
;;; Description : Very much like radio buttons, but if checked add an 
;;;             : "X" to the output.
(defmethod build-vis-locs-for ((self check-box-dialog-item)
                               (vis-mod vision-module))
  (let ((btn-height (point-v (view-size self)))
        (text (dialog-item-text self))
        (feats nil))
    (setf feats
          (cons
            (car (define-chunks-fct `((isa visual-location
                                           screen-x ,(+ 8 (point-h (view-position self)))
                                           screen-y ,(py (view-loc self))
                                           kind visual-object
                                           value box
                                           height 11
                                           width 11
                                           color ,(if (check-box-checked-p self)
                                                    'blue
                                                    'light-gray)))))
            (unless (equal text "")
              (let* ((font-spec (view-font self))
                     (start-y nil)
                     (accum nil)
                     (textlines (string-to-lines text))
                     (width-fct #'(lambda (str) (string-width str font-spec))))
                (multiple-value-bind (ascent descent) (font-info font-spec)
                  (setf start-y (+ (point-v (view-position self))
                                   (round (- btn-height (* (length textlines)
                                                           (+ ascent descent))) 2)))
                  (dolist (item textlines (nreverse accum))
                    (push
                      (set-color-of-feats (system-color->symbol (part-color self :text))
                                          (build-string-feats vis-mod :text item
                                                              :start-x 
                                                              (+ (point-h (view-position self))
                                                                 17)
                                                              :y-pos 
                                                              (+ start-y (round (+ ascent descent) 2))
                                                              :width-fct width-fct 
                                                              :height (min ascent btn-height)
                                                              :obj self))
                      accum)
                    (incf start-y (+ ascent descent))))))))
    (when (check-box-checked-p self)
      (setf feats
            (cons
              (car (define-chunks-fct `((isa visual-location
                                             screen-x ,(+ 8 (point-h (view-position self)))
                                             screen-y ,(py (view-loc self))
                                             kind visual-object
                                             value check
                                             height 11
                                             width 11))))
              feats)))
    feats))
