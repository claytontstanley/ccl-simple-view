(defun set-color-of-feats (color feats)
  (dolist (feat feats feats)
    (set-chunk-slot-value-fct feat 'color color)))

(defmethod build-vis-locs-for ((self editable-text-dialog-item) (vis-mod vision-module))
  (let* ((font-spec (view-font self))
         (text (dialog-item-text self))
         (feats 
           (cons
             (car (define-chunks-fct `((isa visual-location
                                            screen-x ,(px (view-loc self))
                                            screen-y ,(py (view-loc self))
                                            kind visual-object
                                            value box
                                            height ,(point-v (view-size self))
                                            width ,(point-h (view-size self))))))
             (unless (equal text "")
               (multiple-value-bind (ascent descent) (font-info font-spec)
                 (set-color-of-feats (system-color->symbol (part-color self :text))
                                     (build-string-feats vis-mod :text text
                                                         :start-x (1+ (point-h (view-position self)))
                                                         :y-pos (+ (point-v (view-position self))
                                                                   descent (round ascent 2))
                                                         :width-fct #'(lambda (str)
                                                                        (string-width str font-spec))
                                                         :height ascent :obj self)))))))
    (dolist (x feats)
      (setf (chunk-visual-object x) self))
    feats))

(defmethod build-vis-locs-for ((self button-dialog-item) (vis-mod vision-module))
  (let* ((btn-width (point-h (view-size self)))
         (btn-height (point-v (view-size self)))
         (text (dialog-item-text self))
         (feats (cons
                  (car (define-chunks-fct `((isa visual-location
                                                 screen-x ,(px (view-loc self))
                                                 screen-y ,(py (view-loc self))
                                                 kind oval
                                                 value oval
                                                 height ,(point-v (view-size self))
                                                 width ,(point-h (view-size self))
                                                 color light-gray))))
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
                        (dolist (item textlines (flatten (nreverse accum)))
                          (push
                            (set-color-of-feats (system-color->symbol (part-color self :text))
                                                (build-string-feats vis-mod :text item
                                                                    :start-x 
                                                                    (+ (point-h (view-position self))
                                                                       (round 
                                                                         (- btn-width (funcall width-fct item))
                                                                         2))
                                                                    :y-pos 
                                                                    (+ start-y (round (+ ascent descent) 2))
                                                                    :width-fct width-fct 
                                                                    :height (min ascent btn-height)
                                                                    :obj self))
                            accum)
                          (incf start-y (+ ascent descent)))))))))
    (let ((fun (lambda (x y) (declare (ignore x)) (approach-width (car feats) y))))
      (dolist (x (cdr feats))
        (setf (chunk-visual-approach-width-fn x) fun)
        (set-chunk-slot-value-fct x 'color 'black)))
  (dolist (x feats)
    (setf (chunk-visual-object x) self))
  feats))
