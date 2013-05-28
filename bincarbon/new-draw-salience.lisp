;;;  -*- mode: LISP; Package: CL-USER; Syntax: COMMON-LISP;  Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Author      : Mike Byrne
;;; Copyright   : (c)2006 Rice U./Mike Byrne, All Rights Reserved
;;; Availability: public domain
;;; Address     : Rice University
;;;             : Psychology Department
;;;             : Houston,TX 77251-1892
;;;             : byrne@acm.org
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Filename    : draw-salience.lisp
;;; Version     : r2
;;; 
;;; Description : Draws salience information on windows.  Works only in MCL.
;;;             : 
;;;             : There are three functions here, none of which take any
;;;             : arguments:
;;;             : 
;;;             : DRAW-SALIENCE-OVALS
;;;             : This draws ovals over the objects in the window, and the more
;;;             : salient the object is, the darker the oval.
;;;             : 
;;;             : PRINT-SALIENCE-VAL
;;;             : This prints the actual salience value for each object next
;;;             : to the object.  Higher salience gets darker text.
;;;             : 
;;;             : PRINT-SALIENCE-PROB
;;;             : This prints a probability next to each object, which is the 
;;;             : probability of that object being returned from a 
;;;             : +VISUAL-LOCATION> call.  Assumes that there's noise, of 
;;;             : course.  Higher probability gets darker text.
;;; 
;;; Bugs        : 
;;; 
;;; Todo        : 
;;; 
;;; ----- History -----
;;; 2006.07.18 mdb [r2]
;;;             : Updated some rendering code, renamed functions.
;;; 2006.07.07 Mike Byrne
;;;             :  Incept date.
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defparameter *base-colors* '(14795253 4653446))


(defun draw-salience-ovals ()
  (let* ((feat-lst (sort (copy-list (visicon-chunks (get-module :vision)))
                         #'< :key #'effective-salience))
         (color-lst (apply #'generate-colors (length feat-lst) *base-colors*)))
    (invalidate-view (current-device) t)
    (event-dispatch)
    (window-select (current-device))
    (set-pen-mode (current-device) :srccopy)
    (with-focused-view (current-device)
      (dotimes (i (length feat-lst))
        (render-salience-oval (nth i feat-lst) (nth i color-lst))))))

(defun render-salience-oval (feat color)
  (let ((cntr (list (chunk-slot-value-fct feat 'screen-x)
                    (chunk-slot-value-fct feat 'screen-y)))
        (x  (round (chunk-slot-value-fct feat 'width) 1.4))
        (y  (round (chunk-slot-value-fct feat 'height) 1.4)))
    (with-fore-color color
      (paint-oval (current-device) (- (nth 0 cntr) x) (- (nth 1 cntr) y)
                  (+ (nth 0 cntr) x) (+ (nth 1 cntr) y)))))


(defun generate-colors (nsteps start-color end-color)
  (multiple-value-bind (start-red start-green start-blue)
    (color-values start-color)
    (multiple-value-bind (end-red end-green end-blue)
      (color-values end-color)
      "Produces a blend of <steps> colors."
      (let ((delta-blue (/ (- end-blue start-blue) nsteps))
            (delta-green (/ (- end-green start-green) nsteps))
            (delta-red (/ (- end-red start-red) nsteps))
            (answer '()))
        (dotimes (step nsteps)
          (push (make-color (round (+ start-red (* step delta-red)))
                            (round (+ start-green (* step delta-green)))
                            (round (+ start-blue (* step delta-blue))))
                answer))
        (nreverse answer)))))


(defun print-salience-val ()
  (let* ((feat-lst (sort (copy-list (visicon-chunks (get-module :vision)))
                         #'< :key #'pre-noise-salience))
         (color-lst (apply #'generate-colors (length feat-lst) *base-colors*)))
    (invalidate-view (current-device) t)
    (event-dispatch)
    (window-select (current-device))
    (set-pen-mode (current-device) :srccopy)
    (with-focused-view (current-device)
      (dotimes (i (length feat-lst))
        (render-feat-num (nth i feat-lst) (nth i color-lst)
                         (effective-salience (nth i feat-lst)))))))


(defun print-salience-prob ()
  (if (zerop *salience-noise*)
    (error "No salience noise; results are deterministic.")
    (let* ((feat-lst (sort (copy-list (visicon-chunks (get-module :vision))) 
                           #'< :key #'pre-noise-salience))
           (color-lst (apply #'generate-colors (length feat-lst) *base-colors*))
           (prob-lst (mapcar 
                       #'(lambda (f)
                           (exp (/ (pre-noise-salience f)
                                   (* *salience-noise* (sqrt 2))))) feat-lst))
           (tot (reduce #'+ prob-lst)))
      (setf prob-lst (mapcar #'(lambda (p) (/ p tot)) prob-lst))
      (invalidate-view (current-device) t)
      (event-dispatch)
      (window-select (current-device))
      (set-pen-mode (current-device) :srccopy)
      (with-focused-view (current-device)
        (dotimes (i (length feat-lst))
          (render-feat-num (nth i feat-lst) (nth i color-lst)
                           (nth i prob-lst)))))))


(defun render-feat-num (feat color num)
  (let ((str (format nil "~5,2F" num))
        (loc (list (chunk-slot-value-fct feat 'screen-x)
                   (chunk-slot-value-fct feat 'screen-y))))
    (with-fore-color color
      (move-to (current-device) (nth 0 loc) (- (nth 1 loc) 10))
      (with-pstrs ((ps str))
                  (#_drawstring ps))
      (move-to (current-device) (nth 0 loc) (- (nth 1 loc) 10)))))
