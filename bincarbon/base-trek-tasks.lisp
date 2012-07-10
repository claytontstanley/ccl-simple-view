;;;  -*- mode: LISP; Package: CL-USER; Syntax: COMMON-LISP;  Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Author      : Mike Byrne
;;; Copyright   : (c)2005 Rice U./Mike Byrne, All Rights Reserved
;;; Availability: public domain
;;; Address     : Rice University
;;;             : Psychology Department
;;;             : Houston,TX 77251-1892
;;;             : byrne@acm.org
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Filename    : base-trek-tasks.lisp
;;; Version     : r4
;;; 
;;; Description : Base classes for Conn (or ship's course), phaser, and 
;;;             : transporter tasks.
;;; 
;;; Bugs        : 
;;; 
;;; Todo        : 
;;; 
;;; ----- History -----
;;; 2007.07.09 fpt [r5]
;;;		: added torpedo-window subclass of the phaser-window and
;;;		jammer-window subclass of the transporter-window to avoid
;;;		having to duplicate so much code
;;; 2007.07.09 fpt [r4]
;;;		: altered transporter-window to get the scan dots' positions
;;;		from the transporter window
;;; 2006.11.21 fpt & mdb [r3]
;;;             : altered class definition for transporter-window, method 
;;;             : definition for move-target, and method definition for 
;;;             : begin-tracking to get target coords from the implemented 
;;;             : instance of a transporter-window
;;; 2006.01.30 mdb 
;;;             : Slightly increased hit probability for transporter.
;;; 2005.11.10 mdb [r2]
;;;             : Small change to deal with different key states in transp.
;;; 2005.06.30 mdb
;;;             : Fixed cursor handling in transporter window.
;;; 2005.06.26 mdb
;;;             : Generalized targeting code so target pane can be anywhere.
;;; 05.06.21 mdb
;;;             :  Incept date.
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(require :procedure-window2)
(require :icon-dialog-item)             ; transporter
(require :thermometer)                  ; phaser

(defparameter *transporter-speed* 8)


;;;; ---------------------------------------------------------------------- ;;;;
;;;;
;;;; Ship's course subclasses, main class, methods, and utility functions
;;;;
;;;; ---------------------------------------------------------------------- ;;;;


;;;; ---------------------------------------------------------------------- ;;;;
;;;; simple oval

(defclass simple-oval (simple-view)
  ((fill-color :accessor fill-color :initarg :fill-color 
               :initform *black-color*)))


(defmethod view-draw-contents ((self simple-oval))
  (let ((dim (view-size self)))
    (with-fore-color (fill-color self)
      (fill-oval self *black-pattern* 0 0 (point-h dim) (point-v dim)))))



;;;; ---------------------------------------------------------------------- ;;;;
;;;; Ships' course class definition
;;;; ---------------------------------------------------------------------- ;;;;

#+:clozure
(defclass static-contained-view (static-view-mixin contained-view) ())

#+:clozure
(defclass static-window (window)
  ()
  (:default-initargs :contained-view-specifically 'static-contained-view)) 

(defclass ships-course-window (procedure-window #+:clozure static-window)
  ((draw-hack-p :accessor draw-hack-p :initarg :draw-hack-p :initform t) 
   (match-p :accessor match-p :initarg :match-p :initform (flip)))
  (:default-initargs
    :window-title "Ship's Course"
    :view-font '("Chicago" 12 :srcor :plain (:color-index 0))
    :pict-name "Course"
    :state-vec #(:CONFIRM-COURSE :TYPE :ACCEPT-COURSE)
    :check-gui-p nil
    :task-id 200
    :view-subviews
    (list 
     (make-dialog-item
      'button-checker
      #@(588 145)
      #@(142 40)
      "Confirm Course"
      'nil
      :view-nick-name :CONFIRM-COURSE
      :view-font '("Charcoal" 12 :srccopy :plain (:color-index 0))
      :default-button nil)
     (make-dialog-item
      'button-checker
      #@(596 593)
      #@(142 40)
      "Accept Course"
      'nil
      :view-nick-name :ACCEPT-COURSE
      :view-font
      '("Charcoal" 12 :srccopy :plain (:color-index 0))
      :default-button nil)
     (make-dialog-item
      'static-text-dialog-item
      #@(744 327)
      #@(24 14)
      ""
      'nil
      :view-nick-name :CURRENT-Z
      :view-font '("Avant Garde" 14 :srcor :bold (:color-index 0)))
     (make-dialog-item
      'static-text-dialog-item
      #@(590 327)
      #@(24 14)
      ""
      'nil
      :view-nick-name :CURRENT-X
      :view-font '("Avant Garde" 14 :srcor :bold (:color-index 0)))
     (make-dialog-item
      'static-text-dialog-item
      #@(666 327)
      #@(24 14)
      ""
      'nil
      :view-nick-name :CURRENT-Y
      :view-font '("Avant Garde" 14 :srcor :bold (:color-index 0)))
     (make-dialog-item
      'static-text-dialog-item
      #@(347 566)
      #@(25 15)
      ""
      'nil
      :view-nick-name :PROG-Z
      :view-font '("Avant Garde" 14 :srcor :bold (:color-index 0)))
     (make-dialog-item
      'static-text-dialog-item
      #@(270 566)
      #@(26 16)
      ""
      'nil
      :view-nick-name :PROG-Y
      :view-font
      '("Avant Garde" 14 :srcor :bold (:color-index 0))
      :part-color-list
      '(:frame 0))
     (make-dialog-item
      'static-text-dialog-item
      #@(193 566)
      #@(24 16)
      ""
      'nil
      :view-nick-name :PROG-X
      :view-font
      '("Avant Garde" 14 :srcor :bold (:color-index 0))
      :part-color-list
      '(:frame 0))
     (make-dialog-item
      'editable-text-dialog-item
      #@(588 483)
      #@(30 16)
      ""
      'nil
      :view-nick-name :X-CORRECT
      :view-font '("Avant Garde" 14 :srcor :bold (:color-index 0))
      :allow-returns nil
      :draw-outline t)
     (make-dialog-item
      'editable-text-dialog-item
      #@(664 483)
      #@(30 16)
      ""
      'nil
      :view-nick-name :Y-CORRECT
      :view-font '("Avant Garde" 14 :srcor :bold (:color-index 0))
      :allow-returns nil
      :draw-outline t)
     (make-dialog-item
      'editable-text-dialog-item
      #@(740 483)
      #@(30 16)
      ""
      'nil
      :view-nick-name :Z-CORRECT
      :view-font '("Avant Garde" 14 :srcor :bold (:color-index 0))
      :allow-returns nil
      :draw-outline t)
     )))


;;;; Initialization

(defmethod initialize-instance :after ((wind ships-course-window) &key)
  (set-view-pict wind (pict-name wind))
  (set-dialog-item-text (view-named :PROG-X wind) (mkstr (1+ (random 98))))
  (set-dialog-item-text (view-named :PROG-Y wind) (mkstr (1+ (random 98))))
  (set-dialog-item-text (view-named :PROG-Z wind) (mkstr (1+ (random 98))))
  (start-timing (timer wind))
  (set-cursor *arrow-cursor*)
  (setf (draw-hack-p wind) nil)
  )


(defmethod show-prog-heading ((wind ships-course-window))
  (let ((heading (prog-heading wind))
        (pos nil))
    (setf pos (heading-to-loc wind (first heading) (second heading) 
                              (third heading)))
    (add-subviews wind
      (make-instance 'simple-oval :view-size #@(15 15)
                     :fill-color *green-color*
                     :view-position (make-point (- (first pos) 7)
                                                (- (second pos) 7))))
    (move-to wind 268 313)
    (with-focused-view wind
      (with-fore-color *green-color*
        (line-to wind (first pos) (second pos))))))



;;;; event handling

(defmethod check-state-update ((wind ships-course-window) state)
  (case state
    (:CONFIRM-COURSE (confirm-course wind))
    (:ACCEPT-COURSE (accept-course wind))
    ))


(defmethod view-key-event-handler :around ((wind ships-course-window) key)
  (when (eq (curr-state wind) :ACCEPT-COURSE)
    (decf (state-num wind)))
  (when (eq (curr-state wind) :TYPE)
    (call-next-method))
  (state-check wind :TYPE key))



(defmethod confirm-course ((wind ships-course-window))
  (cond ((match-p wind)
         (set-dialog-item-text (view-named :CURRENT-X wind) 
                               (mkstr (1+ (random 98))))
         (set-dialog-item-text (view-named :CURRENT-Y wind) 
                               (mkstr (1+ (random 98))))
         (set-dialog-item-text (view-named :CURRENT-Z wind) 
                               (mkstr (1+ (random 98)))))
        (t
         (set-dialog-item-text (view-named :CURRENT-X wind) 
                               (dialog-item-text (view-named :PROG-X wind)))
         (set-dialog-item-text (view-named :CURRENT-Y wind) 
                               (dialog-item-text (view-named :PROG-Y wind)))
         (set-dialog-item-text (view-named :CURRENT-Z wind) 
                               (dialog-item-text (view-named :PROG-Z wind)))))
  (show-curr-heading wind)
  )
        

(defmethod accept-course ((wind ships-course-window))
  (cond ((equal (heading-correction wind) (heading-diff wind))
         (play-snd *sp* "glass")
         (finish-task wind))
        (t
         (play-snd *sp* "uh oh" t)
         (decf (state-num wind)))))


(defmethod show-curr-heading ((wind ships-course-window))
  (let ((heading (curr-heading wind))
        (pos nil))
    (setf pos (heading-to-loc wind (first heading) (second heading) 
                              (third heading)))
    (add-subviews wind
      (make-instance 'simple-oval :view-size #@(15 15)
                     :fill-color *red-color*
                     :view-position (make-point (- (first pos) 7)
                                                (- (second pos) 7))))
    (move-to wind 268 313)
    (with-focused-view wind
      (with-fore-color *red-color*
        (line-to wind (first pos) (second pos))))))
 

(defmethod window-null-event-handler ((wind ships-course-window))
  (unless (draw-hack-p wind)
    (show-prog-heading wind)
    (setf (draw-hack-p wind) t))
  (check-wm wind))




;;;; Utility methods

(defmethod heading-to-loc ((wind ships-course-window) x y z)
  (let ((zdim (round (* z 74/99))))
    (list
     (- (+ 268 (round (* x 160/99))) zdim)
     (+ (- 313 (round (* y 144/99))) zdim))))


(defmethod prog-heading ((wind ships-course-window))
  (list
   (read-from-string (dialog-item-text (view-named :PROG-X wind)))
   (read-from-string (dialog-item-text (view-named :PROG-Y wind)))
   (read-from-string (dialog-item-text (view-named :PROG-Z wind)))))


(defmethod curr-heading ((wind ships-course-window))
  (list
   (read-from-string (dialog-item-text (view-named :CURRENT-X wind)))
   (read-from-string (dialog-item-text (view-named :CURRENT-Y wind)))
   (read-from-string (dialog-item-text (view-named :CURRENT-Z wind)))))


(defmethod heading-diff ((wind ships-course-window))
  (let ((heading-lst (nreverse (pairlis (prog-heading wind)
                                          (curr-heading wind)))))
    (mapcar #'(lambda (x) (- (first x) (rest x)))
            heading-lst)))


(defmethod heading-correction ((wind ships-course-window))
  (list
   (read-from-string (dialog-item-text (view-named :X-CORRECT wind)))
   (read-from-string (dialog-item-text (view-named :Y-CORRECT wind)))
   (read-from-string (dialog-item-text (view-named :Z-CORRECT wind)))))


;;;; ---------------------------------------------------------------------- ;;;;
;;;;
;;;; Phaser subclasses, main class, methods, and utility functions
;;;;
;;;; ---------------------------------------------------------------------- ;;;;

;;;; ---------------------------------------------------------------------- ;;;;
;;;; Subviews of the phaser window
;;;; ---------------------------------------------------------------------- ;;;;

;;;; Click indicator class

(defclass click-indicator (simple-view)
  ((indicator :accessor indicator :initarg :indicator :initform nil)
   (v-offset :accessor v-offset :initarg :v-offset :initform 3)
   )
  (:default-initargs
    :view-size #@(100 25))
  )


(defmethod view-draw-contents ((item click-indicator))
  ;(with-fore-color *red-color* (frame-rect item #@(0 0) (view-size item)))
  (awhen (indicator item)
    (move-to item it (v-offset item))
    (start-polygon item)
    (line-to item (- it 17) (+ (v-offset item) 18))
    (line-to item (+ it 17) (+ (v-offset item) 18))
    (line-to item it (v-offset item))
    (fill-polygon item *black-pattern* (get-polygon item))))


(defmethod set-indicator ((item click-indicator) val)
  (setf (indicator item) val)
  (invalidate-view item t)
  (view-draw-contents item))


(defmethod view-click-event-handler ((item click-indicator) where)
  (when (eq (curr-state (view-window item)) :FOCUS-SET)
    (decf (state-num (view-window item))))
  (when (eq (view-nick-name item) (curr-state (view-window item)))
    (set-indicator item (point-h (add-points where (view-origin item)))))
  (handle-click item))



;;;; Target view class

(defclass target-sv (simple-view)
  ()
  )

(defmethod view-draw-contents ((self target-sv))
  (let ((dim (view-size self)))
    (fill-rect self *black-pattern* 0 0 (point-h dim) (point-v dim))))


(defclass phaser-window (procedure-window)
  ((charge-p :accessor charge-p :initarg :charge-p :initform nil)
   (track-p :accessor track-p :initarg :track-p :initform nil)
   (postcomp-p :accessor postcomp-p :initarg :postcomp-p :initform t)
   (pcondition :accessor pcondition :initarg :pcondition :initform 0)
   (num-hits :accessor num-hits :initarg :num-hits :initform 0)
   (last-hit-p :accessor last-hit-p :initarg :last-hit-p :initform nil)
   (last-dir :accessor last-dir :initarg :last-dir :initform (random 8))
   (idle-move :accessor idle-move :initarg :idle-move :initform 3)
   (key-move :accessor key-move :initarg :key-move :initform 5)
   (no-move :accessor no-move :initarg :no-move :initform nil)
   (trgt-v :accessor trgt-v :initarg :trgt-v 
           :initform (make-instance 'target-sv :view-size #@(11 11)
                                    :view-nick-name :TARGET))
   (trgt-cntr :accessor trgt-cntr :initarg :trgt-cntr :initform #@(258 231))
   (track-width :accessor track-width :initarg :track-width :initform 286)
   ;; for running in ACTR mode
   (last-update :accessor last-update :initarg :last-update :initform 0)
   )
  (:default-initargs
    :state-vec #(:POWER-CONNECTED :CHARGE :STOP-CHARGING :POWER-CONNECTED 
                                  :SETTINGS :SLIDER :FOCUS-SET :FIRING 
                                  :TRACKING :SHOOT :TRACKING :MAIN-CONTROL)
    :gui-vec #((check-box-check . :POWER-CONNECTED) nil nil 
               (check-box-uncheck . :POWER-CONNECTED) 
               (radio-button-push . :SETTINGS) nil 
               (check-box-check . :FOCUS-SET) (radio-button-push . :FIRING)
               nil nil nil nil)
    :pict-name "phaser"
    :window-title "Phaser"
    :VIEW-FONT '("Avant Garde" 12 :SRCCOPY :PLAIN (:COLOR-INDEX 0))
    :view-subviews
    (list 
     (make-dialog-item
      'button-checker
      #@(565 139)
      #@(120 34)
      "Charge"
      nil
      :VIEW-NICK-NAME :CHARGE
      :VIEW-FONT '("Avant Garde" 12 :SRCCOPY :PLAIN (:COLOR-INDEX 0))
      :DEFAULT-BUTTON NIL)
     (MAKE-DIALOG-ITEM
      'button-checker
      #@(567 282)
      #@(120 34)
      "Stop Charging"
      nil
      :VIEW-NICK-NAME :STOP-CHARGING
      :VIEW-FONT '("Avant Garde" 12 :SRCCOPY :PLAIN (:COLOR-INDEX 0))
      :DEFAULT-BUTTON NIL)
     (MAKE-DIALOG-ITEM
      'button-checker
      #@(171 412)
      #@(108 34)
      "Tracking"
      nil
      :VIEW-NICK-NAME :TRACKING
      :VIEW-FONT '("Avant Garde" 12 :SRCCOPY :PLAIN (:COLOR-INDEX 0))
      :DEFAULT-BUTTON NIL)
     (MAKE-DIALOG-ITEM
      'button-checker
      #@(512 618)
      #@(108 34)
      "Main Control"
      nil
      :VIEW-NICK-NAME :MAIN-CONTROL
      :VIEW-FONT '("Avant Garde" 12 :SRCCOPY :PLAIN (:COLOR-INDEX 0))
      :DEFAULT-BUTTON NIL)
     (MAKE-DIALOG-ITEM
      'radio-checker
      #@(534 367)
      #@(70 16)
      "Battery"
      nil
      :VIEW-NICK-NAME :BATTERY
      :VIEW-FONT '("Avant Garde" 12 :SRCCOPY :PLAIN (:COLOR-INDEX 0))
      :RADIO-BUTTON-PUSHED-P
      T)
     (MAKE-DIALOG-ITEM
      'radio-checker
      #@(534 407)
      #@(70 16)
      "Firing"
      nil
      :VIEW-NICK-NAME :FIRING
      :VIEW-FONT '("Avant Garde" 12 :SRCCOPY :PLAIN (:COLOR-INDEX 0)))
     (MAKE-DIALOG-ITEM
      'radio-checker
      #@(534 387)
      #@(70 16)
      "Settings"
      nil
      :VIEW-NICK-NAME :SETTINGS
      :VIEW-FONT '("Avant Garde" 12 :SRCCOPY :PLAIN (:COLOR-INDEX 0)))
     (MAKE-DIALOG-ITEM
      'check-box-checker
      #@(645 400)
      #@(136 16)
      "Focus Set"
      nil
      :VIEW-NICK-NAME :FOCUS-SET
      :VIEW-FONT '("Avant Garde" 12 :SRCCOPY :PLAIN (:COLOR-INDEX 0)))
     (MAKE-DIALOG-ITEM
      'check-box-checker
      #@(645 375)
      #@(136 16)
      "Power Connected"
      nil
      :VIEW-NICK-NAME :POWER-CONNECTED
      :VIEW-FONT '("Avant Garde" 12 :SRCCOPY :PLAIN (:COLOR-INDEX 0)))
     (MAKE-DIALOG-ITEM
      'STATIC-TEXT-DIALOG-ITEM
      #@(812 641)
      #@(54 14)
      "0"
      'NIL
      :VIEW-NICK-NAME :TIME
      :VIEW-FONT
      '("Avant Garde" 14 :SRCOR :PLAIN (:COLOR-INDEX 0)))
     (MAKE-DIALOG-ITEM
      'STATIC-TEXT-DIALOG-ITEM
      #@(136 595)
      #@(273 38)
      ""
      'NIL
      :VIEW-NICK-NAME :STATUS
      :VIEW-FONT '("Avant Garde" 14 :SRCOR :PLAIN (:COLOR-INDEX 0)))
     (make-instance 'thermometer 
       :view-position #@(764 136)
       :view-size #@(25 174)
       :view-nick-name :THERMO
       :pattern *light-gray-pattern*)
     (make-instance 'click-indicator :view-position #@(545 518)
                    :view-size #@(240 44) :v-offset 24
                    :view-nick-name :SLIDER)
     )))  

(defmethod initialize-instance :after ((wind phaser-window) &key)
  (set-view-pict wind (pict-name wind))
  (start-timing (timer wind))
  (set-cursor *arrow-cursor*)
  (mapc #'view-draw-contents (subviews wind))
  (view-draw-contents wind)
  )

(defmethod track-x-min ((wind phaser-window))
  (- (point-h (trgt-cntr wind)) (round (track-width wind) 2)))

(defmethod track-x-max ((wind phaser-window))
  (+ (point-h (trgt-cntr wind)) (round (track-width wind) 2)))

(defmethod track-y-min ((wind phaser-window))
  (- (point-v (trgt-cntr wind)) (round (track-width wind) 2)))

(defmethod track-y-max ((wind phaser-window))
  (+ (point-v (trgt-cntr wind)) (round (track-width wind) 2)))



(defmethod reset-display ((wind phaser-window))
  (set-indicator (view-named :SLIDER wind) nil)
  (check-box-uncheck (view-named :FOCUS-SET wind))
  (check-box-uncheck (view-named :POWER-CONNECTED wind))
  (setf (thermometer-value (view-named :THERMO wind)) 0)
  (radio-button-push (view-named :BATTERY wind))
  (status wind ""))

(defmethod check-state-update ((wind phaser-window) state)
  (case state
    (:POWER-CONNECTED (status wind "") (view-draw-contents wind))
    (:CHARGE (setf (charge-p wind) t))
    (:STOP-CHARGING (setf (charge-p wind) nil))
    (:TRACKING (if (= (state-num wind) 8)
                 (tracking-on wind)
                 (tracking-off wind)))
    (:MAIN-CONTROL (finish-task wind))
    ))


(defmethod view-key-event-handler ((wind phaser-window) key)
  (if (not (track-p wind))
    (play-snd *sp* "Warning" t)
    (let (disp)
      (case key
        (#\space (fire-phaser wind))
        ((#\UpArrow #\8) (setf disp (make-point 0 (key-move wind))))
        ((#\DownArrow #\2) (setf disp (make-point 0 (- (key-move wind)))))
        ((#\BackArrow #\4) (setf disp (make-point (key-move wind) 0)))
        ((#\ForwardArrow #\6) (setf disp (make-point (- (key-move wind)) 0))))
      (when disp
        (let ((x (+ (point-h (view-position (trgt-v wind))) (point-h disp)))
              (y (+ (point-v (view-position (trgt-v wind))) (point-v disp))))
          (setf x (bounds x (track-x-min wind) (track-x-max wind)))
          (setf y (bounds y (track-y-min wind) (track-y-max wind)))
          (set-view-position (trgt-v wind) x y)                           
          (setf (no-move wind) t))
        ))))


(defmethod tracking-on ((wind phaser-window))
  (setf (track-p wind) t)
  (set-view-position (trgt-v wind) 
                     (+ (track-x-min wind) (random (track-width wind))) 
                     (+ (track-y-min wind) (random (track-width wind))))
  (add-subviews wind (trgt-v wind)))


(defmethod tracking-off ((wind phaser-window))
  (unless (postcomp-p wind)
    (shot-feedback wind)))


(defmethod fire-phaser ((wind phaser-window))
  (play-snd *sp* "Phaser" t)
  (state-check wind :SHOOT)
  (setf (track-p wind) nil)
  (remove-subviews wind (trgt-v wind))
  (if
    (check-hit wind (subtract-points (view-position (trgt-v wind))
                                     (trgt-cntr wind)))
    (hit-target wind)
    (setf (last-hit-p wind) nil))
  (reset-display wind)
  (when (postcomp-p wind)
    (shot-feedback wind)))


(defmethod check-hit ((wind phaser-window) disp)
  (let ((hit-test (random 100))
        (dist (sqrt (+ (expt (point-h disp) 2) (expt (point-v disp) 2))))
        (hit-prob nil)
        )
    ;    (print dist)
    (when (zerop hit-test) (return-from check-hit t))
    (when (> dist 70) (return-from check-hit nil))
    (setf hit-prob (* (dist-to-prob dist) (+ 1 (/ (focus-val wind) 20))))
    ;    (print hit-prob)
    (<= hit-test hit-prob)))


(defun dist-to-prob (dist)
  (min 85 (- 95 dist)))


(defmethod focus-val ((wind phaser-window))
  (let ((sld (view-named :SLIDER wind)))
    (float (* 2 (/ (indicator sld) (point-h (view-size sld)))))))


(defmethod status ((wind phaser-window) (txt string))
  (set-dialog-item-text (view-named :STATUS wind) txt))


(defmethod hit-target ((wind phaser-window))
  (incf (num-hits wind))
  (setf (last-hit-p wind) t)
  (when (<= (random 100)
            (- (+ 60 (* 20 (num-hits wind))) (+ (/ (focus-val wind) 20))))
    (setf (num-hits wind) -1)))

  
(defmethod shot-feedback ((wind phaser-window))
  (cond ((= (num-hits wind) -1)
         (status wind "Romulan vessel destroyed.")
         (play-snd *sp* "explosion")
         (play-snd *sp* "explosion" t)
         )
        ((last-hit-p wind)
         (play-snd *sp* "explosion" t)
         (status wind "Romulan vessel hit but not destroyed.")
         (setf (track-p wind) nil)
         (setf (state-num wind) 0)
         (setf (advance-p wind) nil))
        (t
         (play-snd *sp* "Whip")
         (setf (track-p wind) nil)
         (status wind "Phaser missed Romulan vessel.")
         (setf (state-num wind) 0)
         (setf (advance-p wind) nil)
         )))


(defmethod move-target ((wind phaser-window) (max-dist fixnum))
  (if (no-move wind)
    (setf (no-move wind) nil)
    (let ((x (point-h (view-position (trgt-v wind))))
          (y (point-v (view-position (trgt-v wind))))
          (move-dist (random max-dist))
          (move-dir (last-dir wind))
          (move-change (random 7))
          )
      (when (= 1 move-change) (incf move-dir))
      (when (= 2 move-change) (decf move-dir))
      (when (> move-dir 7) (setf move-dir 0))
      (when (< move-dir 0) (setf move-dir 7))
      (when (or (= move-dir 1) (= move-dir 2) (= move-dir 3))
        (incf x move-dist))
      (when (or (= move-dir 5) (= move-dir 6) (= move-dir 7))
        (decf x move-dist))
      (when (or (= move-dir 7) (= move-dir 0) (= move-dir 1))
        (decf y move-dist))
      (when (or (= move-dir 5) (= move-dir 4) (= move-dir 3))
        (incf y move-dist))
      (setf x (bounds x (track-x-min wind) (track-x-max wind)))
      (setf y (bounds y (track-y-min wind) (track-y-max wind)))
      (setf (last-dir wind) move-dir)
      (set-view-position (trgt-v wind) (make-point x y)))))


(defmethod window-null-event-handler ((wind phaser-window))
  (unless *actr-enabled-p*
    (when (charge-p wind)
      (let* ((thermo (view-named :thermo wind)))
        (setf (thermometer-value thermo) (1+ (thermometer-value thermo)))))
    (move-target wind (idle-move wind))
    (when (and (not (training-p wind))
               (not (= (elapsed-secs wind)
                       (read-from-string 
                        (dialog-item-text (view-named :TIME wind))))))
      (set-dialog-item-text (view-named :TIME wind)
                            (mkstr (elapsed-secs wind))))
    (check-wm wind)))


;;;; ---------------------------------------------------------------------- ;;;;
;;;;
;;;; Transporter subclasses, main class, methods, and utility functions
;;;;
;;;; ---------------------------------------------------------------------- ;;;;

;;;; ---------------------------------------------------------------------- ;;;;
;;;; misc transporter subviews

;;;; actual target view class

(defclass transp-target (icon-dialog-item)
  ()
  (:default-initargs
    :view-size #@(33 33)
    :view-position #@(400 100)
    :icon 501
    :view-nick-name :shoot
    ))

(defmethod view-click-event-handler :after ((self transp-target) where)
  (declare (ignore where))
  (handle-click self))

;;;; simple oval

(defclass simple-oval (simple-view)
  ((fill-color :accessor fill-color :initarg :fill-color 
               :initform *black-color*)))


(defmethod view-draw-contents ((self simple-oval))
  (let ((dim (view-size self)))
    (with-fore-color (fill-color self)
      (fill-oval self *black-pattern* 0 0 (point-h dim) (point-v dim)))))


(defclass target-sv (simple-view)
  ()
  )


(defmethod view-draw-contents ((self target-sv))
  (let ((dim (view-size self)))
    (fill-rect self *black-pattern* 0 0 (point-h dim) (point-v dim))))


;;;; ---------------------------------------------------------------------- ;;;;
;;;;  The transporter class
;;;; ---------------------------------------------------------------------- ;;;;

(defclass transporter-window (procedure-window)
  ((scan-p :accessor scan-p :initarg :scan-p :initform nil)
   (track-p :accessor track-p :initarg :track-p :initform nil)
   (tcondition :accessor tcondition :initarg :tcondition :initform 0)
   (postcomp-p :accessor postcomp-p :initarg :postcomp-p :initform nil)
   (scan-rate :accessor scan-rate :initarg :scan-rate :initform 200)
   (last-scan :accessor last-scan :initarg :last-scan 
              :initform (get-internal-real-time))
   (scan-count :accessor scan-count :initarg :scan-count :initform 4)
   (trgt-v :accessor trgt-v :initarg :trgt-v 
           :initform  (make-instance 'transp-target))
   (no-move :accessor no-move :initarg :no-move :initform nil)
   (idle-move :accessor idle-move :initarg :idle-move 
              :initform *transporter-speed*)
   (last-dir :accessor last-dir :initarg :last-dir :initform (random 8))
   (mycurs :accessor mycurs :initarg :mycurs :initform (#_getcursor 501))
   (last-hit-p :accessor last-hit-p :initarg :last-hit-p :initform nil)
   (dots-center :accessor dots-center :initarg :dots-center 
                :initform #@(755 176))
   (key-state :accessor key-state :initarg :key-state 
              :initform :accept-frequency)
   (x1-bound :accessor x1-bound :initarg :x1-bound :initform 117)
   (x2-bound :accessor x2-bound :initarg :x2-bound :initform 412)
   (y1-bound :accessor y1-bound :initarg :y1-bound :initform 352)
   (y2-bound :accessor y2-bound :initarg :y2-bound :initform 648)
   (scan1-pos :accessor scan1-pos :initarg :scan1-pos :initform #@(680 101))
   (scan2-pos :accessor scan2-pos :initarg :scan2-pos :initform #@(824 101))
   (scan3-pos :accessor scan3-pos :initarg :scan3-pos :initform #@(680 245))
   (scan4-pos :accessor scan4-pos :initarg :scan4-pos :initform #@(824 245))
   )
  (:default-initargs
    :pict-name "transporter"
    :window-title "Transporter"
    :state-vec #(:scanner-on :active-scan :lock-signal :scanner-off
                             :enter-frequency :type :accept-frequency
                             :transporter-power :synchronous-mode
                             :shoot :synchronous-mode :main-control)
    :gui-vec #((radio-button-push . :scanner-on) 
               (radio-button-push . :active-scan)
               (radio-button-push . :lock-signal)
               (radio-button-push . :scanner-off) nil nil 
               (check-box-check . :accept-frequency) 
               (check-box-check . :transporter-power) nil nil nil nil)
    :view-font '("Avant Garde" 14 :srcor :plain (:color-index 0))
    :task-id 100
    :view-subviews
    (list
     (make-dialog-item
      'button-checker
      #@(185 137)
      #@(150 28)
      "Synchronous Mode"
      'nil
      :view-nick-name :SYNCHRONOUS-MODE
      :view-font '("Avant Garde" 14 :srccopy :plain (:color-index 0))
      :default-button nil)
     (make-dialog-item
      'button-checker
      #@(448 136)
      #@(134 28)
      "Enter Frequency"
      'nil
      :view-nick-name :ENTER-FREQUENCY
      :view-font '("Avant Garde" 14 :srccopy :plain (:color-index 0))
      :default-button nil)
     (make-dialog-item
      'button-checker
      #@(531 623)
      #@(134 28)
      "Main Control"
      'nil
      :view-nick-name :MAIN-CONTROL
      :view-font '("Avant Garde" 14 :srccopy :plain (:color-index 0))
      :default-button nil)
     (make-dialog-item
      'radio-checker
      #@(703 281)
      #@(117 16)
      "Scanner Off"
      'nil
      :view-nick-name :SCANNER-OFF
      :view-font '("Avant Garde" 14 :srcor :plain (:color-index 0)))
     (make-dialog-item
      'radio-checker
      #@(703 324)
      #@(117 16)
      "Lock Signal"
      'nil
      :view-nick-name :LOCK-SIGNAL
      :radio-button-cluster 1
      :view-font '("Avant Garde" 14 :srcor :plain (:color-index 0)))
     (make-dialog-item
      'radio-checker
      #@(703 260)
      #@(117 16)
      "Scanner On"
      'nil
      :view-nick-name :SCANNER-ON
      :view-font '("Avant Garde" 14 :srcor :plain (:color-index 0)))
     (make-dialog-item
      'radio-checker
      #@(703 302)
      #@(117 16)
      "Active Scan"
      'nil
      :view-nick-name :ACTIVE-SCAN
      :radio-button-cluster 1
      :view-font '("Avant Garde" 14 :srcor :plain (:color-index 0)))
     (make-dialog-item
      'static-text-dialog-item
      #@(529 458)
      #@(274 37)
      ""
      'nil
      :view-nick-name :STATUS
      :view-font '("Avant Garde" 14 :srcor :plain (:color-index 0)))
     (make-dialog-item
      'static-text-dialog-item
      #@(814 635)
      #@(32 15)
      "0"
      'nil
      :view-nick-name :TIME
      :view-font '("Avant Garde" 14 :srcor :plain (:color-index 0)))
     (make-dialog-item
      'editable-text-dialog-item
      #@(497 182)
      #@(35 15)
      "0"
      'nil
      :view-nick-name :FREQUENCY
      :view-font '("Avant Garde" 14 :srccopy :plain (:color-index 0))
      :allow-returns nil
      :draw-outline t)
     (make-dialog-item
      'check-box-checker
      #@(439 224)
      #@(151 16)
      "Accept Frequency"
      'nil
      :view-nick-name :ACCEPT-FREQUENCY
      :view-font '("Avant Garde" 14 :srcor :plain (:color-index 0)))
     (make-dialog-item
      'check-box-checker
      #@(189 223)
      #@(151 16)
      "Transporter Power"
      'nil
      :view-nick-name :TRANSPORTER-POWER
      :view-font '("Avant Garde" 14 :srcor :plain (:color-index 0)))      
     )))


;;; init code

(defmethod initialize-instance :after ((wind transporter-window) &key)
  (set-view-pict wind (pict-name wind))
  (set-cursor *arrow-cursor*)
  (add-markers wind)
  (start-timing (timer wind))
  (dialog-item-disable (view-named :frequency wind))
  (mapc #'view-draw-contents (subviews wind))
  )

;; Niftier version that gets scan dots' positions from the transporter-window
(defmethod add-markers ((wind transporter-window))
  (add-subviews wind
    (make-instance 'target-sv
      :view-size #@(5 5)
      :view-position (scan1-pos wind) ;; #@(680 101)
      :view-nick-name :scan1
      )
    (make-instance 'target-sv
      :view-size #@(5 5)
      :view-position (scan2-pos wind) ;; #@(824 101)
      :view-nick-name :scan2
      )
    (make-instance 'target-sv
      :view-size #@(5 5)
      :view-position (scan3-pos wind) ;; #@(680 245)
      :view-nick-name :scan3
      )
    (make-instance 'target-sv
      :view-size #@(5 5)
      :view-position (scan4-pos wind) ;; #@(824 245)
      :view-nick-name :scan4
      )))


;;;  event handling

(defmethod window-update-cursor :around ((wind transporter-window) where)
  (declare (ignore where))
  (when (track-p wind) (set-cursor (#_getcursor 501))))
    

(defmethod view-click-event-handler :after ((wind transporter-window) where)
  (declare (ignore where))
  (when (and (< (state-num wind) 10) (eq (curr-state wind) :shoot))
    (play-snd *sp* "whit" t)))


(defmethod view-key-event-handler :around ((wind transporter-window) key)
  (when (eq (curr-state wind) (key-state wind))
    (decf (state-num wind)))
  (when (eq (curr-state wind) :type)
    (call-next-method))
  (state-check wind :type key))


;;; idle time code

(defmethod window-null-event-handler ((wind transporter-window))
  (when (scan-p wind) (check-scan wind))
  (when (track-p wind) 
    (set-cursor (#_getcursor 501))
    (move-target wind (idle-move wind)))
  (when (and (not (training-p wind))
             (not (= (elapsed-secs wind)
                     (read-from-string 
                      (dialog-item-text (view-named :time wind))))))
    (set-dialog-item-text (view-named :time wind)
                          (mkstr (elapsed-secs wind))))
  (check-wm wind))



(defmethod check-scan ((wind transporter-window))
  (when (and (< (scan-rate wind)
                (- (get-internal-real-time) (last-scan wind))))
    (setf (last-scan wind) (get-internal-real-time))
    (when (and (= (random 3) 1)
               (> (scan-count wind) 1))
      (remove-subviews wind (scan-marker wind (scan-count wind)))
      (decf (scan-count wind)))
    (let ((width (second (assoc (scan-count wind)
                                 '((1 36) (2 50) (3 82) (4 144))))))
      (dotimes (i (scan-count wind))
        (set-view-position (scan-marker wind (1+ i))
                           (+ (point-h (dots-center wind))
                                       (- (/ width 2)) (random width))
                           (+ (point-v (dots-center wind)) 
                              (- (/ width 2)) (random width)))))))


(defmethod move-target ((wind transporter-window) (max-dist fixnum))
  (if (no-move wind)
    (setf (no-move wind) nil)
    (let ((x (point-h (view-position (trgt-v wind))))
          (y (point-v (view-position (trgt-v wind))))
          (move-dist (random max-dist))
          (move-dir (last-dir wind))
          (move-change (random 7))
          (x1-bound (x1-bound wind))
          (x2-bound (x2-bound wind))
          (y1-bound (y1-bound wind))
          (y2-bound (y2-bound wind))
          )
      (when (= 1 move-change) (incf move-dir))
      (when (= 2 move-change) (decf move-dir))
      (when (> move-dir 7) (setf move-dir 0))
      (when (< move-dir 0) (setf move-dir 7))
      (when (or (= move-dir 1) (= move-dir 2) (= move-dir 3))
        (incf x move-dist))
      (when (or (= move-dir 5) (= move-dir 6) (= move-dir 7))
        (decf x move-dist))
      (when (or (= move-dir 7) (= move-dir 0) (= move-dir 1))
        (decf y move-dist))
      (when (or (= move-dir 5) (= move-dir 4) (= move-dir 3))
        (incf y move-dist))
      (setf x (bounds x x1-bound x2-bound))
      (setf y (bounds y y1-bound y2-bound))
      (when (or (= x x1-bound) (= x x2-bound) (= y y1-bound) (= y y2-bound))
        (incf move-dir 4)
        (when (> move-dir 7) (setf move-dir (- move-dir 8)))) 
      (setf (last-dir wind) move-dir) 
      (set-view-position (trgt-v wind) x y))))



;;; state change handling

(defmethod check-state-update ((wind transporter-window) state)
  ;(print state)
  (case state
    (:scanner-on (status wind ""))
    (:active-scan (setf (scan-p wind) t))
    (:lock-signal (setf (scan-p wind) nil))
    (:enter-frequency (activate-freq wind))
    (:synchronous-mode (if (eq (state-num wind) 8)
                         (begin-tracking wind)
                         (unless (postcomp-p wind) (give-feedback wind))))
    (:accept-frequency (check-freq wind))
    (:shoot (cease-tracking wind))
    (:main-control (finish-task wind))
    ))


(defmethod activate-freq ((wind transporter-window))
  (let ((ted (view-named :frequency wind)))
    (dialog-item-enable ted)
    (set-selection-range ted 0 1)))

#|
(inspect *exp*)
(setf wind (task-window *experiment*))
(view-named :frequency *wind*)
(view-named :calibration *wind*)
(inspect *)
(dialog-item-disable *)
(dialog-item-enable *)
(curr-state *wind*)
|#

(defmethod check-freq ((wind transporter-window))
  (let ((freq (freq-val wind)))
    (if (or (not (numberp freq))
            (> freq 100)
            (< freq 1))
      (progn
        (play-snd *sp* "Warning" t)
        (check-box-uncheck (view-named :accept-frequency wind))
        (decf (state-num wind)))
      (dialog-item-disable (view-named :frequency wind)))))


(defmethod begin-tracking ((wind transporter-window))
  (setf (track-p wind) t)
  (set-cursor (#_getcursor 501))
  (set-view-position (trgt-v wind) 
                     (+ (x1-bound wind) (random 295)) 
                     (+ (y1-bound wind) (random 296)))
  (add-subviews wind (trgt-v wind)))



(defmethod cease-tracking ((wind transporter-window))
  (remove-subviews wind (trgt-v wind))
  (setf (track-p wind) nil)
  (play-snd *sp* "Transporter TNG" t)
  (set-cursor *arrow-cursor*)
  (determine-hit wind)
  (when (postcomp-p wind) 
    (give-feedback wind)
    (reset-display wind))
  )


(defmethod give-feedback ((wind transporter-window))
  (cond ((last-hit-p wind)
         (play-snd *sp* "yes" t)
         (status wind "Beam successful--return to main control.")
         )
        (t
         (play-snd *sp* "damn" t)
         (status wind (if (flip)
                        "Beam failed--jammed by hostile signal."
                        "Beam failed--beam too weak."))
         (setf (state-num wind) 0)
         (setf (advance-p wind) nil)))
  (unless (postcomp-p wind)
    (reset-display wind)))
         


(defmethod determine-hit ((wind transporter-window))
  (let ((dist (abs (- (freq-val wind) (random 100)))))
    (cond ((> 1 (scan-count wind)) (setf (last-hit-p wind) nil))
          ((< dist 41) (setf (last-hit-p wind) t))
          ((< dist 90) (setf (last-hit-p wind) nil))
          (t (setf (last-hit-p wind) (flip))))))


;;; utils

(defmethod reset-display ((wind transporter-window))
  (check-box-uncheck (view-named :accept-frequency wind))
  (check-box-uncheck (view-named :transporter-power wind))
  (radio-button-push (view-named :scanner-off wind))
  (set-dialog-item-text (view-named :frequency wind) "0")
  (dotimes (i 4)
    (awhen (scan-marker wind (1+ i))
      (remove-subviews wind it)))
  (add-markers wind)
  (setf (scan-count wind) 4)
  )


(defmethod freq-val ((wind transporter-window))
  (read-from-string (dialog-item-text (view-named :frequency wind))))


(defmethod status ((wind transporter-window) (txt string))
  (set-dialog-item-text (view-named :status wind) txt))


(defmethod scan-marker ((wind transporter-window) (num fixnum))
  (view-named (read-from-string (mkstr ":SCAN" num)) wind))





;;;; ---------------------------------------------------------------------- ;;;;
;;;; torpedo-window subclass of the phaser-window



(defclass torpedo-window (phaser-window)
  
()

  (:default-initargs
    :task-id 300
    :pict-name "phaser-extrabtn"
    :window-title "Torpedo Subgoal Shuffling"
    :state-vec #(:DISPERSION :DISPERSION-SLIDER :DISPERSION-SET 
                 :BAY-ACTIVATED :LOAD :STOP-LOADING :BAY-ACTIVATED 
                                  :FIRING :LOCKING :SHOOT :LOCKING 
                                  :MAIN-CONTROL)
    :gui-vec #((radio-button-push . :DISPERSION) nil (check-box-check . :DISPERSION-SET)
               (check-box-check . :BAY-ACTIVATED)
               nil nil (check-box-uncheck . :BAY-ACTIVATED) 
               (radio-button-push . :FIRING) nil nil nil nil)
    :trgt-cntr #@(281 253)
    :view-subviews
    (list 
     ;; tracking cluster
     (MAKE-DIALOG-ITEM
      'button-checker
      #@(150 410)
      #@(110 30)
      "Tracking"
      nil
      :VIEW-NICK-NAME :TRACKING
      :VIEW-FONT '("Avant Garde" 12 :SRCCOPY :PLAIN (:COLOR-INDEX 0))
      :DEFAULT-BUTTON NIL)
     (MAKE-DIALOG-ITEM
      'button-checker
      #@(300 410)
      #@(110 30)
      "Locking"
      nil
      :VIEW-NICK-NAME :LOCKING
      :VIEW-FONT '("Avant Garde" 12 :SRCCOPY :PLAIN (:COLOR-INDEX 0))
      :DEFAULT-BUTTON NIL)
     
     ;; power cluster
     (make-dialog-item
      'button-checker
      #@(538 139)
      #@(120 34)
      "Charge"
      nil
      :VIEW-NICK-NAME :CHARGE
      :VIEW-FONT '("Avant Garde" 12 :SRCCOPY :PLAIN (:COLOR-INDEX 0))
      :DEFAULT-BUTTON NIL)
     (MAKE-DIALOG-ITEM
      'button-checker
      #@(538 274)
      #@(120 34)
      "Stop Charging"
      nil
      :VIEW-NICK-NAME :STOP-CHARGING
      :VIEW-FONT '("Avant Garde" 12 :SRCCOPY :PLAIN (:COLOR-INDEX 0))
      :DEFAULT-BUTTON NIL)
     (make-instance 'thermometer 
       :view-position #@(736 133)
       :view-size #@(25 174)
       :view-nick-name :THERMO
       :pattern *light-gray-pattern*)
     (make-dialog-item
      'button-checker
      #@(538 184)
      #@(120 34)
      "Load"
      nil
      :VIEW-NICK-NAME :LOAD
      :VIEW-FONT '("Avant Garde" 12 :SRCCOPY :PLAIN (:COLOR-INDEX 0))
      :DEFAULT-BUTTON NIL)
     (make-dialog-item
      'button-checker
      #@(538 229)
      #@(120 34)
      "Stop Loading"
      nil
      :VIEW-NICK-NAME :STOP-LOADING
      :VIEW-FONT '("Avant Garde" 12 :SRCCOPY :PLAIN (:COLOR-INDEX 0))
      :DEFAULT-BUTTON NIL)
     
     ;; central button cluster
     (MAKE-DIALOG-ITEM
      'radio-checker
      #@(513 354)
      #@(70 15)
      "Battery"
      nil
      :VIEW-NICK-NAME :BATTERY
      :VIEW-FONT '("Avant Garde" 12 :SRCCOPY :PLAIN (:COLOR-INDEX 0)))
     (MAKE-DIALOG-ITEM
      'radio-checker
      #@(513 374)
      #@(70 16)
      "Firing"
      nil
      :VIEW-NICK-NAME :FIRING
      :VIEW-FONT '("Avant Garde" 12 :SRCCOPY :PLAIN (:COLOR-INDEX 0)))
     (MAKE-DIALOG-ITEM
      'radio-checker
      #@(513 414)
      #@(70 16)
      "Settings"
      nil
      :VIEW-NICK-NAME :SETTINGS
      :VIEW-FONT '("Avant Garde" 12 :SRCCOPY :PLAIN (:COLOR-INDEX 0)))
     (MAKE-DIALOG-ITEM
      'check-box-checker
      #@(643 414)
      #@(136 16)
      "Focus Set"
      nil
      :VIEW-NICK-NAME :FOCUS-SET
      :VIEW-FONT '("Avant Garde" 12 :SRCCOPY :PLAIN (:COLOR-INDEX 0)))
     (MAKE-DIALOG-ITEM
      'check-box-checker
      #@(643 354)
      #@(136 16)
      "Power Connected"
      nil
      :VIEW-NICK-NAME :POWER-CONNECTED
      :VIEW-FONT '("Avant Garde" 12 :SRCCOPY :PLAIN (:COLOR-INDEX 0)))
     (MAKE-DIALOG-ITEM
      'check-box-checker
      #@(643 394)
      #@(136 16)
      "Bay Activated"
      nil
      :VIEW-NICK-NAME :BAY-ACTIVATED
      :VIEW-FONT '("Avant Garde" 12 :SRCCOPY :PLAIN (:COLOR-INDEX 0)))
     (MAKE-DIALOG-ITEM
      'radio-checker
      #@(513 434)
      #@(130 16)
      "Dispersion"
      nil
      :VIEW-NICK-NAME :DISPERSION
      :VIEW-FONT '("Avant Garde" 12 :SRCCOPY :PLAIN (:COLOR-INDEX 0)))
     (MAKE-DIALOG-ITEM
      'check-box-checker
      #@(643 374)
      #@(136 16)
      "Dispersion Set"
      nil
      :VIEW-NICK-NAME :DISPERSION-SET
      :VIEW-FONT '("Avant Garde" 12 :SRCCOPY :PLAIN (:COLOR-INDEX 0)))
     (MAKE-DIALOG-ITEM
      'radio-checker
      #@(513 394)
      #@(70 16)
      "Scope"
      nil
      :VIEW-NICK-NAME :SCOPE
      :VIEW-FONT '("Avant Garde" 12 :SRCCOPY :PLAIN (:COLOR-INDEX 0)))
     
     ;; main control
     (MAKE-DIALOG-ITEM
      'button-checker
      #@(442 616)
      #@(108 34)
      "Main Control"
      nil
      :VIEW-NICK-NAME :MAIN-CONTROL
      :VIEW-FONT '("Avant Garde" 12 :SRCCOPY :PLAIN (:COLOR-INDEX 0))
      :DEFAULT-BUTTON NIL)
     
     ;; focus
     (make-instance 'click-indicator :view-position #@(164 562)
                    :view-size #@(240 44) :v-offset 24
                    :view-nick-name :DISPERSION-SLIDER)
     
     ;; elapsed time
     (MAKE-DIALOG-ITEM
      'STATIC-TEXT-DIALOG-ITEM
      #@(743 638)
      #@(54 14)
      "0"
      'NIL
      :VIEW-NICK-NAME :TIME
      :VIEW-FONT
      '("Avant Garde" 14 :SRCOR :PLAIN (:COLOR-INDEX 0)))
     
     ;; status
     (MAKE-DIALOG-ITEM
      'STATIC-TEXT-DIALOG-ITEM
      #@(135 645)
      #@(273 38)
      ""
      'NIL
      :VIEW-NICK-NAME :STATUS
      :VIEW-FONT '("Avant Garde" 14 :SRCOR :PLAIN (:COLOR-INDEX 0)))
     )
    ))

(defmethod reset-display ((wind torpedo-window))
  (set-indicator (view-named :DISPERSION-SLIDER wind) nil)
  (check-box-uncheck (view-named :DISPERSION-SET wind))
  (check-box-uncheck (view-named :BAY-ACTIVATED wind))
  (setf (thermometer-value (view-named :THERMO wind)) 0)
  (radio-button-push (view-named :BATTERY wind))
  (status wind ""))

(defmethod check-state-update ((wind torpedo-window) state)
  (case state
    (:BAY-ACTIVATED (status wind "") (view-draw-contents wind))
    (:LOAD (setf (charge-p wind) t))
    (:STOP-LOADING (setf (charge-p wind) nil))
    (:LOCKING (if (= (state-num wind) 8)
                 (tracking-on wind)
                 (tracking-off wind)))
    (:MAIN-CONTROL (finish-task wind))
    ))

(defmethod shot-feedback ((wind torpedo-window))
  (cond ((= (num-hits wind) -1)
         (status wind "Romulan vessel destroyed.")
         (play-snd *sp* "explosion")
         (play-snd *sp* "explosion" t)
         )
        ((last-hit-p wind)
         (play-snd *sp* "explosion" t)
         (status wind "Romulan vessel hit but not destroyed.")
         (setf (track-p wind) nil)
         (setf (state-num wind) 0)
         (setf (advance-p wind) nil))
        (t
         (play-snd *sp* "Whip")
         (setf (track-p wind) nil)
         (status wind "Torpedo missed Romulan vessel.")
         (setf (state-num wind) 0)
         (setf (advance-p wind) nil)
         )))

(defmethod fire-phaser ((wind torpedo-window))
  (play-snd *sp* "torpedo" t)
  (state-check wind :SHOOT)
  (setf (track-p wind) nil)
  (remove-subviews wind (trgt-v wind))
  (if
    (check-hit wind (subtract-points (view-position (trgt-v wind))
                                     (trgt-cntr wind)))
    (hit-target wind)
    (setf (last-hit-p wind) nil))
  (reset-display wind)
  (when (postcomp-p wind)
    (shot-feedback wind)))


(defmethod focus-val ((wind torpedo-window))
  (let ((sld (view-named :DISPERSION-SLIDER wind)))
    (float (* 2 (/ (indicator sld) (point-h (view-size sld)))))))



;;;; ---------------------------------------------------------------------- ;;;;
;;;; jammer-window subclass of the transporter-window




(defclass jammer-window (transporter-window)
  ()
  (:default-initargs
    :window-title "Jammer"
    :state-vec #(:enter-calibration 
                 :type :accept-calibration
                 :scanner-on :active-scan :lock-signal :scanner-off
                 :transporter-power :desynchronize
                 :shoot :desynchronize :main-control)
    :gui-vec #(nil nil (check-box-check . :accept-calibration)
                   (radio-button-push . :scanner-on) 
                   (radio-button-push . :active-scan)
                   (radio-button-push . :lock-signal)
                   (radio-button-push . :scanner-off)                    
                   (check-box-check . :transporter-power) nil nil nil nil)
    :pict-name "transporter-extra-buttons"
    :key-state :accept-calibration
    :dots-center #@(752 173)
    ;; x & y bounds for the target
    :x1-bound 120
    :x2-bound 412
    :y1-bound 352
    :y2-bound 645
    :scan1-pos #@(-10 -10)
    :scan2-pos #@(-10 -10)
    :scan3-pos #@(-10 -10)
    :scan4-pos #@(-10 -10)
    :task-id 400
    :view-subviews
    (list
     ;; left button grouping
     (make-dialog-item
      'button-checker
      #@(185 107)
      #@(150 28)
      "Desynchronize"
      'nil
      :view-nick-name :DESYNCHRONIZE
      :view-font '("Avant Garde" 14 :srccopy :plain (:color-index 0))
      :default-button nil)
     (make-dialog-item
      'button-checker
      #@(185 145)
      #@(150 28)
      "Synchronous Mode"
      'nil
      :view-nick-name :SYNCHRONOUS-MODE
      :view-font '("Avant Garde" 14 :srccopy :plain (:color-index 0))
      :default-button nil)
     (make-dialog-item
      'check-box-checker
      #@(189 223)
      #@(151 16)
      "Transporter Power"
      'nil
      :view-nick-name :TRANSPORTER-POWER
      :view-font '("Avant Garde" 14 :srcor :plain (:color-index 0)))      
     (make-dialog-item
      'check-box-checker
      #@(189 244)
      #@(151 16)
      "Diagnostic Z"
      'nil
      :view-nick-name :DIAGNOSTIC-Z
      :view-font '("Avant Garde" 14 :srcor :plain (:color-index 0)))      
     (make-dialog-item
      'check-box-checker
      #@(189 265)
      #@(151 16)
      "Auxiliary Power"
      'nil
      :view-nick-name :AUXILIARY-POWER
      :view-font '("Avant Garde" 14 :srcor :plain (:color-index 0)))      
     
     ;; Middle button group
     (make-dialog-item
      'button-checker
      #@(440 107)
      #@(150 28)
      "Calibration"
      'nil
      :view-nick-name :ENTER-CALIBRATION
      :view-font '("Avant Garde" 14 :srccopy :plain (:color-index 0))
      :default-button nil)
     (make-dialog-item
      'button-checker
      #@(440 145)
      #@(150 28)
      "Enter Frequency"
      'nil
      :view-nick-name :ENTER-FREQUENCY
      :view-font '("Avant Garde" 14 :srccopy :plain (:color-index 0))
      :default-button nil)
     (make-dialog-item
      'editable-text-dialog-item
      #@(525 188)
      #@(35 15)
      "0"
      'nil
      :view-nick-name :CALIBRATION
      :view-font '("Avant Garde" 14 :srccopy :plain (:color-index 0))
      :allow-returns nil
      :draw-outline t :enabled-p nil)
     (make-dialog-item
      'editable-text-dialog-item
      #@(463 188)
      #@(35 15)
      "0"
      'nil
      :view-nick-name :FREQUENCY
      :view-font '("Avant Garde" 14 :srccopy :plain (:color-index 0))
      :allow-returns nil
      :draw-outline t :enabled-p nil)
     (make-dialog-item
      'check-box-checker
      #@(439 224)
      #@(151 16)
      "Accept Frequency"
      'nil
      :view-nick-name :ACCEPT-FREQUENCY
      :view-font '("Avant Garde" 14 :srcor :plain (:color-index 0)))
     (make-dialog-item
      'check-box-checker
      #@(439 245)
      #@(151 16)
      "Accept Calibration"
      'nil
      :view-nick-name :ACCEPT-CALIBRATION
      :view-font '("Avant Garde" 14 :srcor :plain (:color-index 0)))
     (make-dialog-item
      'check-box-checker
      #@(439 266)
      #@(151 16)
      "Frequency Sample"
      'nil
      :view-nick-name :FREQ-SAMPLE
      :view-font '("Avant Garde" 14 :srcor :plain (:color-index 0)))
     
     
     ;; main control group
     (make-dialog-item 'button-checker
                       #@(505 577) #@(108 34)
                       "Main Control" nil :view-nick-name :MAIN-CONTROL
                       :view-font '("Charcoal" 12 :srccopy :plain (:color-index 0))
                       :default-button NIL)
     (make-dialog-item 'button-checker
                       #@(625 577)
                       #@(108 34)
                       "Diagnostic" nil :view-nick-name :DIAGNOSTIC
                       :view-font '("Charcoal" 12 :srccopy :plain (:color-index 0))
                       :default-button NIL)
     (make-dialog-item 'button-checker
                       #@(505 617)
                       #@(108 34)
                       "Alpha Mask" nil :view-nick-name :ALPHA-MASK
                       :view-font '("Charcoal" 12 :srccopy :plain (:color-index 0))
                       :default-button NIL)
     (make-dialog-item 'button-checker
                       #@(625 617)
                       #@(108 34)
                       "System Lock" nil :view-nick-name :SYSTEM-LOCK
                       :view-font '("Charcoal" 12 :srccopy :plain (:color-index 0))
                       :default-button NIL)
     
     ;; right button group -- out of order to get defaults correct
     (make-dialog-item
      'radio-checker
      #@(703 281)
      #@(117 16)
      "Scanner Off"
      'nil
      :view-nick-name :SCANNER-OFF
      :view-font '("Avant Garde" 14 :srcor :plain (:color-index 0)))
     (make-dialog-item
      'radio-checker
      #@(703 260)
      #@(117 16)
      "Scanner On"
      'nil
      :view-nick-name :SCANNER-ON
      :view-font '("Avant Garde" 14 :srcor :plain (:color-index 0)))
     (make-dialog-item
      'radio-checker
      #@(703 323)
      #@(117 16)
      "Lock Signal"
      'nil
      :view-nick-name :LOCK-SIGNAL
      :radio-button-cluster 1
      :view-font '("Avant Garde" 14 :srcor :plain (:color-index 0)))
     (make-dialog-item
      'radio-checker
      #@(703 302)
      #@(117 16)
      "Active Scan"
      'nil
      :view-nick-name :ACTIVE-SCAN
      :radio-button-cluster 1
      :view-font '("Avant Garde" 14 :srcor :plain (:color-index 0)))
     
     (make-dialog-item
      'radio-checker
      #@(703 365)
      #@(117 16)
      "Tracker Off"
      'nil
      :view-nick-name :TRACKER-OFF
      :radio-button-cluster 2
      :view-font '("Avant Garde" 14 :srcor :plain (:color-index 0)))
     (make-dialog-item
      'radio-checker
      #@(703 344)
      #@(117 16)
      "Tracker On"
      'nil
      :view-nick-name :TRACKER-ON
      :radio-button-cluster 2
      :view-font '("Avant Garde" 14 :srcor :plain (:color-index 0)))
     
     (make-dialog-item
      'radio-checker
      #@(703 407)
      #@(117 16)
      "Fix Frequency"
      'nil
      :view-nick-name :FIX-FREQUENCY
      :radio-button-cluster 3
      :view-font '("Avant Garde" 14 :srcor :plain (:color-index 0)))
     (make-dialog-item
      'radio-checker
      #@(703 386)
      #@(117 16)
      "Modulator"
      'nil
      :view-nick-name :MODULATOR
      :radio-button-cluster 3
      :view-font '("Avant Garde" 14 :srcor :plain (:color-index 0)))
     
     
     (make-dialog-item
      'static-text-dialog-item
      #@(529 492)
      #@(274 37)
      ""
      'nil
      :view-nick-name :STATUS
      :view-font '("Avant Garde" 14 :srcor :plain (:color-index 0)))
     (make-dialog-item
      'static-text-dialog-item
      #@(834 635)
      #@(32 15)
      "0"
      'nil
      :view-nick-name :TIME
      :view-font '("Avant Garde" 14 :srcor :plain (:color-index 0)))
     )))



(defmethod initialize-instance :after ((wind jammer-window) &key)
  (dialog-item-disable (view-named :calibration wind))
  )



(defmethod check-state-update ((wind jammer-window) state)
  ;(print state)
  (case state
    (:enter-calibration (progn  (status wind "") 
                                (activate-freq-2 wind)))        ; mdb change
    (:active-scan (setf (scan-p wind) t))
    (:lock-signal (setf (scan-p wind) nil))
    (:desynchronize (if (eq (state-num wind) 8)
                      (begin-tracking wind)
                      (unless (postcomp-p wind) (give-feedback wind))))
    (:accept-calibration (check-freq wind))
    (:shoot (cease-tracking wind))
    (:main-control (finish-task wind))
    ))


(defmethod activate-freq-2 ((wind jammer-window))
  (let ((ted (view-named :calibration wind)))
    (dialog-item-enable ted)
    (set-selection-range ted 0 1)))


(defmethod check-freq ((wind jammer-window))
  (let ((freq (freq-val wind)))
    (if (or (not (numberp freq))
            (> freq 100)
            (< freq 1))
      (progn
        (play-snd *sp* "Warning" t)
        (check-box-uncheck (view-named :accept-calibration wind))
        (decf (state-num wind)))
      (dialog-item-disable (view-named :calibration wind)))))


(defmethod freq-val-2 ((wind jammer-window))
  (read-from-string (dialog-item-text (view-named :calibration wind))))


(defmethod freq-val ((wind jammer-window))
  (read-from-string (dialog-item-text (view-named :calibration wind))))


(defmethod give-feedback ((wind jammer-window))
  (cond ((last-hit-p wind)
         (play-snd *sp* "yes" t)
         (status wind "Jamming successful--return to main control.")
         )
        (t
         (play-snd *sp* "damn" t)
         (status wind (if (flip)
                        "Jamming failed--hostile signal too strong."
                        "Jamming failed--beam too weak."))
         (setf (state-num wind) 0)
         (setf (advance-p wind) nil)))
  (unless (postcomp-p wind)
    (reset-display wind)))

(defmethod cease-tracking ((wind jammer-window))
  (remove-subviews wind (trgt-v wind))
  (setf (track-p wind) nil)
  (play-snd *sp* "Jammer" t)
  (set-cursor *arrow-cursor*)
  (determine-hit wind)
  (when (postcomp-p wind) 
    (give-feedback wind)
    (reset-display wind))
  )


(defmethod reset-display ((wind jammer-window))
  (check-box-uncheck (view-named :accept-calibration wind))
  (check-box-uncheck (view-named :transporter-power wind))
  (radio-button-push (view-named :scanner-off wind))
  (set-dialog-item-text (view-named :calibration wind) "0")
  (dotimes (i 4)
    (awhen (scan-marker wind (1+ i))
      (remove-subviews wind it)))
  (add-markers wind)
  (setf (scan-count wind) 4)
  )



;;;; ---------------------------------------------------------------------- ;;;;
;;;; utility functions

(defun bounds (x min max)
  (if (> x max) (setf x max))
  (if (< x min) (setf x min))
  x)


;;;; ---------------------------------------------------------------------- ;;;;
;;;;  testing code

#|
(setf *rf* (open-resource-file (choose-file-dialog :prompt "Resource file.")))
(setf *sw* (make-instance 'score-windoid))
(setf *sp* 
        (make-instance 
          'snd-player :res-file nil
          :preload '("Warning" "Whit" "Phaser" "Whip" "damn" "expolsion"
                     "Transporter TNG" "Probe" "a" "b" "c" "d" "e" "f" "g" "h" 
                     "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s" "t" "u" "v" 
                     "w" "x" "y" "z" "uh oh" "glass")))

(setf *experiment* (make-instance 'phaser-window :snd-player *sp*))
(setf *experiment* (make-instance 'transporter-window :snd-player *sp*))
|#

;;;; bookkeeping

(provide :base-trek-tasks)
