;;;  -*- mode: LISP; Package: CL-USER; Syntax: COMMON-LISP;  Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Author      : Mike Byrne
;;; Copyright   : (c)2003 Rice U./Mike Byrne, All Rights Reserved
;;; Availability: public domain
;;; Address     : Rice University
;;;             : Psychology Department
;;;             : Houston,TX 77251-1892
;;;             : byrne@acm.org
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Filename    : procedure-window2.lisp
;;; Version     : 2.0r5
;;; 
;;; Description : Code to manage windows that run procedures.
;;; 
;;; Bugs        : 
;;; 
;;; Todo        : 
;;; 
;;; ----- History -----
;;; 2006.02.22 mdb
;;;             : Now checks to make sure PROBE is not "" in DO-PROBE.  This
;;;             : really should never happen anyway, but for some reason it
;;;             : does and that screws things up.
;;; 2005.11.06 mdb
;;;             : * DELTA-SCORE now works for null arguments.
;;; 2004.11.15 mdb [r3]
;;;             : Now supports logging everything in the WM dialog:
;;;             :      -1 is when the dialog comes up
;;;             :      -2 codes each keystroke
;;;             :      -3 codes dialog close
;;;             : Now recording where the button was clicked
;;; 2004.01.13 mdb [r2]
;;;             : * Annoying problem with GUI state failing to update hopefully
;;;             :   finally fixed.  Stupid solution:  store vector of function
;;;             :   names to call on view names [GUI-CHECK function].
;;; 2003.12.11 mdb [2.0r1]
;;;             : * Changed screen size for eMacs
;;;             : * Added score windoid, WM testing, and such to base library.
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require :pict-svm)
(require :snd-player)
(require :timer)
(require :misc-lib)

(defvar *sp* nil "Holds the sound player.")
(defvar *sw* nil "Holds the score window.")
(defvar *wmt* nil "Holds the working memory tester.")
(defvar *tid* 0 "Holds the task ID.")
(defvar *experiment* nil "Holds the experiment window.")

(defparameter *step-value 25 "Value of a correct step execution.")
(defparameter *step-penalty -75 "Penalty for step error.")
(defparameter *wm-penalty -200 "Penalty for missing WM probe.")



;;;; ---------------------------------------------------------------------- ;;;;
;;;; Score windoid
;;;; ---------------------------------------------------------------------- ;;;;

(defclass score-windoid (windoid)
  ((score :accessor score :initarg :score :initform 0)
   (last-correct :accessor last-correct :initarg :last-correct :initform nil)
   )
  (:default-initargs
    :close-box-p nil
    :view-size #@(75 25)
    :view-position #@(950 743)
    :window-title "Score"
    :grow-icon-p nil
    :back-color 16776960
    :view-subviews
    (list
     (make-instance 'static-text-dialog-item
       :view-size #@(50 16)
       :dialog-item-text "0"
       :view-nick-name :TEXT
       :view-position #@(25 4)))))


(defmethod delta-score ((sw score-windoid) delta)
  (incf (score sw) delta)
  (set-dialog-item-text (view-named :TEXT sw) (mkstr (score sw)))
  (set-window-layer sw -4 t)
  (ccl::window-bring-to-front sw)
  )

(defmethod delta-score ((sw null) delta)
  (declare (ignore delta))
  nil)



;;;; ---------------------------------------------------------------------- ;;;;
;;;; WM probe class and subclass
;;;; ---------------------------------------------------------------------- ;;;;

;;;; The quick string dialog


(defclass qkstr-dialog (dialog)
  ((strlen :accessor strlen :initarg :strlen :initform 3)
   (wind :accessor wind :initarg :wind :initform nil)
   )
  (:default-initargs
    :view-size #@(250 60)
    :close-box-p nil
    :window-type :movable-dialog
    :back-color *tool-back-color*
    :window-title "Recall"
    :view-subviews
    (list
     (make-dialog-item
      'static-text-dialog-item
      #@(5 18)
      #@(54 16)
      "Type:"
      'NIL
      :view-font '("Charcoal" 12 :srccopy :plain (:color-index 0)))
     (make-dialog-item
      'editable-text-dialog-item
      #@(49 18)
      #@(81 16)
      ""
      'nil
      :view-nick-name :IN-FIELD
      :view-font '("Charcoal" 12 :srccopy :plain (:color-index 0))
      :allow-returns nil
      :draw-outline t)
     (make-dialog-item
      'button-dialog-item
      #@(177 18)
      #@(64 18)
      "OK"
      #'(lambda (item) 
          (if *modal-dialog-on-top*
            (return-from-modal-dialog 
             (dialog-item-text (view-named :IN-FIELD (view-window item))))
            (beep)))
      :view-nick-name :OK-BUTTON
      :dialog-item-enabled-p nil
      :view-font '("Charcoal" 12 :srccopy :plain (:color-index 0))
      :default-button t))))


(defmethod view-key-event-handler :after ((dlg qkstr-dialog) key)
  ;(declare (ignore key))
  (if (= (length (dialog-item-text (view-named :IN-FIELD dlg)))
         (strlen dlg))
    (dialog-item-enable (view-named :OK-BUTTON dlg))
    (dialog-item-disable (view-named :OK-BUTTON dlg)))
  (push
   (make-instance 'proc-action
     :latency  (round (start-stop-timer (timer (wind dlg))))
     :step-num -2 :expected :key :got :key :info key)
   (action-log (wind dlg)))
  )
    
(defmethod window-close :before ((dlg qkstr-dialog))
  (unless (boundp '*current-event*)
    (print "no event bound"))
  (push
   (make-instance 'proc-action
     :latency  (round (start-stop-timer (timer (wind dlg))))
     :step-num -3
     :expected :close
     :got :close)
   (action-log (wind dlg))))


(defun qkstr-dlg (wind &optional (strlen 3))
  (play-snd *sp* "Indigo")
  (spin-for (timer *experiment*) 150)
  (event-dispatch)
  (modal-dialog (make-instance 'qkstr-dialog :strlen strlen
                               :wind wind) t))



;;;; ---------------------------------------------------------------------- ;;;;
;;;; the WM tester class

(defclass wm-tester ()
  ((delay :accessor delay :initarg :delay :initform 3000)
   (snd-lst :accessor snd-lst :initarg :snd-lst 
            :initform '("a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" 
                        "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z"))
   (probe-len :accessor probe-len :initarg :probe-len :initform nil)
   (last-time :accessor last-time :initarg :last-time 
              :initform (+ (get-internal-real-time) 8000))
   (curr-stims :accessor curr-stims :initarg :curr-stims :initform nil)
   (snd-player :accessor snd-player :initarg :snd-player :initform nil)
   (results :accessor results :initarg :results :initform nil)
   (maxlen :accessor maxlen :initarg :maxlen :initform 15)
   (minlen :accessor minlen :initarg :minlen :initform 4)
   (probe-depth :accessor probe-depth :initarg :probe-depth :initform 3)
   ))


(defmethod initialize-instance :after ((wmt wm-tester) &key)
  (reset-probe wmt))

(defmethod reset-time ((wmt wm-tester))
  (setf (last-time wmt) (+ (get-internal-real-time) 4000)))
                      


(defmethod do-wm-check ((wmt wm-tester) (wind window))
  (unless (< (- (get-internal-real-time) (last-time wmt))
             (delay wmt))
    (if (= (length (curr-stims wmt)) (probe-len wmt))
      (do-probe wmt wind)
      (new-probe wmt))))

(defmethod do-wm-check ((wmt null) (wind window))
  nil
  )

(defmethod new-probe ((wmt wm-tester))
  (let ((probe (random-item (snd-lst wmt))))
    (push probe (curr-stims wmt))
    (play-snd (snd-player wmt) probe t)
    (setf (last-time wmt) (get-internal-real-time))))


(defmethod do-probe ((wmt wm-tester) (wind window))
  (let ((start (get-internal-real-time))
        (finish nil)
        (response nil) 
        (probe "")
        (correct-p nil))
    (push (make-instance 'proc-action
            :latency  (round (start-stop-timer (timer wind)))
            :step-num -1 :expected :probe :got :probe)
          (action-log wind))
    (setf response (qkstr-dlg wind (probe-depth wmt)))
    (setf finish (get-internal-real-time))
    (awhen (find-window "Recall" 'QKSTR-DIALOG)
           (window-close it))
    (without-interrupts
     (setf response (string-downcase response))
     (dolist (item (nreverse (curr-stims wmt)))
       (setf probe (concatenate 'string probe item)))
     (when (and (not (equal probe ""))
                (stringp response) (= (length response) 3))
       (if (string= response 
                    (subseq probe (- (length probe) (probe-depth wmt))))
         (setf correct-p 1)
         (setf correct-p 0))
       (when (= correct-p 0)
         (play-snd (snd-player wmt) "warning tone")
         (delta-score *sw* *wm-penalty)
         ))
     (push (list correct-p probe response finish start) (results wmt))
     (reset-probe wmt 1000)
     ;(values (- finish start) correct-p)
     )))



(defmethod reset-probe ((wmt wm-tester) &optional (delay 0))
  (setf (curr-stims wmt) nil)
  (setf (probe-len wmt) (+ (minlen wmt) (random (- (maxlen wmt) (minlen wmt)))))
  (setf (last-time wmt) (+ (get-internal-real-time) delay)))



(defmethod print-results ((wmt wm-tester) &optional (strm t))
  (dolist (result (reverse (results wmt)))
    (terpri strm)
    (tab-output result strm))) 



;;;; ---------------------------------------------------------------------- ;;;;
;;;; an action record

(defclass proc-action ()
  ((latency :accessor latency :initarg :latency :initform nil)
   (expected :accessor expected :initarg :expected :initform nil)
   (got :accessor got :initarg :got :initform nil)
   (step-num :accessor step-num :initarg :step-num :initform nil)
   (info :accessor info :initarg :info :initform nil)
   ))

(defmethod is-error ((p-act proc-action))
  (neq (expected p-act) (got p-act)))


(defmethod write-pa ((p-act proc-action) &optional (strm t))
  (let ((out-lst 
         (list 
          (if (minusp (step-num p-act)) 
            (step-num p-act) 
            (+ (step-num p-act) *tid*))
          (if (is-error p-act) 0 1)
          (latency p-act) 
          (expected p-act)
          (got p-act))))
    (terpri strm)
    (when (info p-act) (setf out-lst (append out-lst (list (info p-act)))))
    (tab-output out-lst strm)))


;;;; ---------------------------------------------------------------------- ;;;;
;;;; the procedure window itself

(defclass procedure-window (pict-svm window)   ; color-dialog     
  ((pict-name :accessor pict-name :initarg :pict-name :initform nil)
   (state-vec :accessor state-vec :initarg :state-vec :initform nil)
   (state-num :accessor state-num :initarg :state-num :initform 0)
   (training-p :accessor training-p :initarg :training-p :initform nil)
   (action-log :accessor action-log :initarg :action-log :initform nil)
   (timer :accessor timer :initarg :timer 
          :initform (make-instance 'event-timer))
   (out-path :accessor out-path :initarg :out-path :initform nil)
   (start-time :accessor start-time :initarg :start-time 
               :initform (get-internal-real-time))
   (num-errors :accessor num-errors :initarg :num-errors :initform 0)
   (advance-p :accessor advance-p :initarg :advance-p :initform t)
   (snd-player :accessor snd-player :initarg :snd-player :initform nil)
   (task-id :accessor task-id :initarg :task-id :initform 0)
   
   (gui-vec :accessor gui-vec :initarg :gui-vec :initform nil)
   (check-gui-p :accessor check-gui-p :initarg :check-gui-p :initform t)
   )
  (:default-initargs
    :close-box-p nil
    :grow-icon-p nil
    :view-size #@(1024 768)
    :view-position #@(0 0)
    ))


(defmethod curr-state ((wind procedure-window))
  (svref (state-vec wind) (state-num wind)))



(defmethod write-log ((wind procedure-window))
  (when (eq t (out-path wind))
    (write-events wind t)
    (return-from write-log nil))
  (unless (out-path wind)
    (setf (out-path wind) (choose-new-file-dialog :prompt "Save data in:")))
  (with-open-file (strm (out-path wind) :direction :output :if-exists :append
                        :if-does-not-exist :create)
    (write-events wind strm)))


(defmethod write-events ((wind procedure-window) &optional (strm t))
  (setf *tid* (task-id wind))
  (multiple-value-bind (sec min hour date mnth yr)
                       (decode-universal-time (get-universal-time))
    (declare (ignore sec))
    (format strm "~%---------- Task ID: ~S  trial ending ~S:~S on ~S/~S/~S ----------"
            (task-id wind) hour min mnth date yr)
    (dolist (p-act (reverse (action-log wind)))
      (write-pa p-act strm))))



(defmethod state-check ((wind procedure-window) state-name 
                          &optional info)
  (let ((p-act (make-instance 'proc-action
                  :latency (round (start-stop-timer (timer wind)))
                  :expected (curr-state wind)
                  :got state-name
                  :step-num (state-num wind)
                  :info info)))
    (push p-act (action-log wind))
    (if (not (is-error p-act))
      (advance-state wind)
      (proc-error wind))))


(defmethod advance-state ((wind procedure-window))
  (setf (advance-p wind) t)
  (gui-check wind)                      ; r2 new
  (check-state-update wind (curr-state wind))
  (when (advance-p wind)
    (incf (state-num wind)))
  (unless (training-p wind)
    (unless (eq (last-correct *sw*) (curr-state wind))
      (delta-score *sw* *step-value))
    (setf (last-correct *sw*) (curr-state wind))))


(defmethod gui-check ((wind procedure-window))
  (when (and (check-gui-p wind) (gui-vec wind))
    (awhen (aref (gui-vec wind) (state-num wind))
      (funcall (symbol-function (first it)) (view-named (rest it) wind)))))


(defmethod check-state-update ((wind procedure-window) state)
  (format t "New state is: ~S" state))


(defmethod proc-error ((wind procedure-window))
  (incf (num-errors wind))
  (if (snd-player wind)
    (play-snd (snd-player wind) "Warning" t)
    (beep))
  (if (training-p wind) 
    (finish-task wind)
    (delta-score *sw* *step-penalty)))


(defmethod handle-click ((self simple-view) &optional info)
  (state-check (view-window self) (view-nick-name self) info))


(defmethod finish-task ((wind procedure-window))
  (unless (training-p wind)
    (delta-score *sw* *step-value))
  (write-log wind)
  (if *modal-dialog-on-top*
    (return-from-modal-dialog 
     (values (num-errors wind) (elapsed-secs wind) (first (action-log wind))))
    (window-close wind)))


(defmethod elapsed-secs ((wind procedure-window))
  (round (- (get-internal-real-time)
            (start-time wind))
         1000))


(defmethod check-wm ((wind procedure-window))
  (unless (training-p wind) 
    (do-wm-check *wmt* wind)))


#|
    (multiple-value-bind (latency crct-p) (check-wm *wmt*)
      (when crct-p
        (push
         (make-instance 'proc-action
           :latency (round (stop-timing (timer wind)))
           :step-num -1
           :expected :wmprobe
           :got crct-p
           :info latency)
         (action-log wind))))))
|#

;;;; ---------------------------------------------------------------------- ;;;;
;;;; subclasses for use with procedure windows

(defclass checker ()
  ())

(defmethod view-click-event-handler :around ((self checker) where)
  (when (eq (view-nick-name self) (curr-state (view-window self)))
    (call-next-method))
  (handle-click self (point-string (subtract-points where
                                                    (view-position self)))))


(defclass button-checker (checker button-dialog-item)
  ())


(defclass radio-checker (checker radio-button-dialog-item)
  ())

(defclass check-box-checker (checker check-box-dialog-item)
  ())



;;;; ---------------------------------------------------------------------- ;;;;
;;;;  utilities


;;;; ---------------------------------------------------------------------- ;;;;
;;;; bookkeeping

(provide :procedure-window2)