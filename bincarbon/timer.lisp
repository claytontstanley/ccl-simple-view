;;;  -*- mode: LISP; Package: CL-USER; Syntax: COMMON-LISP;  Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Author      : Mike Byrne
;;; Copyright   : (c)1999-2004 Rice U./Mike Byrne, All Rights Reserved
;;; Availability: public domain
;;; Address     : Rice University
;;;             : Psychology Department
;;;             : Houston,TX 77251-1892
;;;             : byrne@acm.org
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Filename    : timer.lisp
;;; Version     : r3
;;; 
;;; Description : 
;;; 
;;; Bugs        : 
;;; 
;;; Todo        : 
;;; 
;;; ----- History -----
;;; 99.11.05 Mike Byrne
;;;             :  Incept date.
;;; 00.02.02 mdb
;;;             : Added unless bound modifications.
;;; 00.11.15 mdb
;;;             : Added exit methods.
;;; 2002.11.24 mdb [r2]
;;;             : Minor bug fix.
;;; 2004.10.31 mdb [r3]
;;;             : Added FIND-KEY-COORDS function.  Use 
;;;             : (wait-for-keys <timer> #'find-key-coords) to figure out
;;;             : what the values are for any particular key.
;;; 2008.06.11 fpt [r4]
;;;		: ACT-R 6 dropped pm-get-time for mp-time, so I updated
;;;		current-time and the appropriate unless bound function
;;;		to reflect this change.
;;; 2012.08.06 cts
;;;             : Ported some of the code to work with Clozure Common Lisp,
;;;               while maintaining backwards compatibility with RMCL. Lots
;;;               of work on this file is still left.
;;;		
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(require :misc-lib)

(unless (boundp '*acr-enabled-p*)
  (defvar *actr-enabled-p*)
  (setf *actr-enabled-p* nil))



;;;; ---------------------------------------------------------------------- ;;;;
;;;; Macros

(defmacro check-mac-error (form)
  "Checks a Macintosh error code"
  `(let ((mac-error ,form))
     (when (not (= mac-error 0))
       (error "MacOS error ~A" mac-error))))


;;;; ---------------------------------------------------------------------- ;;;;
;;;; Class definitions

(defclass timer () ())


(defclass event-timer (timer)
  ((tickms :accessor tickms :initarg :tickms :initform 50/3)
   (start-time :accessor start-time :initarg :start-time :initform nil)
   ))

#+:digitool
(defclass ms-timer (timer)
  ((keymap :accessor keymap :initarg :keymap :initform (make-record keymap))
   (map-ptr :accessor map-ptr :initarg :map-ptr :initform nil)
   )
  )

#+:clozure
(defclass ms-timer (timer)
  (;(keymap :accessor keymap :initarg :keymap :initform (make-record keymap))
   (map-ptr :accessor map-ptr :initarg :map-ptr :initform nil)
   )
  )


(defclass mic-timer (timer)
  ((mic-thresh :accessor mic-thresh :initarg :mic-thresh :initform 18)
   (quiet-thresh :accessor quiet-thresh :initarg :quiet-thresh :initform 8)
   )
  )


;;;; ---------------------------------------------------------------------- ;;;;
;;;; Base class methods

(defgeneric spin-for (tmr ms-delay)
  (:documentation "Spins for ms-delay milliseconds."))

(defmethod spin-for ((tmr timer) ms-delay)
  (without-interrupts
     (let ((start (get-internal-real-time)))
       (while (> ms-delay (- (get-internal-real-time) start))))))


(defgeneric dispose-timer (tmr)
  (:documentation "Releases any resources used by the timer."))


;;;; ---------------------------------------------------------------------- ;;;;
;;;; Event timing

(defgeneric tick->ms (tmr ticks)
  (:documentation "Converts ticks to milliseconds."))

(defmethod tick->ms ((tmr event-timer) ticks)
  (float (* ticks (tickms tmr))))


(defgeneric start-timing (tmr)
  (:documentation "Starts an event timer at the current event."))

(defmethod start-timing ((tmr event-timer))
  (setf (start-time tmr) (current-time tmr)))


(defgeneric current-time (tmr)
  (:documentation "Returns the current time in milliseconds."))

#+:digitool
(defmethod current-time ((tmr event-timer))
  (cond (*actr-enabled-p* (mp-time))
        ((boundp '*current-event*)
         (tick->ms tmr (pref *current-event* :eventrecord.when)))
        (t (tick->ms tmr (#_tickcount)))))


#+:clozure
(defmethod current-time ((tmr event-timer))
  0)

(defgeneric stop-timing (tmr)
  (:documentation "Stops an event timer at the current event and returns the time in ms."))

(defmethod stop-timing ((tmr event-timer))
  (- (current-time tmr) (start-time tmr)))


(defmethod start-stop-timer ((tmr event-timer))
  (let ((lat (stop-timing tmr)))
    (start-timing tmr)
    lat))


;;;; ---------------------------------------------------------------------- ;;;;
;;;; ms-level (roughly) keyboard timing

#+:digitool
(defmethod initialize-instance :after ((tmr ms-timer) &key)
  ;(setf (map-ptr tmr) (rref (keymap tmr) keymap.array)))
  (setf (map-ptr tmr) (rref (keymap tmr) keymap.contents)))


#+:digitool
(defmethod dispose-timer ((tmr ms-timer))
  (dispose-record (keymap tmr)))

(defgeneric wait-for-keys (tmr fct)
  (:documentation "Wait for FCT [a test on the state of keys] to be true, returning the time and optionally which key was pressed."))


#+:digitool
(defmethod wait-for-keys ((tmr ms-timer) (fct function))
  (without-interrupts
   (let ((start (get-internal-real-time)))
     (#_getkeys (keymap tmr))
     (while (not (funcall fct tmr))
       (#_getkeys (keymap tmr)))
     (values
      (- (get-internal-real-time) start)
      (funcall fct tmr)))))


;;; Examples of functions to pass to WAIT-FOR-KEYS 

(defun spacebar-down (tmr)
  "Returns T if the spacebar is down, NIL otherwise."
  (if (= 2 (%get-unsigned-byte (map-ptr tmr) 6))
    #\space
    nil))

#| So, to wait for the spacebar and get latency:

(setf latency (wait-for-keys tmr #'spacebar-down))

|#


(defun d-or-f (tmr)
  "Tests for keys"
  (without-interrupts 
   (cond ((= (%get-unsigned-byte (map-ptr tmr)) 4) #\d)
         ((= (%get-unsigned-byte (map-ptr tmr)) 8) #\f)
         (t nil))))

#| To wait for either the D or F key, and get the RT and which key was pressed:

(multiple-value-bind (rt key)
                     (wait-for-keys tmr #'d-or-f)
  ;; other forms
  )

|#


(defun j-or-k (tmr)
  "Tests for keys"
  (without-interrupts 
   (cond ((= (%get-unsigned-byte (map-ptr tmr) 4) 64) #\j)
         ((= (%get-unsigned-byte (map-ptr tmr) 5) 1) #\k)
         (t nil))))


(defgeneric wait-for-no-keys (tmr)
  (:documentation "Wait until no key is down, returning latency."))

#+:digitool
(defmethod wait-for-no-keys ((tmr ms-timer))
  (without-interrupts
   (let ((start (get-internal-real-time)))
     (#_getkeys (keymap tmr))
     (while (not (test-for-no-key-down tmr))
       (#_getkeys (keymap tmr)))
     (- (get-internal-real-time) start))))

#+:digitool
(defmethod test-for-no-key-down ((tmr ms-timer))
  (dotimes (i 16)
    (when (not (= 0 (%get-unsigned-byte (map-ptr tmr) i)))
      (return-from test-for-no-key-down nil)))
  t)


(defgeneric wait-for-any-key (tmr)
  (:documentation "Spins the system until a key is pressed, returns the latency in ms."))

#+:digitool
(defmethod wait-for-any-key ((tmr ms-timer))
  (without-interrupts
   (let ((start (get-internal-real-time)))
     (#_GetKeys (keymap tmr))
     (while (not (test-for-any-key-down tmr))
       (#_GetKeys (keymap tmr)))
     (- (get-internal-real-time) start))))


(defgeneric test-for-any-key-down (tmr)
  (:documentation "Returns T if any key is depressed, NIL otherwise."))

(defmethod test-for-any-key-down ((tmr timer))
  (dotimes (i 16)
    (unless (= 0 (%get-unsigned-byte (map-ptr tmr) i))
      (return-from test-for-any-key-down t)))
  nil)


(defmethod find-key-coords ((tmr timer))
  (dotimes (i 16)
    (unless (= 0 (%get-unsigned-byte (map-ptr tmr) i))
      (format t "~%Byte offset: ~S  Value: ~S"  i 
              (%get-unsigned-byte (map-ptr tmr) i))
      (return-from find-key-coords t)))
  nil)
 


#|
(defgeneric wait-for-spacebar (tmr)
  (:documentation "Spins until the spacebar is pressed, returns the latency in ms."))

(defmethod wait-for-spacebar ((tmr ms-timer))
  (without-interrupts
   (let ((start (get-internal-real-time)))
     (#_GetKeys (keymap tmr))
     (while (not (test-for-spacebar-down tmr))
       (#_GetKeys (keymap tmr)))
     (- (get-internal-real-time) start))))


(defgeneric test-for-spacebar-down (tmr)
  (:documentation   "Returns T if the spacebar is down, NIL otherwise."))


(defmethod test-for-spacebar-down ((tmr ms-timer))
  (if (= 2 (%get-unsigned-byte (map-ptr tmr) 6))
    t
    nil))
|#


#|
;;;; ---------------------------------------------------------------------- ;;;;
;;;;  Voice key stuff
;;;; ---------------------------------------------------------------------- ;;;;

;;; SETUP-VK-LIBRARY      [Function]
;;; Description : Adds the shared library to the search path, defines MCL 
;;;             : functions for the library's entry points, and loads up
;;;             : the shared library.

(defun setup-vk-library ()
  "Sets up the voice key library for use by MCL"
  (ccl::add-to-shared-library-search-path "VoiceKeyLib")
  (define-entry-point 
    ("get-mic-latency-synch" ("VoiceKeyLib" "GetMicLatencySynch"))
    ((thresh :signed-short))
    :signed-long)
  (define-entry-point 
    ("stop-mic-timing-asynch" ("VoiceKeyLib" "StopMicTimingAsynch"))
    ()
    :signed-long)
  (define-entry-point 
    ("start-mic-timing-asynch" ("VoiceKeyLib" "StartMicTimingAsynch"))
    ((thresh :signed-short))
    :signed-short)
  (define-entry-point
    ("init-microphone" ("VoiceKeyLib" "InitMicrophone"))
    ()
    :signed-short)
  (define-entry-point
    ("release-microphone" ("VoiceKeyLib" "ReleaseMicrophone"))
    ())
  (define-entry-point
    ("get-asynch-mic-time" ("VoiceKeyLib" "GetAsynchMicTime"))
    ()
    :signed-long)
  (define-entry-point
    ("switch-mic-to-synch" ("VoiceKeyLib" "SwitchMicToSynch"))
    ()
    :signed-long)
  (define-entry-point
    ("get-mic-level" ("VoiceKeyLib" "GetMicLevel"))
    ()
    :signed-short)
  (ccl::get-shared-library "VoiceKeyLib"))


(eval-when (load eval)
  (setup-vk-library))


(defmethod initialize-instance :after ((tmr mic-timer) &key)
  (check-mac-error (init-microphone)))


(defmethod dispose-timer ((tmr mic-timer))
  (release-microphone))


(defgeneric wait-for-quiet (tmr &optional lag)
  (:documentation "Spins until two mic samples <lag> ms apart register below the quiet threshold."))

(defmethod wait-for-quiet ((tmr mic-timer) &optional (lag 60))
  (let ((start (get-internal-real-time))
        (hit-quiet nil)
        (done nil))
    (while (not done)
      (cond (hit-quiet (if (> (quiet-thresh tmr) (get-mic-level))
                         (setf done t)
                         (setf hit-quiet nil)))
            ((> (quiet-thresh tmr) (get-mic-level)) (setf hit-quiet t))
            (t (spin-for tmr lag))))
    (- (get-internal-real-time) start)))



(defgeneric mic-latency (tmr)
  (:documentation "Spins until the mic level goes above threshold and returns the latency in ms."))


(defmethod mic-latency ((tmr mic-timer))
  (get-mic-latency-synch (mic-thresh tmr)))


(defgeneric start-mic-timing (tmr)
  (:documentation "Initiates asynchronous microphone timing."))

(defmethod start-mic-timing ((tmr mic-timer))
  (start-mic-timing-asynch (mic-thresh tmr)))


(defgeneric stop-mic-timing (tmr)
  (:documentation "Stops asynchronous mic timing.  If no sound has been detected, returns 0.  
Else returns sound latency in ms."))

(defmethod stop-mic-timing ((tmr mic-timer))
  (stop-mic-timing-asynch))


(defgeneric asynch-to-synch (tmr)
  (:documentation "Switches from asynch to synchronous mic timing."))

(defmethod asynch-to-synch ((tmr mic-timer))
  (switch-mic-to-synch))
|#


;;;; ---------------------------------------------------------------------- ;;;;
;;;; bookkeeping

(unless (fboundp 'mp-time)
  (defun mp-time () nil))


(provide :timer)


;;;; ---------------------------------------------------------------------- ;;;;
;;;; test code

#|
(defvar *gtimer*)
(setf *gtimer* (make-instance 'event-timer))

|#
