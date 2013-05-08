;;;  -*- mode: LISP; Package: CL-USER; Syntax: COMMON-LISP;  Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Author      : Mike Byrne
;;; Copyright   : (c)2003-7 CMU/Rice U./Mike Byrne, All Rights Reserved
;;; Availability: public domain
;;; Address     : Rice University
;;;             : Psychology Department
;;;             : Houston,TX 77251-1892
;;;             : byrne@acm.org
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Filename    : experiment-window4.lisp
;;; Version     : 4.0 r7
;;; 
;;; Description : Provides some base classes for running trial-based 
;;;             : experiments: EXPERIMENT-WINDOW, for the actual window,
;;;             : TRIAL, for keeping track of trial information, TRIAL-BLOCK
;;;             : for keeping track of blocks, and 
;;;             : INSTRUCTION-DIALOG for presenting text instructions.
;;;
;;; Bugs        : none known
;;; 
;;; Todo        : [] RPM hooks for mic and key timing?  Might have to go in
;;;             :    the timer library, not sure.
;;; 
;;; ----- History -----
;;; 2003.04.12 mdb
;;;             : Initial writing based on experiment-window 3.1.1, Centgraf's
;;;             : comments, and a few bits of other code.  Main changes:
;;;             : * SETUP-TRIAL and DO-TRIAL take a trial object as a second
;;;             :   param, making method dispatch straightforward.
;;;             : * Less stupid implementation of TAB-OUTPUT.
;;;             : * Added ITI [inter-trial interval] slot to the 
;;;             :   experiment-window class.
;;;             : * Added cursor movement that should work under OS X.
;;; 2003.10.14 mdb [4.0.1]
;;;             : More mods for OS X:  Launch-url now in there.
;;; 2003.11.23 mdb [4.0.2]
;;;             : * Default window size changed to 1024 by 768 for eMacs.
;;;             : * CFMBundle now required properly.
;;;             : * Silly case bug fixed for mouse-move code.
;;; 2003.12.05 mdb
;;;             : * Changed WARNING function window title.
;;;             : * Upped size of instruction dialog to 1024 x 768 as well.
;;; 2003.12.12 mdb [r3]
;;;             : * Moved TAB-OUTPUT to misc-lib.
;;; 2005.06.28 mdb [r4]
;;;             : * Changed slots of TRIAL to be TRIAL-KIND and TRIAL-BLOCK.
;;;             : * Added XCOND slot to experiment window class and now pass
;;;             :   subject number and condition through to the Web form.
;;; 2005.06.30 mdb
;;;             : MOP changes in MCL 5.1 broke the readable-writer code.  Fixed.
;;; 2005.07.09 mdb
;;;             : Bug in URL generation fixed.
;;; 2005.09.14 mdb [r5]
;;;             : Changed all "dat" extentions to "txt" to make things happier.
;;; 2007.03.22 fpt [r6]
;;;             : Change warning to have a :size keyword argument (defaults to
;;;             : #@(300 250)) that it in turn supplies to message-dialog.
;;; 2007.06.06 mdb [r7]
;;;             : Added WRITE-READABLE method for pathnames.   
;;; 2012.06.26 mdb
;;;             : Some changes to MOVE-CURSOR-TO to handle RMCL 5.2 better. 
;;; 2012.08.06 cts
;;;             : Ported the code to work with Clozure Common Lisp, while maintaining
;;;               backwards compatibility with RMCL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+:clozure (require :ccl-simple-view)
(require :timer)
#+:digitool (require :cfbundle)
(require :misc-lib)
(require :seq-math)
(require :launch-url)
(require :menubar-hide)

#+:clozure
(defun osx-p ()
  t)

;;;; ---------------------------------------------------------------------- ;;;;
;;;; globals, macros, & setup
;;;; ---------------------------------------------------------------------- ;;;;

(defparameter *end-of-block-str*
  "Finished block ~A of ~A. Please take a short break to help your concentration.
   Click “OK” when you are ready to continue.")

(defparameter *end-of-expt-str*
  "Thank you, you've finished the main portion of the experiment. Please take a few moments to answer a few simple questions.")

(defparameter *end-of-expt-nourl-str*
  "You're done, thank you! Please see the experimenter.")


(defvar *experiment* nil "The experiment window itself.")


(unless (fboundp 'menubar-hide)
  (defun menubar-hide () nil))

(unless (fboundp 'menubar-show)
  (defun menubar-show () nil))



;;;; ---------------------------------------------------------------------- ;;;;
;;;; The READABLE-WRITER class
;;;;
;;;             : A base class for objects that can write readable 
;;;             : representations of themselves.
;;;
;;;; ---------------------------------------------------------------------- ;;;;


(defclass readable-writer ()
  ((slot-lst :accessor slot-lst :initarg :slot-lst :initform nil)
   ))


;;;; ---------------------------------------------------------------------- ;;;;
;;;;
;;;; The TRIAL class
;;;;
;;;;             : A class for storing information about trials, like condition
;;;;             : stimuli, and the like.  Used by the EXPERIMENT-WINDOW class.
;;;;             : A WRITE-TRIAL method is expected for printing the trial
;;;;             : information to the data file.
;;;;
;;;; ---------------------------------------------------------------------- ;;;;


;;; TRIAL      [Class]
;;; Description :  A class for storing information about a particular trial.

(defclass trial ()
  ((trial-kind :accessor trial-kind :initarg :trial-kind :initform nil)
   (trial-block :accessor trial-block :initarg :trial-block :initform nil)
   (start-time :accessor start-time :initarg :start-time :initform nil)))


;;; WRITE-TRIAL      [Generic Function]
;;; Description : Writes a text representation (for the data file) of the trial
;;;             : to the given stream

(defgeneric write-trial (trial strm)
  (:documentation "Writes a text representation (for the data file) of the trial to the given stream."))


;;;; ---------------------------------------------------------------------- ;;;;
;;;;             
;;;; The EXPERIMENT-WINDOW class
;;;;
;;;             : A class for the actual experiment window and trial management.
;;;             : This class assumes that the experiment happens in blocks.
;;;             : If not, well, build only one block of trials.
;;;             : Any derived subclass will probably want to supply certain
;;;             : methods:
;;;             : 
;;;             : INITIALIZE-INSTANCE, to set up the experiment:  set the 
;;;             : data file path and number of blocks in the experiment,
;;;             : write a first line to the data file, that kind of thing.
;;;             : 
;;;             : A BUILD-TRIAL-lst method, for building lists of trial
;;;             : objects.
;;;             : 
;;;             : A :before or :after method for SETUP-TRIAL.
;;;             : 
;;;             : A DO-TRIAL method.
;;;             : 
;;;             : A FINISH-TRIAL method, to do cleanup for each trial.
;;;
;;;; ---------------------------------------------------------------------- ;;;;

;;; EXPERIMENT-WINDOW      [Class]
;;; Description : Base class for the experiment window, including slots for
;;;             : trial management and the data file.

(defclass experiment-window (color-dialog)
  ((block-lst :accessor block-lst :initform nil :initarg :block-lst)
   (current-trial :accessor current-trial :initform nil)
   (nblocks :accessor nblocks :initarg :nblocks :initform nil)
   (cblock :accessor cblock :initarg :cblock :initform 0)
   (base-path :accessor base-path :initarg :base-path
              :initform
              (progn
                (warning "Please locate the directory for saving data.")
                (namestring (choose-directory-dialog))))
   (completed-trials :accessor completed-trials :initform nil)
   (timer :accessor timer :initarg :timer)
   (instructions :accessor instructions :initarg :instructions
                 :initform nil)
   (snum :accessor snum :initarg :snum :initform (get-subject-number))
   (write-type :accessor write-type :initarg :write-type :initform :BOTH)
   (url :accessor url :initarg :url 
        :initform "http://chil.rice.edu/cgi-bin/FMSurvey1.acgi")
   (iti :accessor iti :initarg :iti :initform 500)
   (xcond :accessor xcond :initarg :xcond :initform 0)
   (xnum :accessor xnum :initarg :xnum :initform nil)
   )
  (:default-initargs
    :view-size #@(1024 768)
    :view-position #@(0 0)
    :window-title "Experiment"
    :close-box-p NIL))



(defclass event-exp-window (experiment-window)
  ()
  (:default-initargs
    :timer (make-instance 'event-timer)))


(defclass timed-exp-window (experiment-window)
  ()
  (:default-initargs
    :timer (make-instance 'ms-timer)))



;;;; ---------------------------------------------------------------------- ;;;;
;;;; cover windows
;;;; ---------------------------------------------------------------------- ;;;;


(defclass cover-window (dialog)
  ()
  (:default-initargs
    :view-size #@(800 600)
    :view-position #@(0 0)))


;;;; ---------------------------------------------------------------------- ;;;;
;;;;
;;;; The TRIAL-BLOCK class.
;;;;
;;;             : Used to store and manage lists of trials.
;;;
;;;; ---------------------------------------------------------------------- ;;;;


;;; TRIAL-BLOCK      [Class]
;;; Date        : 97.10.09
;;; Description : A class for storing information about blocks.

(defclass trial-block ()
  ((kind :accessor kind :initform nil :initarg :kind)
   (trial-lst :accessor trial-lst :initform nil :initarg :trial-lst)
   (size :accessor size :initform 0 :initarg :size)
   (current-idx :accessor current-idx :initarg :current-idx :initform 0)
   ))


;;;; ---------------------------------------------------------------------- ;;;;
;;;;  
;;;;  The INSTRUCTION-DIALOG class
;;;
;;;             : A simple class for presenting text instructions.  The way
;;;             : this class is used is simple:  make an instance, and it
;;;             : will prompt for a text file containing the instructions.
;;;             : If that's not OK, you can just pass in the text you want in
;;;             : the initarg.
;;;             : A second issue is control--what to do when the instructions
;;;             : have been presented.  Just pass whatever Lisp expression you
;;;             : want run as the :completion initarg, and you'll be set.
;;;
;;;; ---------------------------------------------------------------------- ;;;;


(defclass instruction-dialog (dialog)
  ((instruction-texts :accessor texts
                      :initarg  :texts
                      :initform (read-from-chosen-file))
   (completion-hook :accessor completion-hook
                    :initform nil
                    :initarg :completion)
   (modal-p :accessor modal-p :initform nil :initarg :modal-p)
   )
  (:default-initargs
    :window-type :movable-dialog
    :window-title "Instructions"
    :view-position #@(0 0)
    :view-size #@(1024 768)
    :close-box-p nil
    :view-subviews
    (list 
      (make-dialog-item
        'DEFAULT-BUTTON-DIALOG-ITEM
        #@(700 550) #@(85 25) "Continue"
        #'(lambda (self) (continue-click (view-window self)))
        :view-nick-name 'CONTINUE)
      (make-dialog-item
        'STATIC-TEXT-DIALOG-ITEM
        #@(20 40) #@(595 450) ""
        'NIL
        :view-nick-name 'TEXT
        :view-font '("Geneva" 12 :SRCOR :PLAIN (:COLOR-INDEX 0)))
      (make-dialog-item
        'STATIC-TEXT-DIALOG-ITEM
        #@(2 560) #@(433 15)
        "press the “Return” key or click “Continue” when you are ready to go on"
        'NIL
        :view-font '("Geneva" 10 :SRCOR :BOLD (:COLOR-INDEX 0))))))



;;;; ---------------------------------------------------------------------- ;;;;
;;;; end of class definitions



;;;; ---------------------------------------------------------------------- ;;;;
;;;; 
;;;;  Methods for the EXPERIMENT-WINDOW class and its children
;;;;
;;;; ---------------------------------------------------------------------- ;;;;

(defgeneric setup-experiment (wind)
  (:documentation "Method for any initialization code not in the INITIALIZE-INSTANCE method."))

(defmethod setup-experiment ((wind experiment-window))
  nil)


(defgeneric run-experiment (wind)
  (:documentation "Run the experiment.  Works different for different subclasses."))


;;; RUN-EXPERIMENT      [Method]
;;; Description : Running the experiment, which is three phases:  Set up
;;;             : whatever needs to be set up, then iterating through all
;;;             : the blocks, then a finish method.

(defmethod run-experiment ((wind timed-exp-window))
  (menubar-hide)
  (when (member (write-type wind) '(:LISP :BOTH))
    (with-open-file (strm (make-data-path wind "lisp") :direction :output 
                          :if-exists :append :if-does-not-exist :create)
      (format strm "(list ")))
  (setup-experiment wind)
  (dolist (block (block-lst wind))
    (run-block wind block))
  (finish-experiment wind))


;;; RUN-EXPERIMENT      [Method]
;;; Date        : 00.06.06
;;; Description : Much simpler for event-driven--just run the first block.

(defmethod run-experiment ((wind event-exp-window))
  (menubar-hide)
  (setup-experiment wind)
  (run-block wind (first (block-lst wind))))


(defgeneric finish-experiment (wind)
  (:documentation "Clean-up at the end of the experiment: beep, close the experiment window, 
                   display a finished message, and clean up the data file. Might want an :AFTER method here."))

(defmethod finish-experiment ((wind experiment-window))
  (beep)
  (window-close wind)
  (#_showcursor)
  (when (and (base-path wind) (member (write-type wind) '(:SS :BOTH)))
    (let ((path (make-data-path wind "txt")))
      (set-mac-file-creator path :|R*ch|)
      (lock-file path)))
  (when (member (write-type wind) '(:LISP :BOTH))
    (with-open-file (strm (make-data-path wind "lisp") :direction :output 
                          :if-exists :append :if-does-not-exist :create)
      (format strm ")")))
  (menubar-show)
  ;; if there's no URL stored, then there's no questionnaire, so just bail.
  (if (null (url wind))
    (warning *end-of-expt-nourl-str*)
    ;; if there's no experiment number, use a "dumb" URL.  If there is,
    ;; then use a "smart" URL
    (let ((url-str (if (null (xnum wind))
                     (url wind)
                     (mkstr (url wind) "?xnum=" (xnum wind) 
                            "&snum=" (snum wind)
                            "&xcond=" (xcond wind)))))
      (warning *end-of-expt-str*)
      (cond ((osx-p)
             (launch-url url-str))
            (t
             (error "should not ever get here"))))))


(defmethod make-data-path ((wind experiment-window) type)
  (when (base-path wind)
    (make-pathname :directory (base-path wind)
                   :name
                   (format nil "subj~3,'0D" (snum wind))
                   :type type)))


(defmethod data-file ((wind experiment-window))
  (make-data-path wind "txt"))



;;;; ---------------------------------------------------------------------- ;;;;
;;;; Trial management

;;; SETUP-TRIAL      [Method]
;;; Description : Pop trial-lst into the current trial slot.  Probably
;;;             : requires an :AFTER method from the subclass.

(defgeneric setup-trial (wind trl)
  (:documentation "Pops the window's trial-lst into the <current-trial> slot.  Probably will 
                   require an :AFTER method from the subclass."))

(defmethod setup-trial ((wind experiment-window) (trl trial))
  (setf (trial-block trl) (cblock wind)
        (start-time trl) (get-internal-real-time))
  )


(defgeneric do-trial (wind trl)
  (:documentation "Method to handle the main part of the trial, after set-up."))


(defgeneric finish-trial (wind)
  (:documentation "Method for trial clean-up. *Must* be called explicitly in EVENT-EXP-WINDOWs."))


;;; FINISH-TRIAL      [Method]
;;; Date        : 00.02.16
;;; Description : At the end of a trial, check to see if we're at the end of
;;;             : a block.  If so, finish the block.  If not, update the
;;;             : trial index in the block, wait for a bit, and start the next
;;;             : trial.
;;;             : Note that this method does not take a trial argument so that
;;;             : it can be called more easily from code in, say, a button. 

(defmethod finish-trial ((wind event-exp-window))
  (let ((curr-block (nth (cblock wind) (block-lst wind))))
    (incf (current-idx curr-block))
    (if (= (size curr-block) (current-idx curr-block))
      (finish-block wind curr-block)
      (progn
        (setf (current-trial wind)
              (nth (current-idx curr-block) (trial-lst curr-block)))
        (spin-for (timer wind) (iti wind))
        (setup-trial wind (current-trial wind))))))


(defmethod show-instructions ((wind experiment-window))
  (awhen (pop (instructions wind))
    (wait-for-instructions it)))



;;;; ---------------------------------------------------------------------- ;;;;
;;;; block management


(defgeneric setup-block (wind blk)
  (:documentation "Sets up a block.  Default does nothing."))

(defmethod setup-block ((wind experiment-window) (blk trial-block))
  nil)


(defgeneric run-block (wind blk)
  (:documentation "Runs a block of trials. Different subclasses do this differently."))  

;;; RUN-BLOCK      [Method]
;;; Description : Run a block when not event-driven.  To run a block, do the 
;;;             : setup, then iterate through the trials, then finish off the 
;;;             : block.

(defmethod run-block ((wind timed-exp-window) (blk trial-block))
  (setup-block wind blk)
  (dolist (trl (trial-lst blk))
    (setf (current-trial wind) trl)
    (setup-trial wind trl)
    (do-trial wind trl)
    (finish-trial wind))
  (finish-block wind blk))


;;; RUN-BLOCK      [Method]
;;; Description : Run a block when event-driven.  All that's done here is
;;;             : starting off the block.

(defmethod run-block ((wind event-exp-window) (blk trial-block))
  (setup-block wind blk)
  (setf (current-trial wind) (first (trial-lst blk)))
  (setup-trial wind (current-trial wind)))


(defgeneric finish-block (wind blk)
  (:documentation "Called upon finishing a block of trials.  Write the data, put up a status update. 
                   Subclasses do more.  Might add a :BEFORE method to compute accuracy."))

(defmethod finish-block ((wind experiment-window) (blk trial-block))
  (write-block wind blk)
  (gc)
  (incf (cblock wind))
  (beep)
  (unless (= (cblock wind) (nblocks wind))
    (warning 
      (format nil *end-of-block-str* 
              (cblock wind) (nblocks wind)))))


;;; FINISH-BLOCK      [Method]
;;; Description : When it's event-driven, have to check for the end of the
;;;             : experiment and start the next block manually.

(defmethod finish-block :after ((wind event-exp-window) (blk trial-block))
  (if (= (nblocks wind) (cblock wind))
    (finish-experiment wind)
    (run-block wind (nth (cblock wind) (block-lst wind)))))


(defgeneric write-block (wind blk)
  (:documentation "Writes a block of trial data to the file specified in <wind>, if there is one.  If not, print to T."))

(defmethod write-block ((wind experiment-window) (blk trial-block))
  (if (null (base-path wind))
    ;; if not writing to a file, write SS data to terminal
    (dolist (the-trial (trial-lst blk))
      (write-trial the-trial t))
    ;; otherwise, need to know what type to write
    (progn
      (when (member (write-type wind) '(:SS :BOTH))
        (with-open-file (strm (make-data-path wind "txt") :direction :output 
                              :if-exists :append :if-does-not-exist :create)
          (dolist (the-trial (trial-lst blk))
            (write-trial the-trial strm))))
      (when (member (write-type wind) '(:LISP :BOTH))
        (with-open-file (strm (make-data-path wind "lisp") :direction :output 
                              :if-exists :append :if-does-not-exist :create)
          (write-readable (trial-lst blk) strm))))))

;;;; ---------------------------------------------------------------------- ;;;;
;;;; Instruction dialog stuff
;;;; ---------------------------------------------------------------------- ;;;;

;;; INITIALIZE-INSTANCE      [Method]
;;; Description : Puts the first text block into the window.

(defmethod initialize-instance :after ((wind instruction-dialog) &key) 
  "Pops the first instruction text into the window"
  (set-dialog-item-text (view-named 'TEXT wind) (pop (texts wind))))


;;; CONTINUE-CLICK      [Method]
;;; Description : When the 'Continue' button is clicked, pop the next block
;;;             : of text.  If there is no more text, then run the completion
;;;             : hook.

(defmethod continue-click ((wind instruction-dialog))
  "Called when the 'Continue' button is clicked, advances the text or closes"
  (let ((the-text (pop (texts wind))))
    (if the-text
      (set-dialog-item-text (view-named 'TEXT wind) the-text)
      (progn
        (when (not (modal-p wind)) 
          (window-close wind)
          (event-dispatch))
        (eval (completion-hook wind))))))


;;; READ-FROM-FILE      [Method]
;;; Description : Called by the default initarg, this prompts the user for
;;;             : a text file [assumed to be a valid Lisp list of strings]
;;;             : and puts that list into the list of texts.

(defmethod read-from-file ((wind instruction-dialog))
  "Reads a file (assumed to be a valid Lisp list of strings) into an instruction list"
  (with-open-file (str (choose-file-dialog :mac-file-type "TEXT"
                                           :button-string "Instr")
                       :direction :input)
    (setf (texts wind) (read str))))


;;; READ-FROM-CHOSEN-FILE      [Function]
;;; Description : A function [as opposed to method] version of READ-FROM-FILE.

(defun read-from-chosen-file ()
  "Reads from a file chosen by the user"
  (with-open-file (str (choose-file-dialog :mac-file-type "TEXT"
                                           :button-string "Read")
                       :direction :input)
    (read str)))


;;; WAIT-FOR-INSTRUCTIONS      [Function]
;;; Date        : 98.05.12
;;; Description : There are times when using the completion hook mechanism
;;;             : is less than optimal and a waiting function call is 
;;;             : preferred.  This function handles that need.

(defun wait-for-instructions (texts)
  "Displays TEXTS as a modal dialog."
  (modal-dialog 
    (make-instance 'instruction-dialog :texts texts :modal-p t
                   :completion '(return-from-modal-dialog t))))




;;;; ---------------------------------------------------------------------- ;;;;
;;;; Readable writing stuff
;;;; ---------------------------------------------------------------------- ;;;;

; Taken from MCL's src.
#+:clozure
(defun class-instance-slots (class)
  (loop for s in (class-slots class)
        when (eq :instance (slot-definition-allocation s))
        collect s))

(defgeneric write-readable (thing &optional strm)
  (:documentation "Write THING to STRM in a way that it can be re-read."))


(defmethod write-readable ((thing readable-writer) &optional (strm t))
  (let* ((slot-desc (class-instance-slots (class-of thing))))
    (write-mk-in thing)
    (dolist (sname (slot-lst thing))
      (format strm " ~S " (caar (last (assoc sname slot-desc))))
      (write-readable (slot-value thing sname)))
    (princ ")" strm)
    thing))

#-mcl-common-mop-subset
(defmethod write-readable ((thing standard-object) &optional (strm t))
  (write-mk-in thing strm)
  (dolist (sdesc (class-instance-slots (class-of thing)))
    (format strm " ~S " (caar (last sdesc)))
    (write-readable (slot-value thing (first sdesc)) strm))
  (princ ")" strm)
  thing)

#+mcl-common-mop-subset
(defmethod write-readable ((thing standard-object) &optional (strm t))
  (write-mk-in thing strm)
  (dolist (sdesc (class-instance-slots (class-of thing)))
    (format strm " ~S " (first (slot-definition-initargs sdesc)))
    (write-readable (slot-value thing (slot-definition-name sdesc)) strm))
  (princ ")" strm)
  thing)


(defmethod write-mk-in ((thing standard-object) &optional (strm t))
  (format strm "~%(make-instance ")
  (write-readable (class-name (class-of thing)) strm)
  (terpri strm))


(defmethod write-readable ((thing list) &optional (strm t))
  (if (listp (rest thing))
    (write-readable-list thing strm)
    (write-readable-cons thing strm)))

(defun write-readable-list (ls &optional (strm t))
  (princ "(list " strm)
  (dolist (item ls)
    (write-readable item strm)
    (princ " " strm))
  (princ ")" strm))

(defun write-readable-cons (cns &optional (strm t))
  (princ "(cons " strm)
  (write-readable (first cns) strm)
  (princ " " strm)
  (write-readable (rest cns) strm)
  (princ ")" strm))

(defmethod write-readable ((thing vector) &optional (strm t))
  (princ "(vector " strm)
  (map 'vector #'(lambda (x) (write-readable x strm) (princ " " strm)) thing)
  (princ ")" strm))

(defmethod write-readable ((thing symbol) &optional (strm t))
  (format strm "'~S" thing))

(defmethod write-readable ((thing number) &optional (strm t))
  (princ thing strm))

(defmethod write-readable ((thing string) &optional (strm t))
  (format strm "~S" thing))

(defmethod write-readable ((thing null) &optional (strm t))
  (princ "NIL" strm))

(defmethod write-readable ((thing pathname) &optional (strm t))
  (write-readable (namestring thing) strm))

;;;; ---------------------------------------------------------------------- ;;;;
;;;;  Misc utility stuff
;;;; ---------------------------------------------------------------------- ;;;;


(defun warning (string &key (cursor nil) 
                       (size #@(400 200)))
  "Displays a warning dialog, with a cursor."
  (when cursor (#_ShowCursor))
  (message-dialog string :title "Message" :size size :position #@(300 250))
  (when cursor (#_HideCursor)))


(defun get-subject-number ()
  "Prompts the user for a subject number."
  (let ((num :foo))
    (while (not (numberp num))
      (setf num (read-from-string (get-string-from-user "Subject number:"))))
    num))


;;; under MCL 5.2, can't use CFBundle, but there are alternate ways to 
;;; deal with framework calls, so use that.

#+ccl-5.2
(when (osx-p)
  (progn
    (defparameter *warp* (ccl::lookup-function-in-bundle 
                           "CGWarpMouseCursorPosition"
                           (ccl::load-framework-bundle "ApplicationServices.framework")))

    (defun move-cursor-to (x y)
      "Moves the cursor to absolute coordinates X Y."
      (ccl::ppc-ff-call *warp* 
                        :single-float (coerce x 'short-float)
                        :single-float (coerce y 'short-float)
                        :unsigned-fullword))
    ))

#+(and :ccl-5.0 (not :ccl-5.2))
(when (osx-p)
  (progn
    (defparameter *warp* (lookup-function-in-framework 
                           "CGWarpMouseCursorPosition"))
    (defun move-cursor-to (x y)
      "Moves the cursor to absolute coordinates X Y."
      (ccl::ppc-ff-call *warp* 
                        :single-float (coerce x 'short-float)
                        :single-float (coerce y 'short-float)
                        :unsigned-fullword))))




#+(and :ccl-4.3.1 (not :ccl-5.0))
(defun move-cursor-to (x y)
  "Moves the cursor to absolute coordinates X Y."
  (let ((tp (make-point x y)))
    (without-interrupts
      (#_LMSetMouseTemp tp)
      (#_LMSetRawMouseLocation tp)
      (#_LMSetCursorNew -1))))

#-(or ccl-4.3.1 :clozure)
(defun move-cursor-to (x y) 
  "Moves the cursor to absolute coordinates X Y."
  (let ((tp (make-point x y)))
    (without-interrupts
      (ccl::%put-point (%int-to-ptr #$MTemp) tp)
      (ccl::%put-point (%int-to-ptr #$RawMouse) tp)
      (%put-word (%int-to-ptr #$CrsrNew) -1))))

#+:clozure
(defun move-cursor-to (x y)
  (#_CGWarpMouseCursorPosition
   (ns:make-ns-point x y)))


;;;; ---------------------------------------------------------------------- ;;;;
;;;; bookkeeping

(provide :experiment-window4)
