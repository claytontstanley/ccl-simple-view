;;;  -*- mode: LISP; Package: CL-USER; Syntax: COMMON-LISP;  Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Author      : Mike Byrne
;;; Copyright   : (c)1997-2000 Rice U./CMU/Mike Byrne, All Rights Reserved
;;; Availability: public domain
;;; Address     : Rice University
;;;             : Psychology Department
;;;             : Houston,TX 77251-1892
;;;             : byrne@acm.org
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Filename    : snd-player.lisp
;;; Version     : 1.0.1
;;; 
;;; Description : Defines a class, SND-PLAYER, that can be used to play
;;;             : Macintosh sounds.  You should have a resource file
;;;             : with a bunch of NAMED 'snd ' resources in it ready to go, 
;;;             : since that's what this class requires.  There are basically 
;;;             : three steps to using the SND-PLAYER class:
;;;
;;;             : [1] Create an instance, and be sure to bind it to something
;;;             : as you'll need to reference it later.  Something like:
;;;             : (setf *foo* (make-instance 'snd-player))
;;;             : This will cause a dialog box to pop up, which you should
;;;             : use to locate your sound file.
;;;             : You have to make a decision here, too, about memory usage.
;;;             : If you really need optimum performance (i.e. the sound is
;;;             : played as close in time as possible to when you issue the
;;;             : command), you will want to pre-load the sounds into memory.
;;;             : Make sure you have lots of memory, this can be a hog.
;;;             : To do this, pass a list of strings that match the names of
;;;             : the sounds you want to preload as a :preload parameter:
;;;             : (setf *foo* 
;;;             :       (make-instance 'snd-player :preload '("a" "b")))
;;;             : This would preload the sounds named "a" and "b" in your
;;;             : resource file.
;;;
;;;             : [2] Play sounds.  The method is called PLAY-SND and takes
;;;             : two arguments + one optional argument.  The first argument
;;;             : is the sound player, the second is the sound name (a string),
;;;             : and the optional third is either T or NIL, a flag for 
;;;             : synchronous or asynchronous; the default is NIL.  Thus:
;;;             : (play-snd *foo* "b" t)
;;;             : tells the player *FOO* to play the sound named "b" 
;;;             : asynchronously.
;;;
;;;             : [3] Release the sound player.  This is _REALLY_ important.
;;;             : When you're done with the sound player, you MUST release it.
;;;             : The player is holding on to some system resources and 
;;;             : possibly a lot of memory, all of which must be restored.
;;;             : The call is a simple one, like:
;;;             : (release-player *foo*)
;;;             : but don't forget to do it.  Crashing or other strange 
;;;             : behavior may result.  You've been warned.
;;;
;;; 
;;; History
;;; 97.01.21 Mike Byrne
;;;             : Initial writing and some testing, everything seems to be
;;;             : up and working fine.
;;; 00.09.17 mdb
;;;             : Tweaked it so that it won't prompt for a resource file if
;;;             : you don't need one.
;;; 
;;; Bugs        : None known yet, but it's still early.
;;; 
;;; Todo        : Use a hash table instead of an a-list.
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+:clozure (require :resources)

;;; CHECK-MAC-ERROR      [Macro]
;;; Description : Little macro to wrap around functions that return Macintosh
;;;             : Error codes.  If an error value (i.e. non-zero) is returns,
;;;             : causes a Lisp error with the error code.

(defmacro check-mac-error (form)
  "Checks a Macintosh error code"
  `(let ((mac-error ,form))
     (when (not (= mac-error 0))
       (error "MacOS error ~A" mac-error))))


;;; SND-PLAYER      [Class]
;;; Date        : 97.01.21
;;; Description : The class for the sound player itself.  Includes slots for
;;;             : the resource file, the sound channel, and lists of sound
;;;             : names (and the corresponding association list).  The 
;;;             : resource file is found and opened by the resource file
;;;             : initform.

(defclass snd-player ()
  ((res-file :accessor res-file :initarg :res-file 
             :initform (open-resource-file 
                        (choose-file-dialog :mac-file-type "RSRC"
                                            :button-string "Sounds")))
   (the-channel :accessor the-channel)
   (preload :accessor preload :initarg :preload :initform nil)
   (snd-alis :accessor snd-alis :initform nil)))


;;; INITIALIZE-INSTANCE      [Method]
;;; Date        : 97.01.21
;;; Description : When a SND-PLAYER object is created, several things
;;;             : have to happen.  First, a sound channel should be
;;;             : allocated in case it is required for async playback.
;;;             : Then, if the sounds are to be pre-loaded, walk through the
;;;             : supplied list of names and build a cons for the name and
;;;             : resource handle.  Theses conses are built into a list
;;;             : for later lookup with assoc.

#+:digitool
(defmethod initialize-instance :after ((self snd-player) &key)
  (declare (ignore a))
  (setf (the-channel self) (make-record :sndchannel :qLength 16))
  (%stack-block ((hndl 4))
    (%put-ptr hndl (the-channel self))
    (check-mac-error
     (#_SndNewChannel hndl #$sampledSynth #$initMono (%null-ptr))))
  (when (preload self)
    (dolist (name (preload self) nil)
      (push (cons name (with-pstrs ((pname name))
                         (#_GetNamedResource "snd " pname)))
            (snd-alis self)))))

#+:clozure
(defmethod initialize-instance :after ((self snd-player) &key)
  (unless (and (slot-boundp self 'snd-alis) (not (null (snd-alis self))))
    ; Cross over to resources.lisp and grab the default resource pool
    (setf (snd-alis self) *pool*))
  (when (preload self)
    (dolist (name (preload self) nil)
      (get-resource-val name 'sound (snd-alis self)))))

;;; RELEASE-PLAYER      [Method]
;;; Date        : 97.01.21
;;; Description : When finished with the sound player, it must be disposed
;;;             : of properly.  This means disposing of the sound channel,
;;;             : releasing and disposing any 'snd' handles, and closing
;;;             : the resource file.  A terrific argument for destructors
;;;             : in CLOS.

#+:digitool
(defmethod release-player ((player snd-player))
  "Does the REQUIRED clean-up after finishing with a sound player."
  (#_SndDisposeChannel (the-channel player) nil)
  (when (snd-alis player)
    (dolist (snd-assoc (snd-alis player))
      (#_HUnlock (rest snd-assoc))
      (#_DisposeHandle (rest snd-assoc))))
  (when (res-file player)
    (close-resource-file (res-file player))))

;;; PLAY-SND      [Method]
;;; Date        : 97.01.21
;;; Description : Plays a sound.  If we're a preloaded sound name, then it
;;;             : just gets the sound handle with an ASSOC lookup.  If not,
;;;             : then it gets the sound handle via a call to GetResource.
;;;             : ASSOC ought to be much faster.

#+:digitool
(defmethod play-snd ((player snd-player) snd-name &optional async)
  "Tells the given sound player to play the named sound with the supplied mode"
  (if (member snd-name (preload player))
    (check-mac-error 
     (#_SndPlay (the-channel player)
      (rest (assoc snd-name (snd-alis player))) async))
    (check-mac-error 
     (#_SndPlay (the-channel player)
      (with-pstrs ((pname snd-name))
        (#_GetNamedResource "snd " pname)) async))))

#+:clozure
(defmethod play-snd ((player snd-player) snd-name &optional async)
  (let ((snd (get-resource-val snd-name 'sound (snd-alis player))))
    (#/play snd)
    (unless async
      (process-wait
        (format nil "waiting for sound ~a to finish" snd)
        (lambda ()
          (not (#/isPlaying snd)))))))

;;;; ---------------------------------------------------------------------- ;;;;
;;;;  bookkeeping

(provide :snd-player)
