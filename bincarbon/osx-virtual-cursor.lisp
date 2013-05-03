;;;  -*- mode: LISP; Package: CL-USER; Syntax: COMMON-LISP;  Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Author      : Mike Byrne
;;; Copyright   : (c)2002-9 Rice U./Mike Byrne, All Rights Reserved
;;; Availability: public domain
;;; Address     : Rice University
;;;             : Psychology Department
;;;             : Houston,TX 77251-1892
;;;             : byrne@acm.org
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Filename    : osx-virtual-cursor.lisp
;;; Version     : r3
;;; 
;;; Description : Provides a view-based virtual cursor for RPM.  This appears
;;;             : to work differently in OS X, where so far it doesn't seem
;;;             : necessary to actualy move the real mouse cursor to get 
;;;             : clicks to process correctly.
;;;
;;; Bugs        : Cosmetic:  the virtual cursor isn't very pretty. 
;;;             : Functional: This won't generate MOUSE-ENTER events. 
;;;             : Maybe only cosmetic:  For clicks to be processed correctly,
;;;             : the relevant window must be the frontmost.  So 
;;;             : DEVICE-HANDLE-CLICK actually ensures that this is the case.
;;; 
;;; ----- History -----
;;; 2009.05.29 mdb [r3]
;;;             : Now plays nicely with ACT6.
;;; 2004.04.03 mdb [r2]
;;;             : * Now uses RPM-OVERLAY class.
;;;             : * Fixed calls to CURRENT-CURSOR.
;;; 2002.11.13 mdb
;;;             :  Incept date.
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *virtual-cursor* nil "The virtual cursor.")


(defclass virtual-cursor (rpm-overlay)
  ()
  (:default-initargs
    :view-size #@(10 10)
    ))

;;; VIEW-DRAW-CONTENTS      [Method]
;;; Description :  Draws a green triangle.

(defmethod view-draw-contents ((self virtual-cursor))
  (let ((oldmode (pen-mode self))
        (oldpat (pen-pattern self))
        (oldsize (pen-size self)))
    (set-pen-mode self :pator) 
    (set-pen-pattern self *black-pattern*)
    (set-pen-size self 2 2)
    (with-focused-view self
      (with-fore-color *green-color*    ; change this to change the color 
        (move-to self 0 0)
        (line-to self 8 3)
        (line-to self 3 8)
        (line-to self 0 0)
        (line-to self 10 10)
        ))
    (set-pen-mode self oldmode)
    (set-pen-pattern self oldpat)
    (set-pen-size self (point-h oldsize) (point-v oldsize))
    ))


(defmethod cursor-to-feature ((wind window))
  (when (equal (view-window *virtual-cursor*) wind)
    ; (let ((loc (true-cursor-loc (device-interface *mp*))))  delta for act6
    (let ((loc (true-cursor-loc (current-device))))
      (make-instance 'cursor-feature
                     :x (px loc) :y (py loc) :value 'POINTER))))


;;; device-level stuff


;;; DEVICE-MOVE-CURSOR-TO      [Method]
;;; Description : Moving the cursor is just moving the view and redrawing.
;;;             : The only problem is that the whole window has to redraw,
;;;             : rather than just the cursor, because asking just the 
;;;             : cursor to redraw often bombs.

; Currently not simulating the mouse click in CCL, so make this an after method
; if CCL, in order to update the location of the virtual cursor.

(defmethod device-move-cursor-to #+:clozure :after ((device window) (xyloc vector))
  (when (and device (wptr device)) (window-select device))
  (set-view-position *virtual-cursor* (px xyloc) (py xyloc))
  (unless (equal (view-window *virtual-cursor*) device)
    (add-subviews device *virtual-cursor*))
  (event-dispatch)
  (view-draw-contents (view-window *virtual-cursor*))  ; *virtual-cursor* doesn't work.
  (window-update-cursor device (vpt2p xyloc))
  ;(event-dispatch)
  xyloc)



(defmethod device-move-cursor-to ((device window) (xyloc list))
  (device-move-cursor-to device (coerce xyloc 'vector)))


(defmethod get-mouse-coordinates ((device window))
  (p2vpt (view-position *virtual-cursor*)))


;;; DEVICE-HANDLE-CLICK      [Method]
;;; Description : Simply call VIEW-CLICK-EVENT-HANDLER.  Something somewhere
;;;             : in that hierarchy the window needs to be on top for this 
;;;             : to work properly, so do a WINDOW-SELECT first.

; Currently not simulating the mouse click in CCL, so only write over this method if MCL

#+:digitool
(defmethod device-handle-click ((device window))
  (window-select device)
  (view-click-event-handler device (view-position *virtual-cursor*))
  (event-dispatch)
  )

(defmethod device-update :after ((device window) time)
  (declare (ignore time))
  ; (pm-set-cursor-position-fct (true-cursor-loc (device-interface *mp*)))  delta for act6
  (set-cursor-position-fct (true-cursor-loc (current-device-interface)))
  (unless (equal (view-window *virtual-cursor*) device)
    (add-subviews device *virtual-cursor*)))


(eval-when (load eval)
  (setf *virtual-cursor* (make-instance 'virtual-cursor
                                        :view-position #@(0 0))))


#|
;; some test code I've been using

(defun test-click (i)
  (let ((wind (device (device-interface *mp*))))
    (device-move-cursor-to wind (view-loc (nth i (subviews wind))))
    (device-handle-click wind)))
|#
