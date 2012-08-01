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
;;; Filename    : pict-svm.lisp
;;; Version     : r2
;;; 
;;; Description : Provides PICT drawing within views in a way that works with
;;;             : MCL 5.0 under Mac OS X.  Works much like the OOU stuff, 
;;;             : except all PICTs must be avaliable as resources, and it 
;;;             : caches the handles for all of them, requiring an explicit
;;;             : release.  Also, all PICTs are scaled to the dimensions of 
;;;             : the view.
;;;
;;;             : Note that when you define subclasses, PICT-SVM should be the
;;;             : FIRST subclass listed, ahead of any view-based classes, so
;;;             : that the view-draw-contents gets called in the right order.
;;; 
;;; Bugs        : 
;;; 
;;; Todo        : 
;;; 
;;; ----- History -----
;;; 03.01.23 Mike Byrne
;;;             :  Incept date.
;;; 2003.04.21 mdb [r2]
;;;             : Small mod so that the draw code can be called more
;;;             : directly. 
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; PICT handle cache

#+:digitool
(defparameter *pict-h-alist* nil "Stores the PICT handles.")

#+:digitool
(defun release-picts ()
  "Releases all the PICT handles in *pict-h-alist*."
  (dolist (pa *pict-h-alist*)
    (#_ReleaseResource (rest pa)))
  (setf *pict-h-alist* nil))

#+:digitool
(defun pid-to-h (pictid)
  "Given a PICT id [either a number or a string], return a handle."
  (let ((ph (rest (assoc pictid *pict-h-alist* :test #'equal))))
    (when ph
      (return-from pid-to-h ph))
    (setf ph (#_GetPicture 
              (if (numberp pictid) pictid (get-resource-id "PICT" pictid))))
    (if (or (null ph) (%null-ptr-p ph))
      (error "~S does not specify a valid PICT resource." pictid)
      (progn
        (push (cons pictid ph) *pict-h-alist*)
        ph))))

;;; PICT-SVM      [Class]
;;; Description : The main class.  Build your subclasses based on this.

#+:digitool
(defclass pict-svm ()
  ((pict-id :accessor pict-id :initarg :pict-id :initform nil)
   )
  )

#+:clozure
(defclass pict-svm (image-view-mixin) ())

#+:digitool
(defmethod draw-pict ((sv pict-svm))
  (when (pict-id sv)
    (with-focused-view (view-window sv)
      (multiple-value-bind (topLeft botRight) (view-corners sv)
        (rlet ((r :Rect :topLeft topLeft :botRight botRight))
          (#_DrawPicture (pid-to-h (pict-id sv)) r)
          )))
    ))

#+:clozure
(defmethod draw-pict ((sv pict-svm)) ())

#+:digitool
(defmethod view-draw-contents ((sv pict-svm))
  (draw-pict sv)
  (call-next-method))

(defgeneric set-view-pict (sv id)
  (:documentation "Sets the ID used for that view, also causes the view to be drawn."))

#+:digitool
(defmethod set-view-pict ((sv pict-svm) id)
  (setf (pict-id sv) id)
  (draw-pict sv)
  )

#+:clozure
(defmethod set-view-pict ((sv pict-svm) id)
  (setf (pict-id sv) id))

;;;; utilites for managing resources. 

#+:digitool
(defmacro without-res-load (&body body)
  `(unwind-protect
     (progn
       (require-trap #_SetResLoad nil)
       ,@body)
     (require-trap #_SetResLoad t)))

#+:digitool
(defun get-resource-id (rsrc-type rsrc-name &key (errorp t))
  (with-returned-pstrs ((name_p rsrc-name))
    (rlet ((id_p :integer)
           (type_p :OSType))
      (with-macptrs ((rsrc_h 
                      (without-res-load 
                        (#_GetNamedResource rsrc-type name_p))))
        (without-interrupts
         (#_GetResInfo rsrc_h id_p type_p name_p)
         (when (and errorp (not (zerop (#_ResError))))
           (error "resource ~s of type ~s not found." rsrc-name rsrc-type))))
      (%get-signed-word id_p))))

#|

Here's an example.

(defparameter *refnum* (open-resource-file (choose-file-dialog)))

(defclass pwind (pict-svm window)
  ()
  (:default-initargs
    :grow-icon-p nil
    :view-size #@(800 600)))

(setf pwind (make-instance 'pwind))

(set-view-pict pwind "phaser")

(defclass pdi (pict-svm dialog-item)
  ()
  (:default-initargs
    :view-size #@(32 32)
    :view-position #@(200 100)))

(setf p1 (make-instance 'pdi))
;(set-dialog-item-text p1 "foo")
;(set-dialog-item-action-function p1 #'(lambda (s) (beep)))

(defmethod view-click-event-handler ((self pdi) where)
  (declare (ignore where))
  (print "click")
  (beep))

(set-view-pict p1 "12")

(add-subviews pwind p1)

;; when done, be sure to close the resource file:
;; (close-resource-file *refnum*) 
|#


;;; bookkeeping

(provide :pict-svm)
