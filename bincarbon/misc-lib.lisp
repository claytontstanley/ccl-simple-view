;;;  -*- mode: LISP; Package: CL-USER; Syntax: COMMON-LISP;  Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Author      : Mike Byrne
;;; Copyright   : (c)1999-2004 Rice/CMU/Mike Byrne, All Rights Reserved
;;; Availability: public domain
;;; Address     : Rice University
;;;             : Psychology Department
;;;             : Houston,TX 77251-1892
;;;             : byrne@acm.org
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Filename    : misc-lib.lisp
;;; Version     : r4
;;; 
;;; Description : Miscellaneous Lisp functions.
;;; 
;;; Bugs        : 
;;; 
;;; Todo        : 
;;; 
;;; ----- History -----
;;; 99.05.25 Mike Byrne
;;;             :  Added OBJ-MATCH-* functions and this header.
;;; 01.03.14 mdb (r2)
;;;             : Added GROUP function.
;;; 2003.12.05 mdb [r3]
;;;             : Added RANDOMIZE-VEC function, made RANDOM-ITEM not care about
;;;             : what kind of sequence it gets.
;;; 2003.12.12 mdb [r4]
;;;             : Put TAB-OUTPUT stuff here.
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(proclaim '(inline last1 single append1 conc1 mklist mkstr))
(proclaim '(optimize speed))

;;;; ---------------------------------------------------------------------- ;;;;
;;;;  Macros from "On Lisp"
;;;; ---------------------------------------------------------------------- ;;;;

(defmacro aif (test-form then-form &optional else-form)
  `(let ((it ,test-form))
     (if it ,then-form ,else-form)))

(defmacro awhen (test-form &body body)
  `(aif ,test-form
     (progn ,@body)))

(defmacro awhile (expr &body body)
  `(do ((it ,expr ,expr))
     ((not it))
     ,@body))

(defmacro while (test &body body)
  `(do ()
     ((not ,test))
     ,@body))


;;;; ---------------------------------------------------------------------- ;;;;
;;;;  Functions from "On Lisp"
;;;; ---------------------------------------------------------------------- ;;;;

(defun last1 (lst)
  (car (last lst)))


(defun single (lst)
  (and (consp lst) (not (cdr lst))))


(defun append1 (lst obj) 
  (append lst (list obj)))


(defun conc1 (lst obj)   
  (nconc lst (list obj)))


(defun mklist (obj)
  (if (listp obj) obj (list obj)))


(defun flatten (x)
  (labels ((rec (x acc)
             (cond ((null x) acc)
                   ((atom x) (cons x acc))
                   (t (rec (car x) (rec (cdr x) acc))))))
    (rec x nil))) 


(defun mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))


(defun explode (sym)
  (map 'list #'(lambda (c)
                 (intern (make-string 1 
                                      :initial-element c)))
       (symbol-name sym)))


(defun group (source n)
  (if (zerop n) (error "zero length"))
  (labels ((rec (source acc)
             (let ((rest (nthcdr n source)))
               (if (consp rest)
                 (rec rest (cons (subseq source 0 n) acc))
                 (nreverse (cons source acc))))))
    (if source (rec source nil) nil)))


;;;; ---------------------------------------------------------------------- ;;;;
;;;;  Other macros
;;;; ---------------------------------------------------------------------- ;;;;

(defmacro time-iter (iter &rest body)
  `(time (dotimes (i ,iter)
           ,@body)))

(defmacro d-append (l1 l2)
  `(setf ,l1 (append ,l1 (mklist ,l2))))

;;;; ---------------------------------------------------------------------- ;;;;
;;;;  Other functions
;;;; ---------------------------------------------------------------------- ;;;;

(defun flip ()
  "Randomly returns either T or NIL."
  (= 0 (random 2)))


(defun flipval (val1 val2)
  (if (flip)
    val1
    val2))


(defun randomize-list (in-list)
  "Randomly permute the items on a list"
  (let* ((the-list (copy-list in-list))
         (new-list nil)
         (start-len (length the-list))
         (current-len start-len)
         (the-item nil))
    (dotimes (i start-len new-list)
      (setf the-item (nth (random current-len) the-list))
      (push the-item new-list)
      (setf the-list (remove the-item the-list :count 1))
      (decf current-len))))



(defun randomize-vec (in-vec)
  "Randomly permute the items in a vector.  Destructive?"
  (let* ((start-len (length in-vec))
         (new-vec (make-array start-len))
         (current-len start-len)
         (the-item nil))
    (dotimes (i start-len new-vec)
      (setf the-item (svref in-vec (random current-len)))
      (setf (svref new-vec i) the-item)
      (setf in-vec (remove the-item in-vec :count 1))
      (decf current-len))))


(defgeneric random-item (seq)
  (:documentation "Returns a random item from a sequence."))

(defmethod random-item ((seq list))
  (nth (random (length seq)) seq))

(defmethod random-item ((seq simple-vector))
  (svref seq (random (length seq))))

(defmethod random-item ((seq sequence))
  (elt seq (random (length seq))))



;;;; ---------------------------------------------------------------------- ;;;;
;;;; Misc I/O functions
;;;; ---------------------------------------------------------------------- ;;;;

(defgeneric tab-output (thing &optional strm)
  (:documentation "Write the appropriately-tabbed representation of THING to STREAM."))


(defmethod tab-output ((thing list) &optional (strm t))
  (dolist (item thing)
    (tab-output item strm)))

(defmethod tab-output ((thing simple-vector) &optional (strm t))
  (dotimes (i (length thing))
    (tab-output (svref thing i) strm)))

(defmethod tab-output ((thing float) &optional (strm t))
  (format strm "~6,1F	" thing))

(defmethod tab-output (thing &optional (strm t))
  (format strm "~S	" thing)) 


;;;; ---------------------------------------------------------------------- ;;;;
;;;; Object matching functions
;;;; ---------------------------------------------------------------------- ;;;;

(defmethod objs-match-slotval ((ls list) (slot-name symbol) value)
  "Return list of objects from <ls> that match <value> on <slot-name>."
  (when ls
    (let (accum)
      (dolist (obj ls (nreverse accum))
        (when (equal value (slot-value obj slot-name))
          (push obj accum))))))


(defmethod objs-match-slotval ((ls list) (slot-name symbol) 
                                         (value number))
  "Return list of objects from <ls> that match <value> on <slot-name>."
  (when ls
    (let (accum)
      (dolist (obj ls (nreverse accum))
        (when (= value (slot-value obj slot-name))
          (push obj accum))))))


(defmethod objs-match-slotval ((ls list) (slot-name symbol) 
                                         (value symbol))
  "Return list of objects from <ls> that match <value> on <slot-name>."
  (when ls
    (let (accum)
      (dolist (obj ls (nreverse accum))
        (when (eq value (slot-value obj slot-name))
          (push obj accum))))))


(defmethod objs-min-slotval ((ls list) (slot-name symbol))
  "Return list of objects from <ls> that have minimum value on <slot-name>."
  (when ls
    (let ((best (slot-value (first ls) slot-name))
          (current nil)
          (out-ls (list (first ls))))
      (dolist (obj (rest ls) (nreverse out-ls))
        (setf current (slot-value obj slot-name))
        (cond ((= current best) (push obj out-ls))
              ((< current best) 
               (setf best current)
               (setf out-ls (list obj))))))))


(defmethod objs-max-slotval ((ls list) (slot-name symbol))
  "Return list of objects from <ls> that have maximum value on <slot-name>."
  (when ls
    (let ((best (slot-value (first ls) slot-name))
          (current nil)
          (out-ls (list (first ls))))
      (dolist (obj (rest ls) (nreverse out-ls))
        (setf current (slot-value obj slot-name))
        (cond ((= current best) (push obj out-ls))
              ((> current best) 
               (setf best current)
               (setf out-ls (list obj))))))))

;;;; ---------------------------------------------------------------------- ;;;;
;;;; bookkeeping

(provide :misc-lib)

