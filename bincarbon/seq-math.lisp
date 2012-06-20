;;;  -*- mode: LISP; Package: CL-USER; Syntax: COMMON-LISP;  Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Author      : Mike Byrne
;;; Copyright   : (c)2003-6 Rice U./Mike Byrne, All Rights Reserved
;;; Availability: public domain
;;; Address     : Rice University
;;;             : Psychology Department
;;;             : Houston,TX 77251-1892
;;;             : byrne@acm.org
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Filename    : seq-math.lisp
;;; Version     : r4
;;; 
;;; Description : Functions/methods for doing math on sequences and pairs of
;;;             : sequences.
;;;             : Note this combines old "list-math.lisp" and "fit-metrics.lisp"
;;;             : files. 
;;; 
;;; Bugs        : 
;;; 
;;; Todo        : 
;;; 
;;; ----- History -----
;;; 2006.02.28 mdb [r4]
;;;             : Old bug in SEQ-MEDIAN _should_ be fixed.  Weird regression.
;;; 2004.11.03 mdb [r3]
;;;             : Fixed bug in list-based seq-mean-stdev code.
;;; 2003.11.29 mdb [r2]
;;;             : Added CHI-SQUARED and supporting functions.
;;; 03.04.11 Mike Byrne
;;;             :  Incept date.
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; ---------------------------------------------------------------------- ;;;;
;;;; Single-sequence functions
;;;; ---------------------------------------------------------------------- ;;;;

(defgeneric seq-mean (seq &key key)
  (:documentation "Returns the mean of a sequence of numbers.  Returns NIL when passed an empty sequence."))


(defmethod seq-mean ((seq vector) &key key)
  (let ((accum 0) (len (length seq)))
    (unless (zerop len)
      (dotimes (i len (float (/ accum len)))
        (incf accum (if key (funcall key (svref seq i))
                        (svref seq i)))))))

(defmethod seq-mean ((seq list) &key key)
  (unless (null seq)
    (let ((accum 0) (len 0))
      (dolist (item seq (float (/ accum len)))
        (incf accum (if key (funcall key item) item))
        (incf len)))))


(defun list-slot-mean (lst slot-name)
  (seq-mean lst :key #'(lambda (obj) (slot-value obj slot-name))))


(defgeneric seq-mean-stdev (seq &key key)
  (:documentation "Returns the mean and the standard deviation of a sequence of numbers.  Returns nil when passed an empty sequence."))


(defmethod seq-mean-stdev ((seq vector) &key key)
  (let ((accum 0) (len (length seq)) (sumsq 0) tmp mean)
    (unless (zerop len)
      (dotimes (i len)
        (setf tmp (if key (funcall key (svref seq i)) (svref seq i)))
        (incf accum tmp)
        (incf sumsq (* tmp tmp)))
      (setf mean (float (/ accum len)))
      (values mean
              (when (> len 1)
                (sqrt (/ (- sumsq (* len (* mean mean))) (- len 1))))))))


(defmethod seq-mean-stdev ((seq list) &key key)
  (unless (null seq)
    (let ((accum 0) (len 0) (sumsq 0) mean)
      (dolist (item seq)
        (when key (setf item (funcall key item)))
        (incf accum item)
        (incf sumsq (* item item))
        (incf len))
      (setf mean (float (/ accum len)))
      (values mean
              (when (> len 1) 
                (sqrt (/ (- sumsq (* len (* mean mean))) (- len 1))))))))

(defmethod seq-mean-biasstdev ((seq list) &key key)
  (unless (null seq)
    (let ((accum 0) (len 0) (sumsq 0) mean)
      (dolist (item seq)
        (incf accum (if key (funcall key item) item))
        (incf sumsq (* item item))
        (incf len))
      (setf mean (float (/ accum len)))
      (values mean
              (when (> len 1) 
                (sqrt (/ (- sumsq (* len (* mean mean))) len)))))))

(defgeneric seq-mean-stderr (seq &key key)
  (:documentation "Returns the mean and the standard error of a sequence of numbers.  Returns nil when passed an empty sequence."))


(defmethod seq-mean-stderr ((seq vector) &key key)
  "Returns the mean and the standard deviation of a list of numbers."
  (let ((accum 0) (len (length seq)) (sumsq 0) tmp mean)
    (unless (zerop len)
      (dotimes (i len)
        (setf tmp (if key (funcall key (svref seq i)) (svref seq i)))
        (incf accum tmp)
        (incf sumsq (* tmp tmp)))
      (setf mean (float (/ accum len)))
      (values mean
              (when (> len 1)
                (/ (sqrt (/ (- sumsq (* len (* mean mean))) (- len 1)))
                   (sqrt len)))))))


(defmethod seq-mean-stderr ((seq list) &key key)
  (unless (null seq)
    (let ((accum 0) (len 0) (sumsq 0) mean)
      (dolist (item seq)
        (incf accum (if key (funcall key item) item))
        (incf sumsq (* item item))
        (incf len))
      (setf mean (float (/ accum len)))
      (values mean
               (when (> len 1)
                (/ (sqrt (/ (- sumsq (* len (* mean mean))) (- len 1)))
                   (sqrt len)))))))


(defgeneric seq-median (seq &key key)
  (:documentation "Returns the median of a sequence of numbers."))


(defmethod seq-median ((seq vector) &key key)
  (let ((newvec (sort (copy-seq seq) #'< :key key))
        (len (length seq)))
    (if (oddp len)
      (svref (floor (/ len 2)) newvec)
      (float (/ (+ (svref (1- (/ len 2)) newvec)
                   (svref (/ len 2) newvec))
                2)))))


(defmethod seq-median ((seq list) &key key)
  "Returns the median of a list of numbers."
  (let ((newlis (sort (copy-seq seq) #'< :key key))
        (len (length seq)))
    (if (oddp len)
      (nth (floor (/ len 2)) newlis)
      (float (/ (+ (nth (1- (/ len 2)) newlis)
                   (nth (/ len 2) newlis))
                2)))))


(defun make-bounder (min max &key key)
  (if (< max min)
    (error "MAX less than MIN in MAKE-BOUNDER.")
    (if key
      (lambda (thing)
        (or (> (funcall key thing) max) 
            (< (funcall key thing) min)))
      (lambda (n)
        (or (> n max) (< n min))))))


(defun filter-bounds (seq min max &key key)
  "Removes items from a list less than the min and greater than the max."
  (remove-if (make-bounder min max :key key) seq))


(defun filter-slot-bounds (seq min max slotname)
  (remove-if (make-bounder min max :key #'(lambda (obj) 
                                            (slot-value obj slotname)))
             seq))


(defun stdev-bounds (seq factor &key key)
  "Will return max and min bounds based on <factor> standard deviations from the mean."
  (multiple-value-bind (mean dev) (seq-mean-stdev seq :key key)
    (if dev
      (values (- mean (* factor dev)) (+ mean (* factor dev)))
      (values nil nil))))


(defun stdev-slot-bounds (seq factor slotname)
  (stdev-bounds seq factor :key #'(lambda (obj) (slot-value obj slotname))))



(defun remove-outliers (seq factor &key key)
  "Returns a seqence with entries greater or less than <factor> times the SD removed."
  (multiple-value-bind (min max) (stdev-bounds seq factor :key key)
    (if min
      (filter-bounds seq min max :key key)
      seq)))


(defun remove-slot-outliers (seq factor slotname)
  (remove-outliers seq factor :key #'(lambda (obj) (slot-value obj slotname))))


(defun singsamp-tval (seq &optional (testval 0))
  (multiple-value-bind (mean stdev) (seq-mean-stdev seq)
    (float (/ (- mean testval) (/ stdev (sqrt (length seq)))))))



(defun trim-seq (seq prop &key key)
  (let ((newseq (sort (copy-seq seq) #'< :key key))
        (nclip (floor (* prop (length seq)) 2)))
    (subseq newseq nclip (- (length seq) nclip))))




;;;; ---------------------------------------------------------------------- ;;;;
;;;; Dual-sequence functions
;;;; ---------------------------------------------------------------------- ;;;;

(defgeneric avg-over-pairs (fn s1 s2 &key key)
  (:documentation "Applies a function to pairs of items from two vectors, returning the average value of the function."))

(defmethod avg-over-pairs ((fn function) (s1 vector) (s2 vector) &key key)
  (let ((len (length s1))
        (accum 0))
    (unless (= len (length s2))
      (error "Sequences of unequal length passed to AVG-OVER-PAIRS."))
    (dotimes (i len (float (/ accum len)))
      (if key
        (incf accum (funcall fn (funcall key (svref s1 i))
                             (funcall key (svref s2 i))))
        (incf accum (funcall fn (svref s1 i) (svref s2 i)))))))


(defmethod avg-over-pairs ((fn function) (s1 sequence) (s2 sequence) 
                              &key key)
  (let ((len (length s1))
        (accum 0))
    (unless (= len (length s2))
      (error "Sequences of unequal length passed to AVG-OVER-PAIRS."))
    (dotimes (i len (float (/ accum len)))
      (if key
        (incf accum (funcall fn (funcall key (elt s1 i))
                             (funcall key (elt s2 i))))
        (incf accum (funcall fn (elt s1 i) (elt s2 i)))))))


(defgeneric indep-tval (seq1 seq2)
  (:documentation "Return the t-value [t-test] for the two sequences, assuming independence."))

(defmethod indep-tval ((seq1 sequence) (seq2 sequence))
  (multiple-value-bind (m1 sd1) (seq-mean-stdev seq1)
    (multiple-value-bind (m2 sd2) (seq-mean-stdev seq2)
      (let ((n1 (length seq1)) (n2 (length seq2)))
        (/ (- m1 m2)
           (sqrt
            (* (+ (/ 1 n1) (/ 1 n2))
               (/ (+ (* (1- n1) sd1 sd1) (* (1- n2) sd2 sd2))
                  (+ n1 n2 -2)))))))))
            



(defun mean-abs-dev (s1 s2 &key key)
  "Returns the mean absolute deviation between two vectors."
  (avg-over-pairs #'absdev s1 s2 :key key))

(defun mean-abs-dev-pct (obs preds &key key)
  "Returns the mean absolute deviation as a percentage of the observed, for two vectors."
  (avg-over-pairs #'pct-abs-dev obs preds :key key))

(defun rmsd (s1 s2 &key key)
  "Returns the RMS deviation between two vectors of numbers."
  (sqrt (avg-over-pairs #'sqr-dev s1 s2 :key key)))

(defun rms%d (ob pred &key key)
  "Returns the RMS percentage deviation between two vectors of numbers."
  (sqrt (avg-over-pairs #'sqr-pct-dev ob pred :key key)))


(defun chi-squared (obseq exseq &key key)
  "Returns the chi-squared between sequences representing observed and expected values."
  (* (avg-over-pairs #'chi-sq obseq exseq :key key)
     (length obseq)))


(defgeneric covar (xseq yseq &key key)
  (:documentation "Returns the covariance between two sequences."))

(defmethod covar ((xseq vector)  (yseq vector) &key key)
  (let ((xacm 0) (yacm 0) (prodacm 0)
        (len (length xseq)) xval yval)
    (unless (= len (length yseq))
      (error "Unequal sequence lengths passed to COVAR."))
    (dotimes (i len)
      (setf xval (if key (funcall key (svref xseq i)) (svref xseq i))
            yval (if key (funcall key (svref yseq i)) (svref yseq i)))
      (incf xacm xval)
      (incf yacm yval)
      (incf prodacm (* xval yval)))
    (/ (- prodacm (/ (* xacm yacm) len))
       (- len 1))))

(defmethod covar ((xseq sequence)  (yseq sequence) &key key)
  (let ((xacm 0) (yacm 0) (prodacm 0)
        (len (length xseq)) xval yval)
    (unless (= len (length yseq))
      (error "Unequal sequence lengths passed to COVAR."))
    (dotimes (i len)
      (setf xval (if key (funcall key (elt xseq i)) (elt xseq i))
            yval (if key (funcall key (elt yseq i)) (elt yseq i)))
      (incf xacm xval)
      (incf yacm yval)
      (incf prodacm (* xval yval)))
    (/ (- prodacm (/ (* xacm yacm) len))
       (- len 1))))


(defgeneric r-squared (xseq yseq &key key)
  (:documentation "Takes two sequences of numbers, returns three things:  the r-squared, the slope of the regression line, and the intercept of the regression line."))

(defmethod r-squared ((xseq sequence) (yseq sequence) &key key)
  (let ((covar (covar xseq yseq :key key))
        slope)
    (multiple-value-bind (xmean xstd)
                         (seq-mean-stdev xseq :key key)
      (multiple-value-bind (ymean ystd)
                           (seq-mean-stdev yseq :key key)
        (setf slope (/ covar (* xstd xstd)))
        (values (expt (/ covar (* xstd ystd)) 2) 
                slope 
                (- ymean (* slope xmean)))))))



;;;; ---------------------------------------------------------------------- ;;;;
;;;; Helper functions on pairs of numbers

(defun absdev (x y)
  "Return the absolute deviation between two numbers."
  (abs (- x y)))

(defun pct-abs-dev (ob pred)
  "Returns the absolute deviation as a percentage of the observation."
  (* (/ (abs (- ob pred)) ob) 100))

(defun sqr-dev (x y)
  "Returns the squared deviation between two numbers."
  (expt (- x y) 2))

(defun sqr-pct-dev (ob pred)
  "Returns the square of the percentage devation between two numbers."
  (expt (* (/ (- ob pred) ob) 100) 2))


(defun chi-sq (ob exp)
  "Returns the chi-square value for a pair of numbers, observed and expected."
  (/ (expt (- ob exp) 2) exp))

;;;; ---------------------------------------------------------------------- ;;;;
;;;; Multi-sequence functions 

(defgeneric across-seqs (tseq dseqs fn &key key)
  (:documentation "FN is a function (keyed) on two sequences.  This computes FN taking TSEQ paired with each entry in DSEQS and returns a list of the results."))

(defmethod across-seqs ((tseq sequence) (dseqs sequence) (fn function) &key key)
  (let (accum)
    (dotimes (i (length dseqs) (nreverse accum))
      (push (funcall fn tseq (elt dseqs i) :key key) accum))))




;;;; --------------------------------------------------------------------- ;;;;
;;;; Bookkeeping


(provide :seq-math)
(provide :list-math)
(provide :fit-metrics)


;;;; ---------------------------------------------------------------------- ;;;;
;;;; test code

#|
(setf obs
      #(2402.2 1833.337 1482.289 1258.448 1134.406 1003.989 1054.524 948.765 
        711.52 740.897 722.518 783.511 828.649 898.92 801.633 881.023))
(setf preds 
      #(2250 1550 1150 1000 900 850 800 750 450 479 508 537 566 595 624 653))

(mean-abs-dev obs preds)
(mean-abs-dev-pct obs preds)
(rmsd obs preds)
(rms%d obs preds)
(r-squared obs preds)

#|
Spreadsheet computations
MAD:		239.04
MA%D:		 24.5
RMSD:		243.96
RMS%D:		 25.8
r-squared:	0.989
|#


(setf xseq
      #(70 67 72 75 68 69 71.5 71 72 69 67 68 66 72 73.5 73 69 73 72 74 72 71 74 72
        70 67 71 72 69 73 74 66 71 70 70 75 74 71 69 70 72 67 69 73 73 71 68 69.5 73
        75 66 69 66 73 68 74 73.5))
(setf yseq
      #(150 140 180 190 145 150 164 140 142 136 123 155 140 145 160 190 155 165 150
        190 195 138 160 155 153 145 170 175 175 170 180 135 170 157 130 185 190 155
        170 155 215 150 145 155 155 150 155 150 180 160 135 160 130 155 150 148 155))

(r-squared xseq yseq)

#|
SPSS results:
r-squared:	0.364
slope:		4.356
intercept:	-149.934
|#
|#
