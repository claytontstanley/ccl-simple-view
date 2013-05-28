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
;;; Filename    : visual-salience.lisp
;;; Version     : 1.0b1
;;; 
;;; Description : Modifies ACT-R's vision module to use salience computation
;;;             : to drive the results of +visual-location> requests.
;;; 
;;; Bugs        : Probably many.
;;; 
;;; Todo        : * Add switch to limit salience to things near CLOF.
;;;             : * Generate new vision module class with parameters which
;;;             :   can be set via SGP.  [later]
;;;             : * Probably some weird interaction with buffer stuffing.
;;; 
;;; ----- History -----
;;; 2006.07.19 mdb [b1]
;;;             : * Misc cleanup for public release.
;;; 2006.07.18 mdb [a2]
;;;             : * Added support for hook functions for Sji computation.
;;;             : * Removed library dependencies.
;;; 06.07.07 mdb [a1]
;;;             :  Started to bother with version numbering.
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+:packaged-actr (in-package :act-r)
#+(and :clean-actr (not :packaged-actr) :ALLEGRO-IDE) (in-package :cg-user)
#-(or (not :clean-actr) :packaged-actr :ALLEGRO-IDE) (in-package :cl-user)

;;; Adds parameters needed for salience computation to all chunks (abstracted from user)
(extend-chunks bu-salience :default-value nil)
(extend-chunks td-salience :default-value nil)
(extend-chunks tot-salience :default-value nil)

;;; All these parameters really should be stored in a slot in the vision module
(defparameter *sval-caches* nil)
(defparameter *salience-slot-gammas* 
  '((size . 0.3) (color . 0.30) (value . 0.40)))
(defparameter *salience-noise* 0.2)
(defparameter *salience-thresh* 0)
(defparameter *salience-source-act* 1)
(defparameter *max-salience-sji* 1)
(defparameter *salience-sji-hook* nil)

(defun pre-noise-salience (x)
  "Compute bu plus td activation w/o noise"
  (+ (chunk-bu-salience x) (chunk-td-salience x)))

(defun effective-salience (x)
  "Salience level must at least be that of bu activation"
  (if (< (chunk-tot-salience x) (chunk-bu-salience x))
    (chunk-bu-salience x)
    (chunk-tot-salience x)))

;;;; ---------------------------------------------------------------------- ;;;;
;;;; Bottom-up salience
;;;; ---------------------------------------------------------------------- ;;;;
(defun compute-bottom-up-salience (lst)
  "Compute bottom-up salience for a list of features."
  (setf *sval-caches* nil)
  (mapc #'(lambda (x) (setf (chunk-bu-salience x) 0)) lst)
  (let ((nfeats (length lst)))
    (dolist (slotname (mapcar #'car *salience-slot-gammas*) lst)
      ;; if we're going to clip to range of POG, do it here
      (let ((val-lst (mapcar #'(lambda (x) 
                                 (chunk-slot-value-fct x slotname)) lst)))
        (if (not (every #'numberp val-lst))
          (process-symbolic-slot lst slotname nfeats)
          (process-numeric-slot lst slotname nfeats))))))

(defun process-numeric-slot (lst slotname nfeats)
  "Increment bottom-up salience for a slot with all numeric values."
  (let ((z-scores (z-trans 
                    lst :key #'(lambda (x) (chunk-slot-value-fct x slotname)))))
    (when z-scores
      (dotimes (i nfeats lst)
        (incf (chunk-bu-salience (nth i lst))
              (* (rest (assoc slotname *salience-slot-gammas*))
                 (prob->bits (z->prob (nth i z-scores)))))))))

(defun process-symbolic-slot (lst slotname nfeats)
  "Increment bottom-salience for a slot with not all numeric values."
  (let ((val-alst nil) 
        (featval nil))
    (dolist (x lst)
      (setf featval (chunk-slot-value-fct x slotname))
      (when (stringp featval) (setf featval :text));; treat all strings as :TEXT
      (aif (assoc featval val-alst)
        (push x (rest it))
        (push (cons featval (list x)) val-alst)))
    (push (cons slotname val-alst) *sval-caches*)
    (dolist (fval val-alst)
      (let ((prob (/ (1- (length fval)) nfeats)))
        (mapc #'(lambda (x)
                  (incf (chunk-bu-salience x)
                        (* (rest (assoc slotname *salience-slot-gammas*))
                           (prob->bits prob)))) (rest fval))))))

(defmethod visicon-update :after ((vis-mod vision-module))
  "Resets activation and stuffs visloc-buffer"
  ;; again, restrict to some area of CLOF?
  (compute-bottom-up-salience (visicon-chunks vis-mod))
  (mapc #'(lambda (x) (setf (chunk-td-salience x) 0)) (visicon-chunks vis-mod))
  (mapc #'(lambda (x) (setf (chunk-tot-salience x)
                            (chunk-bu-salience x))) (visicon-chunks vis-mod))
  (stuff-visloc-buffer vis-mod))

;;;; ---------------------------------------------------------------------- ;;;;
;;;; Top-down salience
;;;; ---------------------------------------------------------------------- ;;;;
(defmethod find-location ((vis-mod vision-module) chunk-spec)
  "Calculates and returns most salient object on the screen if that object is above the threshold"
  (update-new vis-mod)
  (check-finsts vis-mod)
  (mapc #'(lambda (x) (setf (chunk-td-salience x) 0)) (visicon-chunks vis-mod))
  ;; Q:  Do we want to restrict to some area around CLOF?  Not too hard
  ;; to parameterize here.
  ;; update spatial td activation
  (update-spatial-matches vis-mod chunk-spec)
  ;; now update those which match value constraints
  (update-feature-matches vis-mod chunk-spec)
  ;; update tot-salience for all objects
  (mapc #'(lambda (x) 
            (setf (chunk-tot-salience x)
                  (+ (chunk-bu-salience x) (chunk-td-salience x)
                     (act-r-noise *salience-noise*)))) (visicon-chunks vis-mod))
  ;; filter on the attended slot--
  ;; or should this be done before we do any salience computation?
  (let* ((attributes-available 
           (labels ((get-attributes (lst) 
                      (if lst (cons (cons (act-r-slot-spec-name (car lst)) 
                                          (act-r-slot-spec-value (car lst)))
                                    (get-attributes (cdr lst))))))
             (get-attributes (act-r-chunk-spec-slots chunk-spec))))
         (lst
           (aif (first 
                  (member 
                    ':attended attributes-available :key #'car :test #'equal))
             (remove-if-not 
               #'(lambda (x) (test-attended (list '= (car it) (cdr it)) x))
               (visicon-chunks vis-mod))
             (visicon-chunks vis-mod))))
    ;; if anything is left, add noise and find highest-salience items
    (setf lst (objs-max-val
                lst #'(lambda (x) (chunk-tot-salience x))))
    (aif (if lst (if (> (chunk-tot-salience (first lst)) *salience-thresh*)
                   (construct-location
                     vis-mod
                     (random-item (objs-max-val lst 'chunk-visual-tstamp))
                     chunk-spec)))
      (progn
        (setf (loc-failure vis-mod) nil)
        (schedule-set-buffer-chunk 'visual-location it 0 
                                   :module :vision :priority 10)
        (if (auto-attend vis-mod)
          (schedule-event-relative .05 'move-attention 
                                   :params (list :location it) 
                                   :destination :vision :output 'medium
                                   :module :vision :details 
                                   (concatenate 'string "Move-attention " 
                                                (symbol-name it))))
        it)
      (progn
        (setf (loc-failure vis-mod) t)
        (schedule-event-relative 0 'find-loc-failure :module :vision 
                                 :output 'medium)
        nil))))

(defmethod update-spatial-matches ((vis-mod vision-module) chunk-spec)
  "Increments td activation for spatial matches (spatial guidance)"
  (let ((attributes-to-check 
          (list 'screen-x 'screen-y 'distance ':nearest))
        (orig-slot-specs (act-r-chunk-spec-slots chunk-spec))
        (match-lst nil)
        (fprob nil))
    (labels ((redefine-chunk-spec (chunk-spec params-toset)
               (let ((cur-slot-specs (act-r-chunk-spec-slots chunk-spec))
                     (mod-slot-specs nil))
                 (dolist (slot-spec cur-slot-specs)
                   (if (not (set-difference 
                              (list (act-r-slot-spec-name slot-spec)) 
                              params-toset))
                     (push slot-spec mod-slot-specs)))
                 (setf (act-r-chunk-spec-slots chunk-spec) mod-slot-specs))))
      ;strips spec of everything but items listed in 'attributes-to-check'
      (redefine-chunk-spec chunk-spec attributes-to-check))
    (if (act-r-chunk-spec-slots chunk-spec) 
      ;returns objects matching this spec
      (setf match-lst (find-current-locs-with-spec vis-mod chunk-spec)))
    (when match-lst
      (setf fprob (/ (length match-lst) (length (visicon-chunks vis-mod))))
      (mapc #'(lambda (x) 
                (incf (chunk-td-salience x) (prob->bits fprob))) match-lst))
    ;resets spec to original value
    (setf (act-r-chunk-spec-slots chunk-spec) orig-slot-specs)))

(defmethod update-feature-matches ((vis-mod vision-module) chunk-spec)
  "Increments td activation for feature matches (value guidance)"
  (let ((possible-slots-to-check 
          (list 'kind 'color 'value 'size 'height 'width))
        (attributes-to-check nil)
        (slotact nil)
        (attributes-available
          (labels ((get-attributes (lst) 
                     (if lst (cons (cons (act-r-slot-spec-name (car lst)) 
                                         (act-r-slot-spec-value (car lst)))
                                   (get-attributes (cdr lst))))))
            (get-attributes (act-r-chunk-spec-slots chunk-spec)))))
    (dolist (slot possible-slots-to-check)
      (aif (first (member slot attributes-available :key #'car :test #'equal))
        ;check only available slots included in possible-slots-to-check
        (push it attributes-to-check)))
    (when (>  (length attributes-to-check) 0)
      (setf slotact (/ *salience-source-act*  (length attributes-to-check)))
      (dolist (cur-attribute attributes-to-check)
        (proc-slot-matches (visicon-chunks vis-mod) 
                           (car cur-attribute) (cdr cur-attribute) slotact)))))

(defun proc-slot-matches (lst slotname criterion activ)
  "Process salience for features which match a criterion on a particular slot."
  ;; if there's a hook function, use that
  (aif *salience-sji-hook*
    (dolist (x lst lst)
      (incf (chunk-td-salience x)
            (* activ (funcall *salience-sji-hook*
                              slotname criterion
                              (chunk-slot-value-fct x slotname)))))
    ;; there's no hook function. 
    ;; Try fast symbol matching using the stored value caches
    (progn
      (when (symbolp criterion)
        (awhen (assoc slotname *sval-caches*)
          (awhen (assoc criterion (rest it))
            (mapc #'(lambda (x)
                      (incf (chunk-td-salience x) activ)) (rest it))
            (return-from proc-slot-matches lst))))
      ;;OK so no dice there
      ;;do it the slow way by walking through the entire list and matching stuff
      (dolist (x lst lst)
        (incf (chunk-td-salience x)
              (* activ (default-l-sji
                         criterion (chunk-slot-value-fct x slotname))))))))

(defun default-l-sji (criterion value)
  "If the value matches the criterion, return the max Sji, otherwise 0."
  (if (equal criterion value) *max-salience-sji* 0))

;;;; ---------------------------------------------------------------------- ;;;;
;;;; Printing with salience values
;;;; ---------------------------------------------------------------------- ;;;;

(defun print-visicon ()
  "Print the Vision Module's visicon. For debugging."
  (awhen (get-module :vision)  ;; Test that there is a vision module
    (format t "~%Loc        Att   Kind           Value             Color           ID		    BU-L   TD-L")
    (format t "~%---------  ---   -------------  ----------------  --------------  ----------	    ----   ----")
    (dolist (x (visicon-chunks it))
      (print-icon-feature x))))

(defun print-icon-feature (x)
  (format t "~%(~3D ~3D)~11T~A~17T~A~32T~S~50T~A~66T~A~79T~5,2F~87T~5,2F"
          (chunk-slot-value-fct x 'screen-x)
          (chunk-slot-value-fct x 'screen-y) 
          (feat-attended x ;(vis-m *mp*))
                         (get-module :vision))
          (chunk-slot-value-fct x 'kind)
          (if (null (chunk-real-visual-value x))
            (chunk-slot-value-fct x 'value) 
            (chunk-real-visual-value x))
          (chunk-slot-value-fct x 'color) 
          (chunk-visicon-entry x)
          (chunk-bu-salience x)
          (chunk-td-salience x)
          ))

;;;; ---------------------------------------------------------------------- ;;;;
;;;; Math utilities
;;;; ---------------------------------------------------------------------- ;;;;


(defun prob->bits (p)
  "Convert a probability to a bit value via Hick-Hyman computation."
  (if (or (> p 1) (< p 0))
    (error "Illegal probability ~S passed to PROB->BITS." p)
    (log (/ 1 p) 2)))


(defun z->prob (z)
  "Convert a z-score to absolute value and compute the appropriate probability."
  (setf z (abs z))
  (* 2 (- 1 (normal-cumulative z))))


(defun z-trans (seq &key key)
  "Returns a sequence of z-scores based on input sequence."
  (multiple-value-bind (mean stdev)
    (seq-mean-stdev seq :key key)
    (if (zerop stdev)
      nil
      (map (type-of seq) #'(lambda (n) (/ (- 
                                            (if key (funcall key n)
                                              n)
                                            mean) stdev)) seq))))

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



(defun normal-density (x)
  (/ (exp (* -0.5 x x)) (sqrt (* 2.0 (float pi 1.0)))))


(defmacro horner-m (c e)   `(cons ,c ,e))
(defmacro horner-c (coeff) `(car ,coeff))
(defmacro horner-e (coeff) `(cdr ,coeff))


;;; take a list of coefficients (a0 a1 a2 a3 a4)
;;; and turn it into the horner form ((a0 0) (a1 1) ... )
;;;
(defun horner-coef-list-1 (as e)
  (if (null as)
    nil
    (cons (horner-m (car as) e)
          (horner-coef-list-1 (cdr as) (1+ e)))))

(defun horner-coef-list (as)
  (horner-coef-list-1 as 0))

;;; lookup a variable name in the dictionary
;;;   if it is not there, then add it
;;;
(defun horner-lookup (e dict)
  (if (assoc e (cdr dict) :test #'equal)
    (cdr (assoc e (cdr dict) :test #'equal))
    (let ((var (gensym (1- e))))
      (setf (cdr dict)
            (cons (cons e var) (cdr dict)))
      var)))

(defmacro horner-polynomial (exp . coeffs)
  (let* ((c-list (horner-knock-out (horner-coef-list coeffs)))
         (diffs  (horner-deltas 0 c-list '()))
         (dict   (list 'dict))
         (maker  (horner-var-maker exp diffs '() dict)))
    `(let* ,maker
       ;; (DECLARE (FLOAT ,@(horner-dict-vars dict)))
       ,(horner-body 0 c-list dict))))

;;; take a horner list of coefficients and
;;; knock all terms with zero coefficients
;;;
(defun horner-knock-out (horner-list)
  (cond ((null horner-list) nil)
        ((zerop (horner-c (car horner-list)))
         (horner-knock-out (cdr horner-list)))
        (t
         (cons (car horner-list)
               (horner-knock-out (cdr horner-list))))))

;;; find a list of all the differences in exponents
;;;
(defun horner-deltas (e horner-list acc)
  (cond ((null horner-list) acc)		; no coefficients, no work
        ((= e (horner-e (car horner-list)))	; no difference, so no delta
         (horner-deltas e (cdr horner-list) acc))
        (t					; we have a delta
         (let ((en (horner-e (car horner-list))))
           (horner-deltas en
                          (cdr horner-list)
                          (horner-adjoin (- en e) acc)
                          )))))

(defun horner-adjoin (x set)
  (if (member x set :test #'equal)
    set
    (sort (cons x set) #'>)))


;;;; Construct LET* Variable List and the LET* Body

;;; make a variable list for a LET* form
;;;
(defun horner-var-maker (exp d-list v-list dict)
  (if (null d-list)	      ; if all done
    v-list	              ; return the variable and dictionary
    (let ((d (car d-list)))
      (if (= d 1)
        (horner-var-maker exp
                          (cdr d-list)
                          (cons `(,(horner-lookup 1 dict) ,exp) v-list)
                          dict)
        (let* ((d0 (floor d 2))
               (d1 (- d d0)))
          (horner-var-maker exp
                            (horner-adjoin d1 
                                           (horner-adjoin d0 (cdr d-list)))
                            (cons `(,(horner-lookup d dict)
                                     (* ,(horner-lookup d1 dict)
                                        ,(horner-lookup d0 dict)))
                                  v-list)
                            dict))))
    ))

;;; return an expression that when multiplied by x^e
;;;   evaluates the polynomial specified by coeffs
;;;
(defun horner-body (e coeffs dict)
  (if (null coeffs)
    0.0					; empty expression
    (let ((coef (car coeffs)))
      (if (= (horner-e coef) e)		; time to add in a term?
        (if (null (cdr coeffs))		; e.g., 7x^12 and e=12
          `,(horner-c coef)
          `(+ ,(horner-c coef)
              ,(horner-body e (cdr coeffs) dict)))
        `(* ,(horner-lookup (- (horner-e coef) e) dict)
            ,(horner-body (horner-e coef) coeffs dict)))
      )))


(defun normal-cumulative (x)
  (flet ((nctail (x)				; eps < 7.5e-8
           (declare (ftype (function (float) float) nctail))
           (let ((z (/ 1.0 (1+ (* 0.2316419 x)))))
             (declare (float z))
             (* (NORMAL-DENSITY x)
                (horner-polynomial z
                                   0.0
                                   0.319381530
                                   -.356563782
                                   1.781477937
                                   -1.821255978
                                   1.330274429)))))
    (if (< x 0.0)
      (nctail (- x))
      (- 1.0 (nctail x)))))

