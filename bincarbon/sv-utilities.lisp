(require :sv-language-layer)
(require :lol-subset)

(ensure-defined
  (defmacro push-to-end (item place)
    "analogous to the push macro; just places 'item' at the end of 'place', instead of the front"
    `(setf ,place (nconc ,place (list ,item)))))

(defun spin-for-fct (ms-delay)
  (without-interrupts
    (let ((start (internal-real-time->ms
                   (get-internal-real-time))))
      (loop until (< ms-delay (- (internal-real-time->ms
                                   (get-internal-real-time))
                                 start))))))

(defun internal-real-time->ms (&optional (internal-real-time (get-internal-real-time)))
  (* 1000
     (/ internal-real-time
        internal-time-units-per-second)))

(defun file-string (path)
  "Sucks up an entire file from PATH into a freshly-allocated string returning two values: the string and the number of bytes read."
  (if path
    (with-open-file (s path)
      (let* ((len (file-length s))
             (data (make-string len)))
        (values data (read-sequence data s))))))

(ensure-defined
  (defmacro awhen (test-form &body body)
    `(aif ,test-form
       (progn ,@body))))

(defmacro! acond (&rest clauses)
  "works just like cond, but stores the value of each condition as 'it', which is accessible in the code following the condition"
  (if clauses
    (let ((cl1 (car clauses)))
      `(let ((,g!sym ,(car cl1)))
         (if ,g!sym
           (let ((it ,g!sym)) 
             (declare (ignorable it)) 
             ,@(cdr cl1))
           (acond ,@(cdr clauses)))))))

(ensure-defined
  (defmacro while (test &body body)
    "loops through body, evaluating test each time until test returns false"
    `(do ()
       ((not ,test)) 
       ,@body)))

(defmacro aand (&rest args)
  (cond ((null args) t)
        ((null (cdr args)) (car args))
        (t `(aif ,(car args) (aand ,@(cdr args))))))

(defmacro! guard ((guard &rest errstr) &body body)
  (let ((errstr (if errstr 
                  errstr
                  (list "guard ~a failed" `',guard))))
    `(let* ((it (multiple-value-list (progn ,@body)))
            (it1 (first it)))
       (declare (ignorable it it1))
       (assert ,guard nil ,@errstr)
       (apply #'values it))))

(defmacro! with-shadow ((fname fun) &body body)
  "Shadow the function named fname with fun; any call to fname within body will use fun, instead of the default function for fname.
   This macro is intentionally unhygienic: fun-orig is the anaphor, and can be used in body to access the shadowed function"
  `(let ((fun-orig))
     (cond ((fboundp ',fname) ;if there is already a function with that name defined, then shadow it
            (setf fun-orig (symbol-function ',fname))
            (setf (symbol-function ',fname) ,fun)
            (unwind-protect (progn ,@body)
              (setf (symbol-function ',fname) fun-orig)))
           (t ;otherwise, define a new function with that name, and then undo the operation afterwards by unbinding that function
            (setf (symbol-function ',fname) ,fun)
            (unwind-protect (progn ,@body)
              (fmakunbound ',fname))))))

(provide :sv-utilities)
