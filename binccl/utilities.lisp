(defparameter *path-separator*
  #+:digitool ":"
  #+:clozure "/")

(defmacro with-continue (&body body)
  `(handler-bind ((error #'continue))
     ,@body))

(defun sandwich (item lst)
  "places item in between all elements of lst, but not to the left or right of lst"
  (assert (listp lst))
  (cond ((null lst) nil)
        ((eq (length lst) 1) lst)
        (t (cons (car lst) 
                 (cons item (sandwich item (cdr lst)))))))

(defun path-as-lst (&rest lst)
  (format nil "~{~a~}"
          (cons (directory-namestring *load-truename*)
                (sandwich *path-separator* lst))))

(defun load-as-lst (&rest lst)
  (load (apply #'path-as-lst lst)))

(defun replace-all (string part replacement &key (test #'char-equal))
  "Returns a new string in which all the occurences of the part is replaced with replacement."
  (with-output-to-string (out)
    (loop with part-length = (length part)
          for old-pos = 0 then (+ pos part-length)
          for pos = (search part string
                            :start2 old-pos
                            :test test)
          do (write-string string out
                           :start old-pos
                           :end (or pos (length string)))
          when pos do (write-string replacement out)
          while pos))) 

#+:digitool
(defmacro without-duplicate-definition-warnings (&body body)
  `(progn ,@body))

(without-duplicate-definition-warnings
  (defun file-string (path)
    "Sucks up an entire file from PATH into a freshly-allocated string,
     returning two values: the string and the number of bytes read."
    (with-open-file (s path)
      (let* ((len (file-length s))
             (data (make-string len)))
        (values data (read-sequence data s))))))

(defun file-lines (path)
  "Sucks up an entire file from PATH into a list of freshly-allocated
   strings, returning two values: the list of strings and the number of
   lines read."
  (with-open-file (s path)
    (loop for cline = (read-line s nil nil)
          while cline
          collect cline into lines
          counting t into line-count
          finally (return (values lines line-count)))))

(defun lisp-file-p (str)
  (if (search ".lisp" str)
    t))
