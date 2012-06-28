(defmacro with-continue (&body body)
  `(handler-bind ((error #'continue))
     ,@body))

(defparameter *path-separator*
  #+:digitool ":"
  #+:clozure "/")

(defun load-as-lst (&rest lst)
  (let ((path
          (format nil "~{~a~}"
                  (cons (directory-namestring *load-truename*)
                        (sandwich *path-separator* lst)))))
    (print path)
    (load path)))

(defun sandwich (item lst)
  "places item in between all elements of lst, but not to the left or right of lst"
  (assert (listp lst))
  (cond ((null lst) nil)
        ((eq (length lst) 1) lst)
        (t (cons (car lst) 
                 (cons item (sandwich item (cdr lst)))))))

(defun load-in-bincarbon (&rest files)
  (dolist (file files)
    (load-as-lst "bincarbon" file)))

(defun file-string (path)
  "Sucks up an entire file from PATH into a freshly-allocated string,
   returning two values: the string and the number of bytes read."
  (with-open-file (s path)
    (let* ((len (file-length s))
           (data (make-string len)))
      (values data (read-sequence data s)))))

(defun file-lines (path)
  "Sucks up an entire file from PATH into a list of freshly-allocated
   strings, returning two values: the list of strings and the number of
   lines read."
  (with-open-file (s path)
    (loop for line = (read-line s nil nil)
          while line
          collect line into lines
          counting t into line-count
          finally (return (values lines line-count)))))

#-:act-r-6.0 (load-as-lst "actr6" "load-act-r-6.lisp")

#+clozure (require :cocoa)
#+clozure (require :easygui)
#+clozure (dolist (file (file-lines "~/src/mcl-migration/build/file-list.txt"))
            (load (format nil "~a/~a" "~/src/mcl-migration" file)))

#+digitool (load-as-lst "bootstrap-mcl.lisp")
