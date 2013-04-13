#+:clozure (setf ccl:*default-external-format* :utf-8)
#+:clozure (pushnew :sv-dev *features*)
#+:clozure (defvar *load-sv-dev-files-p* t)

(defmacro with-continue (&body body)
  `(handler-bind ((error #'continue))
     ,@body))

(defparameter *path-separator*
  #+:digitool ":"
  #+:clozure "/")

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

(defun load-file-list (&rest file-list)
  "Loads each file in file-list, where the location of 
   file-list is a relative path from the load file's directory
   and the location of each file in file-list is a relative path from
   the base repo's directory"
  (let ((base-repo-namestring
          (format nil "~a~a~a" (directory-namestring *load-truename*) ".." *path-separator*)))
    (dolist (file (file-lines (apply #'path-as-lst file-list)))
      (let ((file (replace-all file "/" *path-separator*)))
        (let ((file (format nil "~a~a" base-repo-namestring file)))
          (cond ((search "load-act-r-6.lisp" file)
                 #-:act-r-6.0 (load file))
                (t
                 (load file))))))))

; Bootstrap script/logic starts here

#-:act-r-6.0 (load-as-lst ".." "submodules" "actr6" "load-act-r-6.lisp")

#+:clozure
(cond ((and (member "swank-repl" *modules* :test #'string-equal)
            *load-sv-dev-files-p*)
       (load-file-list ".." "build" "file-list.txt")
       (load-file-list ".." "build" "file-list-device.txt")
       (load-file-list ".." "build" "file-list-uwi.txt"))
      (t nil))

#+:clozure (setf *resource-pool* (init-pool))

(load-as-lst ".." "submodules" "lisp-dev" "Lisp-Unit-Testing-Framework" "unitTestFramework.lisp")

#+:digitool (load-as-lst ".." "bincarbon" "bootstrap-mcl.lisp")

