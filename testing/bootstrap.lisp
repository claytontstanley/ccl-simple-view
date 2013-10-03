#+:clozure (setf ccl:*default-external-format* :utf-8)
;#+:clozure (pushnew :sv-dev *features*)
#+:clozure (defvar *load-sv-dev-files-p* t)
#+:clozure (require :cocoa)

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
  (load-file (apply #'path-as-lst lst)))

(defun replace-all (string part replacement &key (test #'char-equal))
  "Returns a new string in which all the occurrences of the part is replaced with replacement."
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
   the base repository's directory"
  (let ((base-repo-namestring
          (format nil "~a~a~a" (directory-namestring *load-truename*) ".." *path-separator*)))
    (dolist (file (file-lines (apply #'path-as-lst file-list)))
      (let ((file (replace-all file "/" *path-separator*)))
        (let ((file (format nil "~a~a" base-repo-namestring file)))
          (cond ((search "load-act-r-6.lisp" file)
                 #-:act-r-6.0 (load-file file))
                (t
                 (load-file file))))))))

(defmethod load-file (file)
  (load file))

; Bootstrap script/logic starts here

(defun sv-dev-env-p ()
  (and (member "swank-repl" *modules* :test #'string-equal)
       *load-sv-dev-files-p*))

#+:clozure (when (sv-dev-env-p)
             (load-file-list ".." "build" "file-list.txt"))

(defparameter *actr6-dir-name* #+:clozure "actr6" #+:digitool "actr6mcl")

#-:act-r-6.0 (load-as-lst ".." "submodules" *actr6-dir-name* "load-act-r-6.lisp")

#+:clozure (when (sv-dev-env-p)
             (load-file-list ".." "build" "file-list-device.txt")
             (load-file-list ".." "build" "file-list-uwi.txt"))

#+:clozure (setf *resource-pool* (init-pool))

#+:digitool (load-file-list "file-lists" "allMCL.txt") 

(load-file-list "file-lists" "all.txt")

(setf *break-on-fail-p* t)

(let ((coverage-file (format nil "~a~a" (directory-namestring *load-truename*) "../.coverage.temp")))
  (when ccl:*compile-code-coverage*
    (when (probe-file coverage-file)
      (restore-coverage-from-file coverage-file))))
