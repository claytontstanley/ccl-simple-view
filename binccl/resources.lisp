(defun init-pool ()
  (make-hash-table :test #'equal))

(defvar *pool* (init-pool))

(defun print-pool (&optional (pool *pool*))
  (maphash (lambda (key val)
             (format t "~a->~a~%" key val))
           pool))

(defun get-resource (id &optional (pool *pool*))
  (multiple-value-bind (val present-p) (gethash id pool)
    (unless present-p
      (error "resource with id ~a not present in pool ~a~%" id pool))
    val))

(defun get-id (resource &optional (pool *pool*))
  (error "write this when needed"))

(defun add-resource (resource id &optional (pool *pool*))
  (setf (gethash id pool) resource))

(defun remove-resource (resource &optional (pool *pool*))
  (error "write this when needed"))

(defmethod create-resource ((type (eql 'image)) path)
  (#/initWithContentsOfFile: 
   (#/alloc ns:ns-image) 
   (objc:make-nsstring path)))

; TODO: Write the create-resource for 'sound

; I am requiring all objective-c/lisp functions to not ever use ns-arrays as inputs or outputs
; This slows down computation time b/c conversions have to be done within each function, but it
; makes each one much easier to use in a lisp environment (keep lisp types for inputs and outputs).
;
; I am not doing this for ns-mutable string; I'm putting up with doing the conversion on that one when 
; needed. It also would be problematic to convert the type of anything that can be placed within an ns-array.
; Done this way, where the containers (arrays/lists) are converted, but not the containees, the container
; conversion functions do not have to do any type conversion or type checking.

; TODO: Finish this off and write the &optional return in the first arglist, to mimick dolist
(defmacro! doarray ((varsym array) &body body)
  `(loop for ,g!i below (#/count ,array)
         for ,varsym = (#/objectAtIndex: ,array ,g!i)
         do (progn ,@body)))

(defun ns-array->list (ns-array)
  (let ((out))
    (doarray (item ns-array)
      (push-to-end item out))
    out))

(defun list->ns-array (lst)
  (let ((out (#/array ns:ns-mutable-array)))
    (dolist (item lst out)
      (#/addObject: out item))))

(defun contents-of-directory (dir)
  (ns-array->list
    (#/contentsOfDirectoryAtPath:error:
     (#/defaultManager ns:ns-file-manager)
     (objc:make-nsstring dir)
     ccl:+null-ptr+)))

(defun remove-if-not-predicate (lst predicate)
  (ns-array->list
    (#/filteredArrayUsingPredicate:
     (list->ns-array lst)
     (#/predicateWithFormat:
      ns:ns-predicate
      (objc:make-nsstring predicate)))))

(defun remove-if-not-image (lst)
  (remove-if-not-predicate lst "self ENDSWITH '.tiff'"))

(defun remove-if-not-sound (lst)
  (remove-if-not-predicate lst "self ENDSWITH '.m4a'"))

(defun open-resource-folder (dir)
  (let ((dir (if (pathnamep dir) 
               (directory-namestring dir)
               dir)))
    (dolist (image-name (remove-if-not-image (contents-of-directory dir)))
      (let* ((image-name-lisp-str (objc:lisp-string-from-nsstring image-name))
             (image-name-no-ext (#/stringByDeletingPathExtension image-name))
             (res (create-resource 'image (format nil "~a~a" dir image-name-lisp-str))))
        (add-resource res (objc:lisp-string-from-nsstring image-name-no-ext))))))

; This is a wrapper function to maintain backwards compatibility with lab code that was loading a resource file.
; The idea is that we take all images and sounds in the resource file, and place them into a single folder. Then
; on the old code, load the folder instead of the resource file, but the old code doesn't have to be changed, since
; it just calls the function below, which calls the proper open-resource-folder function

(defun open-resource-file (dir &key if-does-not-exist errorp direction perm data-fork-p)
  (open-resource-folder dir))

(provide :resources)

; Section for test code:
#| 
(let ((dir (choose-directory-dialog)))
  (setf *pool* (init-pool))
  (open-resource-file (directory-namestring dir))
  (print-pool)
  (get-resource "image2"))
|#

