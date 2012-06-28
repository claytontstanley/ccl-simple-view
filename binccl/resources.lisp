; ----------------------------------------------------------------------
; Some functions to create and manage resource data for CCL
;
; If images or sounds are needed for your application, these functions can 
; be used to manage those resources. Usual getters/setters/creators are available:
;
; #'create-resource: Creates an image or sound resource, given a path to that file
; #'add-resource: Adds a created resource to the pool
; #'get-resource-val: Retrieves a resource's value from the pool
;
; Note that a form of lazy evaluation is used to alloc the resources only when needed
; That is, each resource is allocd the first time it's retrieved, 'not' when it's created, or
; added to the pool. If you want to alloc all resources currently in the pool (for pre-caching), 
; call #'alloc-resources

(require :cocoa)

(defun init-pool ()
  (make-hash-table :test #'equal))

(defvar *pool* (init-pool))

(defun print-pool (&optional (pool *pool*))
  (maphash (lambda (key val)
             (format t "~a->~a~%" key val))
           pool))

(defclass resource ()
  ((val :accessor val :initarg :val)
   (alloc-fn :accessor alloc-fn :initarg :alloc-fn)))

(defmacro when-bound ((name instance))
  `(if (slot-boundp  ,instance ',name)
     (,name ,instance)
     'slot-unbound))

(defmethod print-object ((obj resource) stream)
  (print-unreadable-object (obj stream :identity t :type t)
    (format stream "val->~a,alloc-fn->~a~%"
            (when-bound (val obj))
            (when-bound (alloc-fn obj)))))

(defmethod alloc-resource ((obj resource))
  (unless (slot-boundp obj 'val)
    (setf (val obj) (funcall (alloc-fn obj))))
  obj)

(defun alloc-resources (&optional (pool *pool*))
  (maphash 
    (lambda (key val)
      (declare (ignore key))
      (alloc-resource val))
    pool))

(defmethod get-val ((obj resource))
  (alloc-resource obj)
  (val obj))

(defun get-resource (id &optional (pool *pool*))
  (multiple-value-bind (resource present-p) (gethash id pool)
    (unless present-p
      (error "resource with id ~a not present in pool ~a~%" id pool))
    resource))

(defun get-resource-val (id &optional (pool *pool*))
  (get-val (get-resource id pool)))

(defun get-id (resource &optional (pool *pool*))
  (error "write this when needed"))

(defun add-resource (resource id &optional (pool *pool*))
  (setf (gethash id pool) resource))

(defun remove-resource (resource &optional (pool *pool*))
  (error "write this when needed"))

(defmethod create-resource ((type (eql 'image)) path)
  (make-instance 
    'resource
    :alloc-fn
    (lambda ()
      (#/initWithContentsOfFile: 
       (#/alloc ns:ns-image) 
       (objc:make-nsstring path)))))

(defmethod create-resource ((type (eql 'sound)) path)
  (make-instance
    'resource
    :alloc-fn
    (lambda ()
      (#/initWithContentsOfFile:byReference:
       (#/alloc ns:ns-sound)
       (objc:make-nsstring path)
       nil))))

; I am requiring all objective-c/lisp functions to not ever use ns-arrays as inputs or outputs
; This slows down computation time b/c conversions have to be done within each function, but it
; makes each one much easier to use in a lisp environment (keep lisp types for inputs and outputs).
;
; I am not doing this for ns-mutable string; I'm putting up with doing the conversion on that one when 
; needed. It also would be problematic to convert the type of anything that can be placed within an ns-array.
; Done this way, where the containers (arrays/lists) are converted, but not the containees, the container
; conversion functions do not have to do any type conversion or type checking.

(defmacro! do-array ((varsym array &optional ret) &body body)
  `(loop for ,g!i below (#/count ,array)
         for ,varsym = (#/objectAtIndex: ,array ,g!i)
         do (progn ,@body)
         ,@(if ret `(finally (return ,ret)))))

(defun ns-array->list (ns-array)
  (let ((out))
    (do-array (item ns-array out)
      (push-to-end item out))))

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

#|
(defun open-resource-file (dir &key if-does-not-exist errorp direction perm data-fork-p)
  (open-resource-folder dir))|#

(provide :resources)

; Section for test code:
#| 
(let ((dir (choose-directory-dialog)))
  (setf *pool* (init-pool))
  (open-resource-file (directory-namestring dir))
  (print-pool)
  (get-resource-val "voteboxbg"))
|#

