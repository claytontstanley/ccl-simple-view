;; Antiweb (C) Doug Hoyte

;; This is a "production" version of LOL with bug-fixes
;; and new features in the spirit of the book.

;; See http://letoverlambda.com

;; This is the source code for the book
;; _Let_Over_Lambda_ by Doug Hoyte.
;; This code is (C) 2002-2008, Doug Hoyte.
;;
;; You are free to use, modify, and re-distribute
;; this code however you want, except that any
;; modifications must be clearly indicated before
;; re-distribution. There is no warranty,
;; expressed nor implied.
;;
;; Attribution of this code to me, Doug Hoyte, is
;; appreciated but not necessary. If you find the
;; code useful, or would like documentation,
;; please consider buying the book!
;;
;; 2012-06-29: Clayton Stanley. I took only a subset
;; of Doug Hoyte's LOL production code, which is included
;; below. I did not modify any code within this subset,
;; except to change a few &rest to &body keywords in macro definitions,
;; so that slimv could handle auto-indenting properly. The behavior
;; of the code 'was not changed'.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sv-language-layer))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ensure-defined
    (defun mkstr (&rest args)
      (with-output-to-string (s)
        (dolist (a args) (princ a s))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun symb (&rest args)
    (values (intern (apply #'mkstr args)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun group (source n)
    (if (zerop n) (error "zero length"))
    (labels ((rec (source acc)
               (let ((rest (nthcdr n source)))
                 (if (consp rest)
                   (rec rest (cons
                               (subseq source 0 n)
                               acc))
                   (nreverse
                     (cons source acc))))))
      (if source (rec source nil) nil))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ensure-defined
    (defun flatten (lis)
      "Takes a nested list and makes in into a single-level list"
      (declare (list lis))
      (labels ((rec (lis acc)
                 (cond ((null lis) acc)
                       ((atom lis) (cons lis acc))
                       (t (rec (car lis) (rec (cdr lis) acc))))))
        (rec lis nil)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun g!-symbol-p (s)
    (and (symbolp s)
         (> (length (symbol-name s)) 2)
         (string= (symbol-name s)
                  "G!"
                  :start1 0
                  :end1 2))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro defmacro/g! (name args &body body)
    (let ((syms (remove-duplicates
                  (remove-if-not #'g!-symbol-p
                                 (flatten body)))))
      `(defmacro ,name ,args
         (let ,(mapcar
                (lambda (s)
                  `(,s (gensym ,(subseq
                                  (symbol-name s)
                                  2))))
                syms)
           ,@body)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun o!-symbol-p (s)
    (and (symbolp s)
         (> (length (symbol-name s)) 2)
         (string= (symbol-name s)
                  "O!"
                  :start1 0
                  :end1 2))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun o!-symbol-to-g!-symbol (s)
    (symb "G!"
          (subseq (symbol-name s) 2))))

(defmacro defmacro! (name args &body body)
  (let* ((os (remove-if-not #'o!-symbol-p args))
         (gs (mapcar #'o!-symbol-to-g!-symbol os)))
    `(defmacro/g! ,name ,args
       `(let ,(mapcar #'list (list ,@gs) (list ,@os))
          ,(progn ,@body)))))

;; Graham's alambda
(defmacro alambda (parms &body body)
  `(labels ((self ,parms ,@body))
     #'self))

;; Graham's aif
(ensure-defined
  (defmacro aif (test then &optional else)
    `(let ((it ,test))
       (if it ,then ,else))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun |#`-reader| (stream sub-char numarg)
    (declare (ignore sub-char))
    (unless numarg (setq numarg 1))
    `(lambda ,(loop for i from 1 to numarg
                    collect (symb 'a i))
       ,(funcall
          (get-macro-character #\`) stream nil))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (set-dispatch-macro-character
    #\# #\` #'|#`-reader|))


(provide :lol-subset)
