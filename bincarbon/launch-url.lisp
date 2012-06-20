;;;  -*- mode: LISP; Package: CL-USER; Syntax: COMMON-LISP;  Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Author      : Mike Byrne [original MCL code Gary King]
;;; Availability: public domain
;;; Address     : Rice University
;;;             : Psychology Department
;;;             : Houston,TX 77251-1892
;;;             : byrne@acm.org
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Filename    : launch-url.lisp
;;; Version     : r1
;;; 
;;; Description : Defines a LAUNCH-URL function which works under OS X.
;;; 
;;; Bugs        : 
;;; 
;;; Todo        : 
;;; 
;;; ----- History -----
;;; 2008.01.18 mdb
;;;             : Added support for LispWorks, which was way easy.
;;; 03.10.14 Mike Byrne
;;;             :  Incept date.
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


#+mcl
(progn
  (defmacro with-cfstring ((cfstring str) &body body) 
    `(with-cstrs ((cstr ,str)) 
                 (let ((,cfstring (#_CFStringCreateWithCString
                                   (%null-ptr)
                                   cstr 
                                   #$kCFStringEncodingUTF8))) 
                   (when ,cfstring 
                     (unwind-protect 
                         (progn ,@body) 
                       (#_CFRelease ,cfstring))))))

  (defmacro with-cfurl ((cfurl str) &body body) 
    `(with-cfstring (cfstr ,str) 
                    (let ((,cfurl
                           (#_CFURLCreateWithString (%null-ptr)
                                                    cfstr (%null-ptr)))) 
                      (when ,cfurl 
                        (unwind-protect 
                            (progn ,@body) 
                          (#_CFRelease ,cfurl))))))


  (defun launch-url (strng)
    "Opens the provided URL with the OS's specified helper application."
    (with-cfurl (tmp strng)
                (#_LSOpenCFURLRef tmp (%null-ptr))))
  )

#+lispworks
(defun launch-url (string)
  "Opens the provided URL with the OS's specified helper application."
  (sys:open-url string)) 



#|
(defun open-url (url-string)
  (with-cfstrs ((url url-string))
    (let ((cfurl (#_cfurlcreatewithstring (%null-ptr) url (%null-ptr))))
      (unless (%null-ptr-p cfurl)
        (unwind-protect   ; overkill
          (zerop (#_LSOpenCFURLRef cfurl (%null-ptr)))
          (#_cfrelease cfurl))))))
|#

(provide :launch-url)
