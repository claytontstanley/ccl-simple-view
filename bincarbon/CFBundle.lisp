;;;-*- Mode: Lisp; Package: COMMON-LISP-USER -*-

(in-package "CL-USER")

;;; See the sample code at:
;;; http://developer.apple.com/samplecode/Sample_Code/Runtime_Architecture/CallMachOFramework.htm
;;; This is The Official Way to open and use Mach-O libraries from CFM
;;; Carbon applications.  -gb 2/21/02
;;; (Revised slightly, since MCL's interface files now define
;;; more constants and entrypoints than they once did) -gb 11/12/02


;;; Define some shared-library entrypoints for symbols that don't seem
;;; to be in the interface files.  There may be a better way to do this;
;;; if I ever knew it, I've forgotten.


(defvar *__cfstringmakeconstantstring-slep*
  (ccl::get-slep "__CFStringMakeConstantString"))

(defun cfstr (string)
  (with-cstrs ((cstr string))
    (ccl::ff-call-slep *__CFStringMakeConstantString-slep*
                       :address cstr 
                       :address)))

(defun create-frameworks-url ()
  (rlet ((fsref :fsref))
    (let* ((err (#_FSFindFolder #$kOnAppropriateDisk #$kFrameworksFolderType #$true fsref)))
      (declare (type (signed-byte 16) err))
      (if (eql #$noErr err)
        (let* ((url (#_CFURLCreateFromFSRef (%null-ptr) fsref)))
          (if (%null-ptr-p url)
            (error "Failed to create URL")
            url))
        (error "Couldn't find system Frameworks folder")))))

(ccl::defloadvar *frameworks-url* nil)

(defun frameworks-url ()
  (or *frameworks-url*
      (setq *frameworks-url* (create-frameworks-url))))

(defun load-framework-bundle (framework-name)
  (let* ((bundle-url 
          (#_CFURLCreateCopyAppendingPathComponent
           (%null-ptr)
           (frameworks-url)    ; file:///System/Library/Frameworks/
           (CFSTR framework-name)
           #$false)))
    (if (%null-ptr-p bundle-url)
      (error "Can't create URL for ~s in system frameworks folder" 
             framework-name)
      (let* ((bundle (#_CFBundleCreate (%null-ptr) bundle-url)))
        (if (%null-ptr-p bundle)
          (error "Can't create bundle for ~s" framework-name)
          (if (eql #$false (#_CFBundleLoadExecutable bundle))

            (error "Couldn't load bundle library for ~s" framework-name)
            bundle))))))
            

(ccl::defloadvar *system-framework-bundle* nil)

;;; Most BSD/Mach functions are in the System framework.
(defun system-framework-bundle ()
  (or *system-framework-bundle*
      (setq *system-framework-bundle*
            (load-framework-bundle "System.framework"))))


(defun lookup-function-in-framework (symbol-name &optional
                                                 (bundle (system-framework-bundle)))
  (let* ((addr (#_CFBundleGetFunctionPointerForName bundle (CFSTR symbol-name))))
    (if (%null-ptr-p addr)
      (error "Couldn't resolve address of foreign function ~s" symbol-name)
      ;; This may be a little confusing: MCL uses fixnums (whose low 2 bits are
      ;; zero) to represent function addresses (whose low 2 bits are zero ...)
      ;; Shove the pointer in a buffer; fetch a signed 32-bit integer, shift it
      ;; right 2 bits ... voila.
      (rlet ((buf :long))
        (setf (%get-ptr buf) addr)
        (ash (%get-signed-long buf) -2)))))


#|
;;; Lookup the foreign function "gethostname" and call it.
;;; Note that (a) we have to use CCL::PPC-FF-CALL to call the address
;;; (b) that address may become invalid when a saved application is
;;; resumed.
;;; One would want to cache these addresses (and invalidate the cache on
;;; application startup); that's left as an exercise.
;;; The Darwin implementation of gethostname doesn't seem to want to tell
;;; you how big the buffer needs to be.

(defun gethostname ()
  (let* ((gethostname-function-ptr (lookup-function-in-framework
                                    "gethostname" (system-framework-bundle))))
    (%stack-block ((buf 512))
      (if (eql 0 (ccl::ppc-ff-call
                  gethostname-function-ptr
                  :address buf
                  :unsigned-fullword 512
                  :signed-fullword))
        (%get-cstring buf)))))
|#

(provide :cfbundle)