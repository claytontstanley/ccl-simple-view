;;;  -*- mode: LISP; Package: CL-USER; Syntax: COMMON-LISP;  Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Author      : Clayton Stanley 
;;; Copyright   : (c)2003-7 CMU/Rice U./Mike Byrne, All Rights Reserved
;;; Availability: public domain
;;; Address     : Rice University
;;;             : Psychology Department
;;;             : Houston,TX 77251-1892
;;;             : byrne@acm.org
;;;             : clayton.stanley@rice.edu 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Filename    : _FILE_
;;; Version     : 1.0 
;;; 
;;; Description : Provides an interface for CCL that allows the implementation to
;;;		  read in MCL GUI src code. Implemented the minimal MCL GUI interface
;;;		  in CCL that is necessary to run MCL code in Mike's lab
;;;
;;;		  This file is a concatenation of all bootstrap and feature code
;;;		  so that the file can be loaded from a base/standard CCL core file.
;;;		  
;;;		  The file was built on _DATE_ using GNU Make and Bash.
;;;               Editing was done with vim+slimv, and slimv was used to auto indent the lisp file.
;;;
;;;		  Git commit hash associated with build: _GITID_
;;;
;;;		
;;;
;;; Bugs        : many
;;; 
;;; Todo        : [] 
;;;             : 
;;; 
;;; ----- History -----
;;; 2012.06.29 cts 
;;;             : Initial build that allows Votebox MCL GUI to be read in  and used by CCL



