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
;;;               read in GUI source code written for Macintosh Common Lisp. 
;;;               This enables task environments written in MCL (e.g., Phaser, 
;;;               Votebox, NextGen from Mike's lab) to work with CCL with 
;;;               minimal code modifications
;;;
;;;		  This file is a concatenation of all bootstrap and feature code
;;;		  so that the file can be loaded from a base/standard CCL core file.
;;;		  
;;;		  The file was built on _DATE_ using GNU Make and Bash.
;;;               Editing was done with vim+slimv, and lisp code was auto indented using
;;;               vim+slimv's auto-indention algorithm.
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
;;; 2012.08.13 cts
;;;             : Release build that works with Votebox, NextGen, and Phaser from Mike's lab
;;; 2013.02.04 cts
;;;             : Fixed bug that did not update window position after window moved with
;;;               mouse drag. Window position is correctly tracked, and a call to
;;;               view-position on the window works correctly both before and after
;;;               the window position is moved with a mouse drag on the window pane.
;;; 2013.03.11 cts
;;;            : Removed easygui::drawing-view dependency in codebase.
;;;              Simplifies OO hierarchy, removes unecessary cruft, and decreases technical debt
;;; 2013.03.16 cts
;;;            : Changes to ensure code is compatible with CCL 1.8 thru CCL 1.9rc2
