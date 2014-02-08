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
;;; Bugs        : []
;;; 
;;; Todo        : Lots
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
;;; 2013.02.11 cts
;;;             : Removed easygui::drawing-view dependency in codebase.
;;;               Simplifies OO hierarchy, removes unnecessary cruft, and decreases technical debt
;;; 2013.02.16 cts
;;;             : Changes to ensure code is compatible with CCL 1.8 through CCL 1.9rc2
;;; 2013.04.10 cts
;;;             : Added feature: You can now change the color of the text shown for button objects
;;;               (button-dialog-item, check-box-dialog-item, radio-button-dialog-item) in the usual
;;;               MCL way for dialog items (passing :fore-color #color# or :part-color (list :text #color#)
;;;               to initialize-instance)
;;; 2013.04.20 cts
;;;             : #@ read macro no longer clobbers CCL's original version that created NSString objects.
;;;               The #@ read macro now creates MCL points when provided with a list, and an NSString object
;;;               when provided with a string.
;;;             : Reordered loading a few subcomponent files and renamed a few subcomponent files.
;;;             : Removed stray commented out code that is no longer necessary.
;;;             : Now spell checking comments and strings in the code.
;;; 2013.05.13 cts
;;;             : Added implementation for sequence-dialog-item.
;;;             : Added quickdraw functions for paint-polygon and fill-polygon.
;;;             : Removed extra cruft from file: no longer including thermometer.lisp since
;;;               this library code is not part of the core MCL GUI interface.
;;;             : Also removed unused code (all of the pandoric functions) from lol-subset.lisp
;;;             : Added MCL's :centered keyword for windows.
;;;             : Fixed quickdraw polygon functions to use the polygon provided as input to the function, 
;;;               instead of using the polygon stored within the view. This change matches the MCL spec.
;;; 2013.06.02 cts
;;;             : Added MCL's :top and :bottom list arguments to :view-position for windows
;;;             : Enabled :dialog-item-action initarg from MCL spec to work, and map correctly to 
;;;               :action initarg for CCL spec.
;;;             : Cleaned up :parse-mcl-initarg methods. Created a proper generic method signature for the
;;;               methods, so that they have the needed flexibility to work in all cases.
;;;             : Plugged memory leaks by creating an autorelease pool at the beginning of this file.
;;;               This ensures that any autoreleased objects during both compilation and runtime have 
;;;               a pool to release to. This was only an issue when running CCL via SVN (e.g., with common SLIME setup),
;;;               and not via the Clozure CL.app, because the App already has an autorelease pool set up on startup.
;;; 2013.07.31 cts
;;;             : Incorporated additional functionality needed for a recent project into the ccl-simple-view library:
;;;               -Sequence dialog item now defaults to nil sequence if none specified on init
;;;               -menu-view and menu-item objects now set font correctly
;;;             : Refactored and simplified image-view code 
;;;             : Everything works on 10.6.8 - 10.8.4 (newest Mountain Lion install as of testing date)
;;;               -required a fix for radio buttons. Clustering did not work on 10.8 prior to fix
;;;             : Code-coverage report is now generated for each build. 
;;;               -Used report to remove stray code and discover/fix a few minor bugs
;;; 2013.09.17 cts
;;;             : Swapped cocoa class for editable-text-dialog-item from NSTextField to NSTextView
;;;               -Ensures that keypresses on the view that are relayed to view-key-event-handler are accurate
;;;               -Allows for more control over keypresses in the view, so that for example, the next responder
;;;                can be called when a #\tab is pressed in the view.
;;;             : Reworked #\tab presses for UI, so that tabbing moves between all of the views on the window
;;;               that can become first responder.
;;;             : Reworked #\space presses for buttons, so that if a #\space is pressed when a button is
;;;               first responder, then the action for that button fires (i.e., the button is clicked).
;;;             : Ensured that the window is always the starting first responder when the view is created.
;;;               Afterwards, if the user presses #\tab, then the next responder is activated, and tabbing
;;;               then cycles through all available first responders. This is to ensure that no particular view
;;;               is the first responder in the simplest case: for a basic window where the user does not
;;;               wish to tab between views.
;;;             : If tabbing across buttons is enabled at the OS level:
;;;               http://superuser.com/questions/473143/how-to-tab-between-buttons-on-an-mac-os-x-dialog-box
;;;               then a user can work through most tasks involving editing text views and pressing buttons
;;;               using only the keyboard. #\tab to navigate and #\space to select
;;; 2014.02.08 cts
;;;             : Implemented :close-box-p initarg for windows, so that the red close button does not display
;;;               when this is set to t when initializing the window
;;;             : Fixed a bug where incorrect mouse locations were passed to view-click-event-handler for views.
;;;               All locations where correct for view-click-event-handlers defined on windows, and for 
;;;               rpm-window-click-event-handlers (since those are defined on the window).
;;;               This only applied to view-click-event-handlers defined on views (e.g., buttons).
;;;               MCL spec sets location for view-click-event-handler to the mouse click location relative to that 
;;;               view's local coordinate system, not relative to the window's coordinate system. So if a button
;;;               responds to a mouse click that is 10 by 10 pixels within that button's view rectangle, then
;;;               the location for view-click-event-handler will by 10 by 10, and not 10 by 10 plus that view's
;;;               position within the window for example.
;;;               The prior code passed 10 by 10 plus the view's position to view-click-event-handler.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
