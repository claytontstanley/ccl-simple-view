
Directory Structure:

.
├── Makefile
├── README
├── actr6
│   └── devices
├── bincarbon
│   ├── CFBundle.lisp
│   ├── base-trek-tasks.lisp
│   ├── blinker.lisp
│   ├── device-extensions.lisp
│   ├── device-patches-mcl.lisp
│   ├── experiment-window4.lisp
│   ├── launch-url.lisp
│   ├── menubar-hide.lisp
│   ├── misc-lib.lisp
│   ├── pict-svm.lisp
│   ├── procedure-window2.lisp
│   ├── seq-math.lisp
│   ├── snd-player.lisp
│   └── timer.lisp
├── binccl
│   ├── logger.lisp
│   ├── mcl-ccl-colors.lisp
│   ├── resources.lisp
│   └── utilities.lisp
├── build
│   ├── Makefile
│   ├── file-list-device.txt
│   ├── file-list-uwi.txt
│   ├── file-list.txt
│   ├── outFileHeader.lisp
│   ├── post-code.lisp
│   └── post-file-list.txt
├── docs
│   ├── Getting\ Started\ with\ MCL.pdf
│   ├── InterfaceBuilderWithCCLTutorial.pdf
│   └── Macintosh\ Common\ Lisp\ Ref.pdf
├── easygui
│   ├── extensions.lisp
│   └── patches.lisp
├── rmcl
│   ├── examples
│   ├── level-1
│   └── lib
├── scratch
│   ├── colors.lisp
│   ├── dialog-item.lisp
│   ├── ffi.lisp
│   ├── image.lisp
│   ├── it-example1.lisp
│   ├── keyboard.lisp
│   ├── mouse.lisp
│   ├── nstimer.lisp
│   ├── scratch.lisp
│   └── threads.lisp
├── submodules
│   ├── ACT-R-VoteBox-Models
│   ├── X83-Frank-Trek-11
│   ├── actr6
│   ├── ccl
│   ├── kristenVotebox
│   ├── lisp-dev
│   ├── nextGen
│   ├── rmcl
│   └── tambo-diss
├── testing
│   ├── Makefile
│   ├── bootstrap-mcl.lisp
│   ├── bootstrap.lisp
│   ├── ccl -> ../submodules/ccl/dx86cl64
│   ├── data
│   ├── file-lists
│   ├── testCCLDevice.lisp
│   ├── testDrawing.lisp
│   ├── testFormat.lisp
│   ├── testGetText.lisp
│   ├── testIcons.lisp
│   ├── testImages.lisp
│   ├── testLevelIndicator.lisp
│   ├── testModalDialog.lisp
│   ├── testNextGen.lisp
│   ├── testPhaser.lisp
│   ├── testPolygon.lisp
│   ├── testSound.lisp
│   ├── testTamboDis.lisp
│   ├── testTamboDisModel.lisp
│   ├── testTutorials.lisp
│   ├── testVotebox.lisp
│   ├── testVoteboxKristen.lisp
│   ├── testWindow.lisp
│   └── utilities.lisp -> ../binccl/utilities.lisp
└── tools
    └── merge-and-verify-driver


actr6:		All .lisp files needed to build an ACT-R device for CCL that uses a Cocoa display. 
		-Note the share.lisp file lives here for now, which contains the MCL GUI interface for CCL

bincarbon:	All original MCL bincarbon code. Just a few files have minor tweaks to work with CCL. Timer is broken on CCL currently though.
		Other files, like CFBundle.lisp don't work at all on CCL, but aren't necessary on CCL. The main goal for this folder is to document
		all changes to bincarbon files when migrating to CCL, while maintaining backwards compatibility with MCL.

binccl:		CCL-specific utility files. Contains a CCL implementation of an interface for managing resource files (e.g., sounds, images)

build:		Code to generate a single source file from current code in the repo.

docs:		Archived documentation and reference manuals found online.
		-No docs for this specific stuff yet

easygui:	Any .lisp extensions to CCL's easygui package. 
		-Contains a few bug fixes, and an extension which provides a Cocoa view that does not respond to mouse activity	
rmcl:		Any lisp code from RMCL that was either directly copied to this distro (some of the digitool GUI code could be bootstrapped
		after the CCL interface was defined) or rewritten for this distro (e.g., thermometer.lisp, the modal dialog implementation)

scratch:	Stray lisp code, experimental, etc.
		-None of this is loaded for any of the tests

submodules:	Source code for actr and ccl
		-actr is newest version (as of Jun 2012). All code is same except that loader files (loader.lisp) are inside each of the tutorial folders
		-ccl is newest version (as of Jun 2012). Original image. Used to reference ccl src and run basic ccl core file
		-lisp-dev is a git repo for building a custom ccl core that I use during development. I'm using some of the src for the utilities in the GUI code,
			so the builds here grab some of that src.
		-rmcl is the newest version; no changes; used to reference rmcl src, and run rmcl for each of the tests.

testing:	Source code for testing the ccl code
		-testCCLDevice.lisp: Used to test basic functionality of building a Cocoa display by writing lisp code that meets the uwi.lisp interface spec
		-testTutorials.lisp: Runs ACT-R through all tutorials; checks that ACT-R can 'see' CCL's Cocoa device
		-testImages.lisp: Tests that the CCL resources.lisp file correctly manages images
		-testVotebox.lisp: Tests to run Votebox on CCL
		-testPhaser.lisp: Tests to run Phaser on CCL

tools:		Various shell scripts and tools used to manage the repo
		-merge-and-verify-driver. Used by git when I want to do a merge by hand.
	
Main files:	

actr6/devices/ccl/share.lisp: Implements basic MCL GUI interface in CCL. Leverages CCL's easygui package so that a Cocoa window can be used in CCL.
actr6/devices/ccl/uwi.lisp: CCL's implementation of the RPM interface. Note that this file is identical to MCL's file
actr6/devices/ccl/device.lisp: CCL's implementation of the RPM interface. Only slight changes to this file compared to MCL's file. I'm trying to keep these to a minimum.


To run a test:
		-Mount RedGiant volume	
		-Navigate to the Projects/mcl-migration folder on RedGiant
		-Either access the data in the folder over the network (preferred), or copy the folder locally if you want to access it w/o internet.
		-Download Clozure CL from the App Store and install if you don't have it yet.
		-Launch Clozure CL
		-Use the Cocoa listener to load one of the test files.

		e.g.:
		Launch Clozure CL
		Load Votebox in ./testing/testVotebox.lisp

Notes about the tests:
	Each test loads a bootstrap file, and then any necessary files to run that particular test.
		The bootstrap file (bootstrap.lisp) loads up ccl-simple-view and any shared lisp code that is needed to run a test file (particularly the unit test framework). 
		The necessary files to run the test are defined in ./testing/file-lists/[testName]/[testName].txt. If you wanted to bootstrap the test code manually, load each lisp file in the order in that file.

