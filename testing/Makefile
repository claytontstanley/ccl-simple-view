testFiles = testCCLDevice.lisp testTutorials.lisp testVotebox.lisp

testString = ${testFiles:%=-l % }

cclQuit = -e '(quit)'

unit:
	ccl ${testString} ${cclQuit}
