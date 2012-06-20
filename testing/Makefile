testFiles = testCCLDevice.lisp testTutorials.lisp testVotebox.lisp
testString = ${testFiles:%=-l % }


${hostnames:%=run-command-%}


cclQuit = -e '(quit)'

unit:
	ccl ${testString} ${cclQuit}
