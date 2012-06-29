# Think text substitution / template programming. I don't use Make to compile C files. I use it
# as a templating language to manipulate Bash commands.

testFiles = testSound.lisp testGetText.lisp testImages.lisp testCCLDevice.lisp testTutorials.lisp testVotebox.lisp
initFiles = 

testString = ${testFiles:%=-l % }
initString = ${initFiles:%=-l % }

cclQuit = -e '(quit)'

ccl = rlwrap ./ccl

unit:
	${ccl} ${initString} ${testString} ${cclQuit}


continuous-integration:
	make -C ../build cclgui
	make unit initFiles='actr6/load-act-r-6.lisp ../build/ccl-simple-view.lisp'
