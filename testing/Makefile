# Think text substitution / template programming. I don't use Make to compile C files. I use it
# as a templating language to manipulate Bash commands.

testFiles = testCCLDevice.lisp testTutorials.lisp testVotebox.lisp

testString = ${testFiles:%=-l % }

cclQuit = -e '(quit)'

ccl = ./ccl

unit:
	${ccl} ${testString} ${cclQuit}
