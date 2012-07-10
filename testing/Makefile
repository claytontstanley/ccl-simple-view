# Think text substitution / template programming. I don't use Make to compile C files. I use it
# as a templating language to manipulate Bash commands.

testFiles = testLevelIndicator.lisp testPolygon.lisp testSound.lisp testGetText.lisp testImages.lisp testCCLDevice.lisp testTutorials.lisp testVotebox.lisp
initFiles = 

testString = ${testFiles:%=-l % }
initString = ${initFiles:%=-l % }

cclQuit = -e '(quit)'

ccl = rlwrap ./ccl

unit: clean-actr-compiled-files
	${ccl} ${initString} ${testString} ${cclQuit}

continuous-integration:
	make -C ../build ccl-simple-view
	make unit initFiles='actr6/load-act-r-6.lisp ../build/ccl-simple-view.lisp'

clean-actr-compiled-files:
	find actr6/ -name '*.dx64fsl' -exec rm '{}' \;

zip-%: clean-actr-compiled-files
	make -C ../build ccl-simple-view
	rm -f $*.zip	
	make -s file-list-$* | while read line; do zip -r $*.zip "$$line"; done

file-list-%:
	cat file-lists/$*/*
