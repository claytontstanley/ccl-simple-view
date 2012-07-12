# Think text substitution / template programming. I don't use Make to compile C files. I use it
# as a templating language to manipulate Bash commands.

testFiles = testLevelIndicator.lisp testPolygon.lisp testSound.lisp testGetText.lisp testImages.lisp testCCLDevice.lisp testTutorials.lisp testVotebox.lisp
initFiles = 

testString = ${testFiles:%=-l % }
initString = ${initFiles:%=-l % }

actr6 = ../submodules/actr6

cclQuit = -e '(quit)'

ccl = rlwrap ./ccl

binaryFileLst = ade adp bat chm cmd com cpl exe hta ins isp jse lib mde msc msp mst pif scr sct shb sys vb vbe vbs vxd wsc wsf wsh

binaryFileStr = ${binaryFileLst:%= -o -name '*.%'}

unit: clean-actr-compiled-files
	${ccl} ${initString} ${testString} ${cclQuit}

continuous-integration:
	make -C ../build ccl-simple-view
	make unit initFiles='${actr6}/load-act-r-6.lisp ../build/ccl-simple-view.lisp'

clean-actr-compiled-files:
	find ${actr6}/ -name '*.dx64fsl' -o -name '*.cfsl' ${binaryFileStr} | while read line; do rm "$$line"; done

