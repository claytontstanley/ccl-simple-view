# Think text substitution / template programming. I don't use Make to compile C files. I use it
# as a templating language to manipulate Bash commands.

actr6 = ../submodules/actr6

cclQuit = -e '(quit)'

ccl = rlwrap ./ccl

binaryFileLst = ade adp bat chm cmd com cpl exe hta ins isp jse lib mde msc msp mst pif scr sct shb sys vb vbe vbs vxd wsc wsf wsh

binaryFileStr = ${binaryFileLst:%= -o -name '*.%'}

unit: clean-actr-compiled-files
	make -C ../build all
	${ccl} -e '(require :cocoa)' -l testUnit.lisp ${cclQuit}

# FIXME: Should I do anything different for CI tests compared to unit tests.
# All of the tests are pretty-much regression CI tests anyways. Don't really have unit tests (yet) for this repo
continuous-integration: unit
	${ccl} -e '(require :cocoa)' -l testCI.lisp ${cclQuit}
	[[ -z "$$(git status --porcelain)" ]]

clean-actr-compiled-files:
	find ${actr6}/ -name '*.dx64fsl' -o -name '*.cfsl' ${binaryFileStr} | while read line; do rm "$$line"; done

