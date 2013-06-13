SHELL := /usr/bin/env bash

# Think text substitution / template programming. I don't use Make to compile C files. I use it
# as a templating language to manipulate Bash commands.

actr6 = ../submodules/actr6

cclQuit = -e '(quit)'

pathToCCL := $(shell dirname `greadlink -f ./ccl`)
ccl = rlwrap "${pathToCCL}/Clozure CL64.app/Contents/MacOS/dx86cl64" -e '(gui::start-cocoa-ide)'

binaryFileLst = ade adp bat chm cmd com cpl exe hta ins isp jse lib mde msc msp mst pif scr sct shb sys vb vbe vbs vxd wsc wsf wsh

binaryFileStr = ${binaryFileLst:%= -o -name '*.%'}

test-file:
	-@echo ""	
	-@echo "Running tests for file ${testFile}"
	${ccl} -l "../${testFile}" ${cclQuit}
	-@echo ""

test-files:
	testFiles=($$(cat ${testFiles})) && for file in "$${testFiles[@]}"; do make test-file testFile="$$file"; done

unit: clean-actr-compiled-files
	make -C ../build build-app
	make test-files testFiles=file-lists/Unit/testUnit.txt

continuous-integration:
	make test-files testFiles=file-lists/CI/testCI.txt

clean-actr-compiled-files:
	find ${actr6}/ -name '*.dx64fsl' -o -name '*.cfsl' ${binaryFileStr} | while read line; do rm "$$line"; done

