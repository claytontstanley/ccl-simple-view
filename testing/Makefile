include ../include.mk

# Think text substitution / template programming. I don't use Make to compile C files. I use it
# as a templating language to manipulate Bash commands.

actr6 = ../submodules/actr6

coverageTempFile = ${TOP}/.coverage.temp
cclQuit = -e '(quit)'
cclStartCoverage = -e '(setf ccl:*compile-code-coverage* t)'
cclSaveCoverage = -e '(save-coverage-in-file "${coverageTempFile}")'

ccl = rlwrap "${pathToCCL}/Clozure CL64.app/Contents/MacOS/dx86cl64" -e '(gui::start-cocoa-ide)'
#ccl = ./ccl

binaryFileLst = ade adp bat chm cmd com cpl exe hta ins isp jse lib mde msc msp mst pif scr sct shb sys vb vbe vbs vxd wsc wsf wsh

binaryFileStr = ${binaryFileLst:%= -o -name '*.%'}

test-file:
	-@echo ""	
	-@echo "Running tests for file ${testFile}"
	${ccl} ${cclStartCoverage} -l "../${testFile}" ${cclSaveCoverage} ${cclQuit}
	-@echo ""

test-files:
	set -e && testFiles=($$(cat ${testFiles})) && for file in "$${testFiles[@]}"; do make test-file testFile="$$file"; done

check-dev-environment:
	../check-dev-env.sh

unit: 
	make clean-actr-compiled-files clean-coverage check-dev-environment
	make -C ../build build-app all
	make test-files testFiles=file-lists/Unit/testUnit.txt

unit-1.%:
	make clean-actr-compiled-files check-dev-environment
	make test-file testFile=testing/testUnit.lisp ccl=./ccl-1.$* cclSaveCoverage='' cclStartCoverage=''

continuous-integration: unit
	make test-files testFiles=file-lists/CI/testCI.txt

clean-actr-compiled-files:
	find ${actr6}/ -name '*.dx64fsl' -o -name '*.cfsl' ${binaryFileStr} | while read line; do rm "$$line"; done

clean-coverage:
	rm -f ${coverageTempFile}

coverage:
	${ccl} -l "make-coverage.lisp" ${cclQuit}
