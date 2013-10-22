include include.mk

TOP := $(shell pwd)

zip-% : zipDir = .
zip-actr6 : zipDir = submodules/actr6

zip-%: clean-%
	make -C testing clean-actr-compiled-files
	make -C build all
	rm -f $*.zip	
	cd ${zipDir}; make -C ${TOP} -s file-list-$* | while read line; do zip --symlinks -r $*.zip "$$line"; done

clean-%:
	echo "done"

clean-tamborello:
	rm -rf submodules/tambo-diss/tamborello-dissertation-model/model-data/

file-list-all : fl = Votebox VoteboxKristen Phaser Tutorials
file-list-tamborello: fl = Phaser TamboDis TamboDisModel
file-list-gallagher: fl = Gallagher Gallagher112
file-list-bincarbon: fl = TouchKeyboard PasswordEntryTextView DMTracker ReplayExperimentWindow 
file-list-% : fl = $*

file-list-actr6:
	echo "support/ccl-simple-view.lisp"
	echo "devices/ccl-cocoa/device.lisp"
	echo "devices/ccl-cocoa/uwi.lisp"

file-list-%:
	cat testing/file-lists/*.txt
	for fl in ${fl}; do cat testing/file-lists/$$fl/*; done

exclude-list = ^$$
list-cmd =

reformat: vicmd = ai
spelling: vicmd = cs

reformat spelling:
	${list-cmd} | grep '.lisp$$' | egrep -v '${exclude-list}' | /usr/bin/xargs -n 1 -o -I {} bash -ic "echo '{}'; ${vicmd} {} || true"

%-all: lst-cmd = (git ls-files && cd bincarbon && git ls-files | perl -pe 's|^|bincarbon/|')

reformat-all spelling-all: %-all:
	make $* list-cmd="${lst-cmd}" exclude-list="bincarbon/(CFBundle|base-trek-tasks|pict-svm|procedure-window2|timer|touch-keyboard).lisp$$"

reformat-builds spelling-builds: %-builds:
	make $* list-cmd="cat build/file-list*"

testing-files-included:
	comm <(cat testing/file-lists/CI/testCI.txt testing/file-lists/Unit/testUnit.txt | sort) <(ls -1 testing/*.lisp | sort)

lastSVNPushGitID = 2c657888e2d506d7e9556cc680c58762dc9890

diff-since-last-actr-svn-push:
	git df ${lastSVNPushGitID} build/outFileHeader.lisp actr6/devices/ccl/{device.lisp,uwi.lisp} 

