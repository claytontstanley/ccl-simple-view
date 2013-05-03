
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

reformat-all spelling-all: %-all:
	make $* lst-cmd="git ls-files" exclude-list="^bincarbon|^testing"

reformat-builds spelling-builds: %-builds:
	make $* list-cmd="cat build/file-list*"

reformat-testing spelling-testing: %-testing:
	make $* list-cmd="find testing -mindepth 1 -maxdepth 1 -name '*.lisp'"

# This is way experimental, but it did work for me. 
convertToWriteRepo:
	cd .git; find . -type f -name config -exec perl -pi -e 's|git://cstanley.no-ip.biz|ssh://raid\@cstanley.no-ip.biz/~/F/root/clayton.stanley/srv/git|' '{}' \;

