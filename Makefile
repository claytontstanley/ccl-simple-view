
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

exclude-list = ^bincarbon*|^testing

list-cmd = git ls-files

reformat:
	${list-cmd} | grep '.lisp$$' | egrep -v '${exclude-list}' | /usr/bin/xargs -n 1 -o -I {} bash -ic "echo '{}'; ai {} || true"

reformat-repo:
	make reformat

reformat-builds:
	make reformat list-cmd="cat build/file-list*"

# This is way experimental, but it did work for me. 
convertToWriteRepo:
	cd .git; find . -type f -name config -exec perl -pi -e 's|git://cstanley.no-ip.biz|ssh://raid\@cstanley.no-ip.biz/~/F/root/clayton.stanley/srv/git|' '{}' \;

