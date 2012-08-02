zip-%:
	make -C testing clean-actr-compiled-files
	make -C build ccl-simple-view
	rm -f $*.zip	
	make -s file-list-$* | while read line; do zip --symlinks -r $*.zip "$$line"; done


file-list-all : fl = Votebox VoteboxKristen Phaser Tutorials
file-list-tamborello: fl = Phaser TamboDis TamboDisModel
file-list-% : fl = $*



file-list-%:
	cat testing/file-lists/*.txt
	for fl in ${fl}; do cat testing/file-lists/$$fl/*; done

exclude-list = ^bincarbon*|^testing|^actr6/devices/ccl/device.lisp|^rmcl/lib/ccl-menus.lisp

reformat:
	git ls-files | grep '.lisp$$' | egrep -v '${exclude-list}' | xargs -n 1 -o -I {} bash -ic "ai {} || true"

# This is way experimental, but it did work for me. 
convertToWriteRepo:
	cd .git; find . -type f -name config -exec perl -pi -e 's|git://cstanley.no-ip.biz|ssh://raid\@cstanley.no-ip.biz/~/F/root/clayton.stanley/srv/git|' '{}' \;

