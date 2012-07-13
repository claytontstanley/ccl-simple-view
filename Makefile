zip-%:
	make -C testing clean-actr-compiled-files
	make -C build ccl-simple-view
	rm -f $*.zip	
	make -s file-list-$* | while read line; do zip --symlinks -r $*.zip "$$line"; done


file-list-all : fl = Votebox VoteboxKristen Phaser Tutorials
file-list-% : fl = $*



file-list-%:
	cat testing/file-lists/allNotLoaded.txt
	for fl in ${fl}; do cat testing/file-lists/$$fl/*; done
