zip-%:
	make -C testing clean-actr-compiled-files
	make -C build ccl-simple-view
	rm -f $*.zip	
	make -s file-list-$* | while read line; do zip --symlinks -r $*.zip "$$line"; done

file-list-%:
	cat testing/file-lists/$*/*
