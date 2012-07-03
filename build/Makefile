outFile = ccl-simple-view.lisp

depdir = /Volumes/RedGiant/Projects/mcl-migration 

header = ; ----------------------------------------------------------------------\n; Begin file: $$file\n; ----------------------------------------------------------------------\n\n\n

footer = \n\n\n; ----------------------------------------------------------------------\n; End file: $$file\n; ----------------------------------------------------------------------\n

date = $(shell date)
commitID = $(shell git log -n 1 | grep '^commit' | awk '{print $$2}')

clean:
	rm -f ${outFile} 

ccl-simple-view: clean outFileHeader
	cat file-list.txt post-file-list.txt | while read file; do \
		printf "${header}" >> ${outFile}; \
		cat "../$$file" >> ${outFile}; \
		printf "${footer}" >> ${outFile}; \
	done


outFileHeader:
	cat $@.lisp | sed 's/_GITID_/${commitID}/g' | sed 's/_FILE_/${outFile}/g' | sed 's/_DATE_/${date}/g' >> ${outFile}

deploy:
	cd ${depdir}; git ru; git rb origin/master; git submodule init; git su; git submodule foreach git submodule init; git su
	cd ${depdir}; make -C build ccl-simple-view
	cd ${depdir}; make -C testing continuous-integration

