
outFile = ccl-gui.lisp



header = ; ----------------------------------------------------------------------\n; Begin file: $$file\n; ----------------------------------------------------------------------\n\n\n

footer = ; ----------------------------------------------------------------------\n; End file: $$file\n; ----------------------------------------------------------------------\n\n\n

date = $(shell date)
commitID = $(shell git log -n 1 | grep '^commit' | awk '{print $$2}')

cclgui: clean outFileHeader
	cat file-list.txt | while read file; do \
		printf "${header}" >> ${outFile}; \
		cat "../$$file" >> ${outFile}; \
		printf "${footer}" >> ${outFile}; \
	done

clean:
	rm -f ${outFile} 

outFileHeader:
	cat $@.lisp | sed 's/_GITID_/${commitID}/g' | sed 's/_FILE_/${outFile}/g' | sed 's/_DATE_/${date}/g' >> ${outFile}





