actrPath = ../submodules/actr6

outFile = ccl-simple-view.lisp

depdir = /Volumes/RedGiant/Projects/mcl-migration 

header = ; ----------------------------------------------------------------------\n; Begin file: $$file\n; ----------------------------------------------------------------------\n\n\n

footer = \n\n\n; ----------------------------------------------------------------------\n; End file: $$file\n; ----------------------------------------------------------------------\n

date = $(shell date)
commitID = $(shell git log -n 1 | grep '^commit' | awk '{print $$2}')

outFiles = ccl-simple-view.lisp device.lisp uwi.lisp
decompileTargets = ${outFiles:%=decompile-%}

all: ${outFiles}

ccl-simple-view.lisp : fileList = file-list.txt
device.lisp : fileList = file-list-device.txt
uwi.lisp : fileList = file-list-uwi.txt

path-device.lisp = ${actrPath}/devices/ccl-cocoa
path-uwi.lisp = ${path-device.lisp}
path-ccl-simple-view.lisp = ${actrPath}/support

ccl-simple-view.lisp:
	make -s outFileHeader > ${path-${@}}/$@
	make -s file-list fileList="${fileList}" >> ${path-${@}}/$@

device.lisp uwi.lisp:
	mkdir -p ${path-${@}}
	make -s file-list fileList="${fileList}" > ${path-${@}}/$@

decompile-%:
	./decompile.sh ${path-${*}}/$*

decompile-all: ${decompileTargets}
	echo "done"

clean-%:
	rm -f ${path-${*}}/$*

clean-all: ${outFiles:%=clean-%}
	echo "done"

file-list:
	cat ${fileList} | while read file; do \
		printf "${header}"; \
		cat "../$$file"; \
		printf "${footer}"; \
	done

outFileHeader:
	cat $@.lisp | sed 's/_GITID_/${commitID}/g' | sed 's/_FILE_/${outFile}/g' | sed 's/_DATE_/${date}/g'

deploy:
	cd ${depdir}; git remote update; git rebase origin/master; git submodule update --init --recursive
	cd ${depdir}; make -C build all
	cd ${depdir}; make -C docs/README
	cd ${depdir}; make -C testing continuous-integration
