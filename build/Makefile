actrPath = ../submodules/actr6
server = chil
outFile = ccl-simple-view.lisp

depdir = /Volumes/RedGiant/Projects/mcl-migration 
repo = ~/src/mcl-migration

header = ; ----------------------------------------------------------------------\n; Begin file: $$file\n; ----------------------------------------------------------------------\n\n\n

footer = \n\n\n; ----------------------------------------------------------------------\n; End file: $$file\n; ----------------------------------------------------------------------\n

date = $(shell date)
commitID = $(shell git log -n 1 | grep '^commit' | awk '{print $$2}')

outFiles = ccl-simple-view.lisp device.lisp uwi.lisp
decompileTargets = ${outFiles:%=decompile-%}
rrepos = remote-deployed-repositories.txt

deploy-test = continuous-integration
rebuild-kernel-p = true

all: ${outFiles}

ccl-simple-view.lisp : fileList = file-list.txt
device.lisp : fileList = file-list-device.txt
uwi.lisp : fileList = file-list-uwi.txt

path-device.lisp = ${actrPath}/devices/ccl-cocoa
path-uwi.lisp = ${path-device.lisp}
path-ccl-simple-view.lisp = ${actrPath}/support

git-is-clean = [[ -z "$$(git st --porcelain)" ]]

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

update-remote-repositories:
	set -e && cat ./${rrepos} | while read repo; do echo $$repo && ssh -n ${server} "cd \"$$repo\" && git ru && git rb && git su"; done


rebuild-kernel:
	[[ "${rebuild-kernel-p}" != "true" ]] || ssh ${server} 'cd ${depdir} && rlwrap testing/ccl -e "(rebuild-ccl :full t)" -e "(quit)"'

deploy-bincarbon:
	cd ${repo}/bincarbon && ${git-is-clean} && git push

deploy:
	make deploy-bincarbon	
	cd ${repo} && ${git-is-clean} && git push
	make update-remote-repositories
	ssh ${server} 'cd ${depdir} && git ru && git rb origin/master && git submodule update --init --recursive && git submodule sync'
	ssh ${server} 'cd ${depdir} && ${git-is-clean}'
	make rebuild-kernel
	cd ${depdir} && make -C build all
	cd ${depdir} && make -C docs/README
	ssh ${server} 'cd ${depdir} && make -C testing ${deploy-test} 
	ssh ${server} 'cd ${depdir} && ${git-is-clean}'
