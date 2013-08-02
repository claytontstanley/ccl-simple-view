include ../include.mk

actrPath = ../submodules/actr6
server = chil
rrServer = chil
outFile = ccl-simple-view.lisp
depdir = ~/src/mcl-migration

header = ; ----------------------------------------------------------------------\n; Begin file: $$file\n; ----------------------------------------------------------------------\n\n\n
footer = \n\n\n; ----------------------------------------------------------------------\n; End file: $$file\n; ----------------------------------------------------------------------\n

date = $(shell date)
commitID = $(shell git log -n 1 | grep '^commit' | awk '{print $$2}')

outFiles = ccl-simple-view.lisp device.lisp uwi.lisp
decompileTargets = ${outFiles:%=decompile-%}
rrepos = remote-deployed-repositories.txt

cclOrigFiles = ${pathToCCL}/dx86cl64 ${pathToCCL}/dx86cl64.image

deployTest = continuous-integration
rebuildKernelP = true

ccl-simple-view.lisp : fileList = file-list.txt
device.lisp : fileList = file-list-device.txt
uwi.lisp : fileList = file-list-uwi.txt

path-device.lisp = ${actrPath}/devices/ccl-cocoa
path-uwi.lisp = ${path-device.lisp}
path-ccl-simple-view.lisp = ${actrPath}/support

gitIsClean = [[ -z "$$(git status --porcelain)" ]]
gitFullUpdate = git remote update && git rebase && git submodule update --init --recursive

all: ${outFiles}

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
	set -e && cat ./${rrepos} | while read repo; do echo $$repo && ssh -n ${rrServer} "cd \"$$repo\" && ${gitFullUpdate}"; done

rebuild-kernel: ${cclOrigFiles}
	[[ "${rebuildKernelP}" != "true" ]] || rlwrap ../testing/ccl -e "(rebuild-ccl :full t)" -e "(quit)"

${cclOrigFiles}:
	cp $@.bak $@

build-app: ${cclOrigFiles} 
	rlwrap ../testing/ccl -l build-app.lisp
	ln -sf ../../../../darwin-x86-headers64 "${pathToCCL}/Clozure CL64.app/Contents/Resources/ccl/"

deploy-bincarbon:
	cd ../bincarbon && ${gitIsClean} && git push

deploy:
	make deploy-bincarbon	
	cd .. && ${gitIsClean} && git push
	make update-remote-repositories
	ssh ${server} 'cd ${depdir} && ${gitFullUpdate} && ${gitIsClean}'
	ssh ${server} 'cd ${depdir} && make -C build rebuild-kernel rebuildKernelP=true'
	ssh ${server} 'cd ${depdir} && make -C build all'
	ssh ${server} 'cd ${depdir} && make -C docs/README'
	ssh ${server} 'cd ${depdir} && make -C testing ${deployTest}'
	ssh ${server} 'cd ${depdir} && make -C testing coverage 
	ssh ${server} 'cd ${depdir} && ${gitIsClean}'

