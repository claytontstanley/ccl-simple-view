#!/bin/bash

echo $BASH_VERSION

checkDependency() {
	if eval "$1"; then 
		echo "$2"
		exit 1
	else
		echo "Passed test: $1"
	fi
}

checkDependency "[[ -z '$(which make)' ]]" "Install make (suggestion: Install XCode and check the box to install command line tools)"
checkDependency "[[ -z '$(which port)' ]]" "Install MacPorts (suggestion: Install using the binary found on MacPorts webpage)"
checkDependency "[[ -z '$(which greadlink)' ]]" "Install greadlink (suggestion: sudo port install coreutils)"
checkDependency "[[ -z '$(which rlwrap)' ]]" "Install rlwrap (suggestion: sudo port install rlwrap)"

echo "Initializing and updating submodules"
git submodule update --init --recursive

echo "Success! Developer environment correctly configured"
