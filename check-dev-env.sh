#!/bin/bash

checkDependency() {
	if eval "$1"; then 
		echo "Passed test: $1"
	else
		printf "$2\n"
		exit 1
	fi
}

checkNoPassword() {
	ip=$1
	status=$(ssh -o BatchMode=yes -o ConnectTimeout=5 $ip echo ok 2>&1)
	[[ $status == ok ]]
}

checkSSHHostname() {
	egrep -iq "^Host[[:space:]]+$1$" ~/.ssh/config
}

hostnameTemplate=$(cat <<'EOF'
Host chil
	User		clayton	
	HostName 	chil.rice.edu
EOF
)

checkDependency "[[ -n '$(which make)' ]]" "Install make (suggestion: Install XCode and check the box to install command line tools)"
checkDependency "[[ -n '$(which port)' ]]" "Install MacPorts (suggestion: Install using the binary found on MacPorts webpage)"
checkDependency "[[ -n '$(which greadlink)' ]]" "Install greadlink (suggestion: sudo port install coreutils)"
checkDependency "[[ -n '$(which rlwrap)' ]]" "Install rlwrap (suggestion: sudo port install rlwrap)"
checkDependency "[[ -n '$(which ssh-copy-id)' ]]" "Install ssh-copy-id (suggestion: sudo port install openssh)"
checkDependency "checkSSHHostname chil" "chil hostname not configured: add something like this (replace User field with your chil login name) to ~/.ssh/config: \n$hostnameTemplate"
checkDependency "checkNoPassword chil" "chil not good: use ssh-copy-id to enable passwordless login to chil using public/private keys; otherwise submodule updating gets tedious"

echo "Initializing and updating submodules"
git submodule update --init --recursive

echo "Success! Developer environment correctly configured"
