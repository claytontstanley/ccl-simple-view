#!/bin/bash

srcFile=$1

cat $srcFile | grep 'Begin file:' | awk '{print $NF}' | while read file; do
	echo $file;
	lstart=$( cat $srcFile | grep -nF "Begin file: $file" | awk -F: '{print $1}' )
	lstop=$( cat $srcFile | grep -nF "End file: $file" | awk -F: '{print $1}' )
	lstart=$( expr $lstart + 4 )
	lstop=$( expr $lstop - 4 )
	echo $lstart
	echo $lstop
	code=$( cat "$srcFile" | sed -n "$lstart,$lstop p" )
	echo "$code" > ../$file
done
