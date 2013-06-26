#!/bin/bash

pathToThisDir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

AG="46 46 49 4C 44 4D 4F 56 01 40 FF FF FF FF 00 00
00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00"

xattr -wx com.apple.FinderInfo "$AG" "$pathToThisDir/Avant Garde"
xxd -r -p "$pathToThisDir/Avant Garde rsrcfork.txt" >  "$pathToThisDir/Avant Garde/..namedfork/rsrc"
