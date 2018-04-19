#!/bin/sh

# The wrapper around Parser
# A simple bash script to take arguments as file descriptors and run them through
# our parser!

set -eu 

haskell_exe=".cabal-sandbox/bin/parser-exe"
if [ ! -f ${haskell_exe} ]; then
    echo "The Parser package is not installed"
    echo "Run ./install.sh to install the package."
    exit 1
fi

if [ $# = 1 ]; then
    file_name=$1
    if [ ! -f ${file_name} ]; then
        echo "${file_name} not found! Please give a valid file to parse."
        exit 1
    fi
    .cabal-sandbox/bin/parser-exe ${file_name}

else 
    .cabal-sandbox/bin/parser-exe
fi




