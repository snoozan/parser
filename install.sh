#!/bin/sh

# A simple bash install script - since we're using the CS machines!
# Please run this to set up the project

set -eu

cabal sandbox init
cabal update
cabal install -j
echo "Parser successfully installed! Feel free to run kladoi-parser.sh"
