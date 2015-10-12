#! /bin/bash

set -e

TRUFFLESLREV=`git --git-dir=/Users/vladvergu/tud/slde/projects/semantics/truffle-sl/.git rev-parse --short HEAD`
METABORGSLREV=`git --git-dir=/Users/vladvergu/tud/slde/projects/semantics/metaborg-sl/.git rev-parse --short HEAD`
DSREV=`git --git-dir=/Users/vladvergu/tud/slde/projects/semantics/dynsem/.git rev-parse --short HEAD`

echo "$TRUFFLESLREV,$METABORGSLREV,$DSREV," >> index.csv
