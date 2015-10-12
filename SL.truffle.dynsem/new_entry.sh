#! /bin/bash

set -e

TRUFFLESLREV=`git --git-dir=/Users/vladvergu/tud/slde/projects/semantics/truffle-sl/.git rev-parse --short HEAD`
METABORGSLREV=`git --git-dir=/Users/vladvergu/tud/slde/projects/semantics/metaborg-sl/.git rev-parse --short HEAD`
DSREV=`git --git-dir=/Users/vladvergu/tud/slde/projects/semantics/dynsem/.git rev-parse --short HEAD`
GRAALREV=`hg -R ~/tud/slde/software/truffle/graal-root/graal-compiler/ id -i`

echo "$TRUFFLESLREV,$METABORGSLREV,$DSREV,$GRAALREV," >> index.csv
