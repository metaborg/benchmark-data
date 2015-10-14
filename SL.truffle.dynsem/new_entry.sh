#! /bin/bash

set -e

METABORGSLREV=`git --git-dir=/Users/vladvergu/tud/slde/projects/semantics/metaborg-sl/.git rev-parse --short HEAD`
DSREV=`git --git-dir=/Users/vladvergu/tud/slde/projects/semantics/dynsem/.git rev-parse --short HEAD`
GRAALREV=`hg -R ~/tud/slde/software/truffle/graal-root/graal-compiler/ id -i`

echo "$METABORGSLREV,$DSREV,$GRAALREV," >> index.csv
