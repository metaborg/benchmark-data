#! /bin/bash

set -e

PROJECT="$1"
GOALS="$2"

CURDIR=`pwd`

cd $PROJECT

mvn $GOALS

cd $CURDIR
