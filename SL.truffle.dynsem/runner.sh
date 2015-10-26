#! /bin/bash

set -e

VARIANTPATH="$1"
INPUTARG="$2"
GRAALOUT="$3"
JDKOUT="$4"

CURDIR=`pwd`

cd $VARIANTPATH

echo "Running mvn exec:exec -Dinputfile=$INPUTARG -Dgraaldatafile=$GRAALOUT -Djdkdatafile=$JDKOUT"

mvn exec:exec -Dinputfile=$INPUTARG -Dgraaldatafile=$GRAALOUT -Djdkdatafile=$JDKOUT

cd $CURDIR
