#! /bin/bash

set -e

VARIANTPATH="$1"
INPUTARG="$2"
GRAALOUT="$3"
JDKOUT="$4"
WARMUPS="$5"
REPEATS="$6"

CURDIR=`pwd`

cd $VARIANTPATH

echo "Running mvn exec:exec -Dinputfile=$INPUTARG -Dgraaldatafile=$GRAALOUT -Djdkdatafile=$JDKOUT -Dcountwarmups=$WARMUPS -Dcountrepeats=$REPEATS"

mvn exec:exec -Dinputfile=$INPUTARG -Dgraaldatafile=$GRAALOUT -Djdkdatafile=$JDKOUT -Dcountwarmups=$WARMUPS -Dcountrepeats=$REPEATS

cd $CURDIR
