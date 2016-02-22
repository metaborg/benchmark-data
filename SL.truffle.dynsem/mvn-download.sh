#! /bin/bash

set -e

PROJECT="$1"
GROUPID="$2"
ARTIFACTID="$3"
VERSION="$4"
PACKAGE="$5"

mvn dependency:copy -Dmdep.stripClassifier=true -Dmdep.stripVersion=true -DoverWrite=true -DartifactId=$ARTIFACTID -DgroupId=$GROUPID -Dartifact=$GROUPID:$ARTIFACTID:$VERSION:$PACKAGE -Dproject.basedir=.
