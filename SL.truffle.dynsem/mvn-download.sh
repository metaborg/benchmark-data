#! /bin/bash

set -e

PROJECT="$1"
GROUPID="$2"
ARTIFACTID="$3"
VERSION="$4"

mvn dependency:copy -DartifactId=$ARTIFACTID -DgroupId=$GROUPID -Dartifact=$GROUPID:$ARTIFACTID:$VERSION -Dproject.basedir=.
