#!/bin/sh

#VERSION=13
#BUILD=003
#DATE=13.08.2016 / 18\:34\:42


ver=13
echo $ver

nr=$(cat src/version.properties | grep BUILD | sed 's#BUILD=##g')
echo $nr

nr=$((nr + 1))
echo $nr

buildDate=$(date +%d.%m.%y\ %H:%M:%S)
echo $buildDate

echo VERSION=$ver > src/version.properties
echo BUILD=$nr >> src/version.properties
echo DATE=$buildDate >> src/version.properties

echo VERSION=$ver > /home/emil/daten/software/mediathek/mSearch/src/version.properties
echo BUILD=$nr >> /home/emil/daten/software/mediathek/mSearch/src/version.properties
echo DATE=$buildDate >> /home/emil/daten/software/mediathek/mSearch/src/version.properties

