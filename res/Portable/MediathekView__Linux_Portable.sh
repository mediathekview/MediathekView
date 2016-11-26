#!/bin/sh
#
# Wenn der Arbeitsspeicher knapp ist, kann das helfen:
# java -Xms128M -Xmx1G -jar ./@JARNAME@ "$@"


dir=$(dirname $(readlink -f "$0"))
cd "$dir"

mediathekview="./@JARNAME@"
einstellungen="./Einstellungen/.mediathek3"

if [ -f ../@JARNAME@ ]; then
	cd ..
fi

echo
echo ============================================
echo Programmverzeichnis: $PWD
echo Programmdatei: $mediathekview
echo Einstellungen: $einstellungen
echo ============================================
echo

if [ -n "$JAVA_HOME" ]; then
  $JAVA_HOME/bin/java -jar $mediathekview $einstellungen "$@"
else
  java -jar $mediathekview $einstellungen "$@"
fi
