#!/bin/sh
#
# Wenn der Arbeitsspeicher knapp ist, kann das helfen:
# java -Xms128M -Xmx1G -jar ./MediathekView.jar "$@"


dir=$(dirname $(readlink -f "$0"))
cd "$dir"

mediathekview="./MediathekView.jar"
einstellungen="./Einstellungen/.mediathek3"

if [ -f ../MediathekView.jar ]; then
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
