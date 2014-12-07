#!/bin/sh

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
  $JAVA_HOME/bin/java -Xms128M -Xmx1G -jar $mediathekview $einstellungen "$@"
else
  java -Xms128M -Xmx1G -jar $mediathekview $einstellungen "$@"
fi
