#!/bin/sh

dir=$(dirname $(readlink -f "$0"))
cd "$dir"

mediathekview="./MediathekView.jar"
einstellungen="./Einstellungen/.mediathek3"

if [ -f ../MediathekView.jar ]; then
	mediathekview="../MediathekView.jar"
	einstellungen="../Einstellungen/.mediathek3"
fi

echo
echo ============================================
echo Programmdatei: $mediathekview
echo Einstellungen: $einstellungen
echo ============================================
echo

if [ -n "$JAVA_HOME" ]; then
  $JAVA_HOME/bin/java -Xms128M -Xmx1G -jar $mediathekview $einstellungen "$@"
else
  java -Xms128M -Xmx1G -jar $mediathekview $einstellungen "$@"
fi

cd $OLDPWD

