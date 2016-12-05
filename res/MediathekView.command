#!/bin/sh
#
# Wenn der Arbeitsspeicher knapp ist, kann das helfen:
# java -Xdock:name="MediathekView" -Xms128M -Xmx1G -jar ./@JARNAME@ $*

dir=`dirname "$0"`
cd "$dir"

if [ -n "$JAVA_HOME" ]; then
	$JAVA_HOME/bin/java -Xdock:name="MediathekView" -jar ./@JARNAME@ $*
else
	java -Xdock:name="MediathekView" -jar ./@JARNAME@ $*
fi
cd $OLDPWD
