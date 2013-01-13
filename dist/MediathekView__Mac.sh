#!/bin/sh

dir=`dirname "$0"`
cd "$dir"

if [ -n "$JAVA_HOME" ]; then
	$JAVA_HOME/bin/java -Xdock:name="MediathekView" -Xms128M -Xmx1G -jar ./MediathekView.jar $*
else
	java -Xdock:name="MediathekView" -Xms128M -Xmx1G -jar ./MediathekView.jar $*
fi
cd $OLDPWD

