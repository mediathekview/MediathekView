#!/bin/sh
#
# MediathekView muss mit mindestens 1GB RAM gestartet werden:
# java -Xmx1G -jar ./MediathekView.jar "$@"


#dir=`dirname "$0"`
dir=$(dirname $(readlink -f "$0"))
cd "$dir"

if [ -n "$JAVA_HOME" ]; then
  $JAVA_HOME/bin/java -Xmx1G -jar ./MediathekView.jar "$@"
else
  java -Xmx1G -jar ./MediathekView.jar "$@"
fi
cd $OLDPWD
