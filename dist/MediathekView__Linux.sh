#!/bin/sh


#dir=`dirname "$0"`
dir=$(dirname $(readlink -f "$0"))
cd "$dir"

if [ -n "$JAVA_HOME" ]; then
  $JAVA_HOME/bin/java -Xms128M -Xmx1G -jar ./MediathekView.jar $*
else
  java -Xms128M -Xmx1G -jar ./MediathekView.jar $*
fi
cd $OLDPWD

