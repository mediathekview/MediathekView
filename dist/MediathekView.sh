#!/bin/sh

dir=`dirname "$0"`
cd "$dir"

if [ -n "$JAVA_HOME" ]; then
  $JAVA_HOME/bin/java -jar ./MediathekView.jar $*
else
  java -jar ./MediathekView.jar $*
fi
cd $OLDPWD

