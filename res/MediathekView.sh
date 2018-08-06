#!/bin/sh
#
# Wenn der Arbeitsspeicher knapp ist, kann das helfen:
# java -Xms128M -Xmx1G -jar ./@JARNAME@ "$@"


#dir=`dirname "$0"`
dir=$(dirname $(readlink -f "$0"))
cd "$dir"

if [ -n "$JAVA_HOME" ]; then
  $JAVA_HOME/bin/java -Xmx1G -jar ./@JARNAME@ "$@"
else
  java -Xmx1G -jar ./@JARNAME@ "$@"
fi
cd $OLDPWD
