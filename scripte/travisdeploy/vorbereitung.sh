#!/bin/sh

ORT="build/distributions/"

echo "Wechsel nach ${ORT}.";
cd ${ORT};
echo "Vorhandene Dateien in ${ORT}:";
ls -l
echo "Snapshot Dateien löschen, da sie nicht zum Release gehören.";
rm *-SNAPSHOT.*
echo "Tar gzipen, wenn vorhanden.";
TARVORHANDEN=$(ls -l|grep *.tar|wc -l)
if [ "$TARVORHANDEN" = "1" ]; then
  echo "gzipe...";
  gzip MediathekView-*.tar;
  echo "SHA-1 vom tar löschen";
  rm MediathekView-*.tar.SHA-1;
fi
echo "Checksumme vom tar.gz erzeugen.";
for i in `ls -x -1 |grep *.tar.gz`; do
  sha1sum $i|cut -d' ' -f1 > $i.SHA-1; #Checksumme vom tar.gz erzeugen
done

cd ../../
