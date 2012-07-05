#!/bin/sh

cp -r /mnt/daten/software/java/Mediathek_3/bin/* /mnt/daten/software/java/Mediathek_3/dist
cp /mnt/daten/software/java/Mediathek_3/bin/Anleitung/Anleitung.pdf /mnt/daten/software/java/Mediathek_3/dist

# für Netbeans nochmal
cp -r /mnt/daten/software/java/Mediathek_3/bin/* /mnt/daten/software/java/Mediathek_3/build
cp /mnt/daten/software/java/Mediathek_3/bin/Anleitung/Anleitung.pdf /mnt/daten/software/java/Mediathek_3/build

# Aufräumen
rm /mnt/daten/software/java/Mediathek_3/dist/README.TXT
rm -r /mnt/daten/software/java/Mediathek_3/dist/Anleitung

cp /mnt/daten/software/java/Mediathek_3/src/mediathek/file/*.xml /mnt/daten/www/online/ZDFMediathekView/programmgruppen/

cd /mnt/daten/software/java/Mediathek_3/dist/

# zip erstellen

datum=$(date +%Y.%m.%d )
zip -r MediathekView_3.0.0_$datum.zip .
 