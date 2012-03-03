#!/bin/sh

cp -r /mnt/daten/software/java/Mediathek_3/bin/* /mnt/daten/software/java/Mediathek_3/dist
cp /mnt/daten/software/java/Mediathek_3/bin/Anleitung/Anleitung.pdf /mnt/daten/software/java/Mediathek_3/dist

# für Netbeans nochmal
cp -r /mnt/daten/software/java/Mediathek_3/bin/* /mnt/daten/software/java/Mediathek_3/build
cp /mnt/daten/software/java/Mediathek_3/bin/Anleitung/Anleitung.pdf /mnt/daten/software/java/Mediathek_3/build

# Aufräumen
rm /mnt/daten/software/java/Mediathek_3/dist/README.TXT
rm -r /mnt/daten/software/java/Mediathek_3/dist/Anleitung

