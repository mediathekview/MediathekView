#!/bin/sh

if [ $(hostname) = "beta" ]
then
# nur für den Entwicklungsrechner sinnvoll

# Dateien ins dist-Verzeichnis kopieren
cp -r /mnt/daten/software/Mediathek/Mediathek/bin/* /mnt/daten/software/Mediathek/Mediathek/dist
#cp /mnt/daten/software/Mediathek/Mediathek/bin/Anleitung/Anleitung.pdf /mnt/daten/software/Mediathek/Mediathek/dist

# für Netbeans nochmal
cp -r /mnt/daten/software/Mediathek/Mediathek/bin/* /mnt/daten/software/Mediathek/Mediathek/build
#cp /mnt/daten/software/Mediathek/Mediathek/bin/Anleitung/Anleitung.pdf /mnt/daten/software/Mediathek/Mediathek/build

# Aufräumen
rm /mnt/daten/software/Mediathek/Mediathek/dist/README.TXT
#rm -r /mnt/daten/software/Mediathek/Mediathek/dist/Anleitung

# Programmsets ins www-verzeichnis kopieren
cp /mnt/daten/software/Mediathek/Mediathek/src/mediathek/file/*.xml /mnt/daten/www/online/ZDFMediathekView/programmgruppen/

# zip erstellen
cd /mnt/daten/software/Mediathek/Mediathek/dist/
datum=$(date +%Y.%m.%d )
zip -r MediathekView_3.1.0_$datum.zip .
 
# Dateien ins share-Verzeichnis von VmWare kopieren
cp -r /mnt/daten/software/Mediathek/Mediathek/dist/* /mnt/daten/virtualbox/share/aktMed

fi