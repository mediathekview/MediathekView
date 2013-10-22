#!/bin/sh

if [ $(hostname) = "beta" ]
then
# nur für den Entwicklungsrechner sinnvoll

# Dateien ins dist-Verzeichnis kopieren
cp -r /mnt/daten/software/Mediathek/Mediathek/bin/* /mnt/daten/software/Mediathek/Mediathek/dist
cp -r /mnt/daten/software/Mediathek/Mediathek/dist/lib/* /mnt/daten/software/Mediathek/Mediathek/libs

# für Netbeans nochmal
cp -r /mnt/daten/software/Mediathek/Mediathek/bin/* /mnt/daten/software/Mediathek/Mediathek/build

# Aufräumen
rm /mnt/daten/software/Mediathek/Mediathek/dist/README.TXT
#rm -r /mnt/daten/software/Mediathek/Mediathek/dist/Anleitung

# Programmsets ins www-verzeichnis kopieren
cp /mnt/daten/software/Mediathek/Mediathek/src/mediathek/file/*.xml /mnt/daten/www/online/ZDFMediathekView/programmgruppen/

# release
relNr=$(cat /mnt/daten/software/Mediathek/Mediathek/src/version.properties | grep BUILD | sed 's#BUILD=##g')
datum=$(date +%d.%m.%Y )
echo Datum: $datum >> /mnt/daten/software/Mediathek/Mediathek/dist/info/$relNr.build
echo MediathekView Buildnummer: $relNr >> /mnt/daten/software/Mediathek/Mediathek/dist/info/$relNr.build

# zip erstellen
cd /mnt/daten/software/Mediathek/Mediathek/dist/
datum=$(date +%Y.%m.%d )
zip -r MediathekView_3.3.0_$datum.zip .

# Dateien ins share-Verzeichnis von VmWare kopieren
cp -r /mnt/daten/software/Mediathek/Mediathek/dist/* /mnt/daten/virtualbox/share/aktMed

fi
