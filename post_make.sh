#!/bin/sh

if [ $(hostname) = "beta" ]
then
# nur für den Entwicklungsrechner sinnvoll

dir=`dirname "$0"`
cd "$dir"

# Dateien ins dist-Verzeichnis kopieren
cp -r res/* dist
cp -r dist/lib/* libs

# für Netbeans nochmal
cp -r res/* build

# Aufräumen
rm dist/README.TXT

# Programmsets ins www-verzeichnis kopieren
cp src/mediathek/file/*.xml /mnt/daten/www/online/ZDFMediathekView/programmgruppen4/

# release
relNr=$(cat src/version.properties | grep BUILD | sed 's#BUILD=##g')
datum=$(date +%d.%m.%Y )
echo Datum: $datum >> dist/info/$relNr.build
echo MediathekView Buildnummer: $relNr >> dist/info/$relNr.build

# zip erstellen
cd dist/
datum=$(date +%Y.%m.%d )
zip -r MediathekView_6_$datum.zip .
cd ..

# Dateien ins share-Verzeichnis von VmWare kopieren
cp -r dist/* /mnt/lager/virtualbox/share/aktMed

cd $OLDPWD

fi
