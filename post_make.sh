#!/bin/sh

dir=`dirname "$0"`
cd "$dir"

if [ $(hostname) = "beta" ] || [ $(hostname) = "lt" ]
then
# nur für den Entwicklungsrechner sinnvoll

# Dateien ins dist-Verzeichnis kopieren
cp -r res/* dist
cp -r dist/lib/* libs

# für Netbeans nochmal
cp -r res/* build

# Aufräumen
rm dist/README.TXT

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

fi


if [ $(hostname) = "beta" ]
then
# nur für den Entwicklungsrechner sinnvoll

# Programmsets ins www-verzeichnis kopieren
cp src/mediathek/file/*.xml /mnt/daten/www/online/ZDFMediathekView/programmgruppen4/
# Dateien ins share-Verzeichnis von VmWare kopieren
cp -r dist/* /mnt/lager/virtualbox/share/aktMed
fi

cd $OLDPWD
