#!/bin/sh

dir=`dirname "$0"`
cd "$dir"

if [ $(hostname) = "beta" ] || [ $(hostname) = "lt" ]
then
# nur für den Entwicklungsrechner sinnvoll

# Icons ins res kopieren
cp src/mediathek/res/programm/* res/Icons/Programm/Version-12

# Dateien ins dist-Verzeichnis kopieren
cp -r res/* dist

# libs-Verzeichnis füllen
rm libs/*.jar
cp -r dist/lib/* libs

# orange-extensions-1.3.0.jar entfernen, ist nur für OS X und da ists eh schon dabei
rm dist/lib/orange-extensions-1.3.0.jar

# für Netbeans nochmal
cp -r res/* build

# Aufräumen
rm dist/README.TXT

# release
relNr=$(cat src/version.properties | grep BUILD | sed 's#BUILD=##g')
datum=$(date +%d.%m.%Y )
echo Datum: $datum >> dist/Info/$relNr.build
echo MediathekView Buildnummer: $relNr >> dist/Info/$relNr.build

# zip erstellen
cd dist/
datum=$(date +%Y.%m.%d )
zip -r MediathekView_12_$datum.zip .
cd ..

fi


if [ $(hostname) = "beta" ]
then
# nur für den Entwicklungsrechner sinnvoll

# Programmsets ins www-verzeichnis kopieren
cp src/mediathek/file/*.xml /home/emil/daten/www/online/ZDFMediathekView/programmgruppen11/
# Dateien ins share-Verzeichnis von VmWare kopieren
rm -r /mnt/lager/virtualbox/share/aktMed/*.zip
cp -r dist/* /mnt/lager/virtualbox/share/aktMed
fi

cd $OLDPWD
