#!/bin/sh

if [ $(hostname) != "beta" ] && [ $(hostname) != "lt" ]
then
    # nur für den Entwicklungsrechner sinnvoll
    echo nix zu tun
    exit
fi


dir=`dirname "$0"`
cd "$dir"

# Icons ins res kopieren
cp src/mediathek/res/programm/* res/Icons/Programm/Version-13

# Dateien ins dist-Verzeichnis kopieren
rm dist/README.TXT  #Aufräumen
cp -r res/* dist

# libs-Verzeichnis füllen
rm libs/*.jar
cp -r dist/lib/* libs

# orange-extensions-1.3.0.jar entfernen, ist nur für OS X und da ists eh schon dabei
rm dist/lib/orange-extensions-1.3.0.jar

# für Netbeans nochmal
cp -r res/* build


# release
ver=$(cat src/version.properties | grep VERSION | sed 's#VERSION=##g')
release=$(cat src/version.properties | grep BUILD | sed 's#BUILD=##g')
buildDate=$(date +%d.%m.%Y )
dateToday=$(date +%Y.%m.%d )

fileName=MediathekView_${ver}__${dateToday}.zip
pathName=MediathekView_${ver}

echo ===========================
echo Version: $ver
echo Datum: $buildDate
echo Release: $release
echo Filename: $fileName
echo Pathname: $pathName
echo ===========================

echo Datum: $buildDate >> dist/Info/$release.build
echo Version: $ver >> dist/Info/$release.build
echo Buildnummer: $release >> dist/Info/$release.build

# zip erstellen
mv dist $pathName
zip -r $fileName $pathName  > /dev/null
mv $pathName dist
mv $fileName dist


if [ $(hostname) = "beta" ]
then
# nur für den Entwicklungsrechner sinnvoll

    # Programmsets ins www-verzeichnis kopieren
    cp src/mediathek/file/*.xml /home/emil/daten/www/online/ZDFMediathekView/programmgruppen11/

    # Dateien ins share-Verzeichnis von VmWare kopieren
    rm -r /mnt/lager/virtualbox/share/aktMed/*
    cp -r dist/* /mnt/lager/virtualbox/share/aktMed

fi

cd $OLDPWD


