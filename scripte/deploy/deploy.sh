#!/bin/sh

BATCHDATEI="build/uploadbatch"
LOCAL="build/distributions"
REMOTE="upload"

STATUSDATEI="build/upload.status"

PORT="22"
ADRESSE="deploy@mediathekview.de"
KEYFILE="scripte/deploy/deploy.key"

if [ "$1" != "nightly" ]; then
    # Deploy zum Nexus Repo
    echo "Deploy zum Nexus Repo repo.mediathekview.de"
    ./gradlew release
    
    # Status auf fertig setzen für release
    echo 1 > $STATUSDATEI
else
    # Status auf fertig setzen für nightly
    echo 2 > $STATUSDATEI
fi

rm ${LOCAL}/MediathekView-*.tar;
rm ${LOCAL}/MediathekView-*.tar.SHA-1;

echo "Deploy zu Hauptserver";
# Rechte am Key nur dem Benutzer geben, ansonsten meckert ssh
chmod 600 $KEYFILE

# Ins Verzeichnis wechseln Befehl
echo "cd $REMOTE" >> $BATCHDATEI

for i in `ls -x -1 $LOCAL`; do
  # einzelne fertige Dateien hochladen
  echo "put $LOCAL/$i" >> $BATCHDATEI
done

echo "cd ../" >> $BATCHDATEI
# Upload fertig bestätigen
echo "put $STATUSDATEI" >> $BATCHDATEI 

echo "exit" >> $BATCHDATEI

# SFTP Batchdatei ausführen
sftp -b $BATCHDATEI -o PubkeyAuthentication=yes -o IdentityFile=$KEYFILE -o Port=$PORT $ADRESSE

# Aufräumen
rm $BATCHDATEI $STATUSDATEI


