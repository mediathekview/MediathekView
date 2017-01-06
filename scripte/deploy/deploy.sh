#!/bin/sh

BATCHDATEI="build/uploadbatch"
LOCAL="build/distributions"
REMOTE="upload"

STATUSDATEI="build/upload.status"

PORT="22"
ADRESSE="deploy@mediathekview.de"
KEYFILE="scripte/deploy/deploy.key"

# Deploy zum Nexus Repo
echo "Deploy zum Nexus Repo repo.mediathekview.de"
./gradlew release
rm ${LOCAL}/MediathekView-*.tar;
rm ${LOCAL}/MediathekView-*.tar.SHA-1;

echo "Deploy zu Hauptserver";
# Rechte am Key nur dem Benutzer geben, ansonsten meckert ssh
chmod 600 $KEYFILE

echo 1 > $STATUSDATEI

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

