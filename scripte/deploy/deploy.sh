#!/bin/bash

BATCHDATEI="target/uploadbatch"
LOCAL="target"
REMOTE="upload"

STATUSDATEI="target/upload.status"
COMMITDATEI="target/gitcommithash.txt"

PORT="22"
ADRESSE="deploy@mediathekview.de"
KEYFILE="scripte/deploy/deploy.key"

echo "Deploy zu Hauptserver";
# Rechte am Key nur dem Benutzer geben, ansonsten meckert ssh
chmod 600 $KEYFILE


if [ "$1" = "nightly" ]; then

  echo "Deploye nightly Build mit commit '$2'"

  echo 2 > $STATUSDATEI

  echo $2 > $COMMITDATEI

else
  echo 1 > $STATUSDATEI
fi

# Ins Verzeichnis wechseln Befehl
echo "cd $REMOTE" >> $BATCHDATEI

for file in $(find $LOCAL/ -type f \( -name '*.zip' -o -name '*.gz' \)); do
  # einzelne fertige Dateien hochladen
  echo "put $file" >> $BATCHDATEI
done

echo "cd ../" >> $BATCHDATEI

if [ "$1" = "nightly" ]; then
  echo "put $COMMITDATEI" >> $BATCHDATEI
fi

# Upload fertig bestätigen
echo "put $STATUSDATEI" >> $BATCHDATEI 

echo "exit" >> $BATCHDATEI

# SFTP Batchdatei ausführen
sftp -b $BATCHDATEI -o PubkeyAuthentication=yes -o IdentityFile=$KEYFILE -o Port=$PORT $ADRESSE

# Aufräumen
rm $BATCHDATEI $STATUSDATEI

