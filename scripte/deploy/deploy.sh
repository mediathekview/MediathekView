#!/bin/bash

BATCHDATEI="target/uploadbatch"
LOCAL="target"
REMOTE="upload"

STATUSDATEI="target/upload.status"
PLATTFORMDATEI="target/plattform.txt"
COMMITDATEI="target/gitcommithash.txt"

PORT="52150"
ADRESSE="deploy@mediathekview.de"
KEYFILE="scripte/deploy/deploy.key"

echo "Deploy zu Hauptserver";
# Rechte am Key nur dem Benutzer geben, ansonsten meckert ssh
chmod 600 $KEYFILE


if [ "$1" = "nightly" ]; then

  echo "Deploye nightly Build mit commit '$3' für Platform '$2'"

  echo 2 > $STATUSDATEI

  echo $3 > $COMMITDATEI

else
  echo "Deploye Release für Platform '$2'"

  echo 1 > $STATUSDATEI
fi

echo $2 > $PLATTFORMDATEI

#Update xml für platform umbenennen
for file in $(find $LOCAL/ -type f \( -name 'updates*.xml' \)); do
  mv $file "updates-$2.xml"
done

# Ins Verzeichnis wechseln Befehl
echo "cd $REMOTE" >> $BATCHDATEI

for file in $(find $LOCAL/ -type f \( -name '*.zip' -o -name '*.gz' -o -name '*.AppImage' -o -name 'MediathekView*.exe' -o -name '*.deb' -o -name '*.rpm' -o -name 'MediathekView*.sh' -o -name 'updates*.xml' \)); do
  # einzelne fertige Dateien hochladen
  echo "put $file" >> $BATCHDATEI
done

echo "cd ../" >> $BATCHDATEI

if [ "$1" = "nightly" ]; then
  echo "put $COMMITDATEI" >> $BATCHDATEI
fi

echo "put $PLATTFORMDATEI" >> $BATCHDATEI

# Upload fertig bestätigen
echo "put $STATUSDATEI" >> $BATCHDATEI 

echo "exit" >> $BATCHDATEI

# SFTP Batchdatei ausführen
sftp -b $BATCHDATEI -o PubkeyAuthentication=yes -o IdentityFile=$KEYFILE -o Port=$PORT $ADRESSE

# Aufräumen
rm $BATCHDATEI $STATUSDATEI

