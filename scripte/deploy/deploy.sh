#!/bin/bash

BATCHDATEI="build/uploadbatch"
LOCAL="build/distributions"
REMOTE="upload"

STATUSDATEI="build/upload.status"
UPLOAD_LASTCOMMITDATEI="build/gitcommithash.txt"
LASTCOMMITPFAD="$HOME/.tmp"
LASTCOMMITDATEI="${LASTCOMMITPFAD}/lastcommit.txt"

PORT="22"
ADRESSE="deploy@mediathekview.de"
KEYFILE="scripte/deploy/deploy.key"

if [ "$1" != "nightly" ]; then
  # Deploy zum Nexus Repo
  echo "Deploy zum Nexus Repo repo.mediathekview.de"
  ./gradlew release

  rm ${LOCAL}/MediathekView-*.tar
  rm ${LOCAL}/MediathekView-*.tar.SHA-1

  # Status auf fertig setzen für release
  echo 1 > $STATUSDATEI
else
  if [ -f "${LASTCOMMITDATEI}" ]; then
    lastcommit=$(cat ${LASTCOMMITDATEI})
    gitlastcommit=$(git log -n 1 --format='%H')
    echo "Letzter Commit im Cache: '${lastcommit}'"
    echo "Letzter Commit im Repo:  '${gitlastcommit}'"
    if [ "${lastcommit}" == "${gitlastcommit}" ]; then
      echo "Commit-Hash hat sich nicht geändert. Nightly-Deploy ausgesetzt."
      exit
    else
      echo "Commit-Hash hat sich geändert. Nightly-Deploy wird ausgeführt."
      echo $gitlastcommit > ${LASTCOMMITDATEI}
    fi
  else
    echo -n "Lastcommit-Status-Datei '${LASTCOMMITDATEI}' nicht vorhanden. Wird angelegt..."
    if [ ! -d "${LASTCOMMITPFAD}" ]; then mkdir ${LASTCOMMITPFAD}; fi
    git log -n 1 --format="%H" > ${LASTCOMMITDATEI}
    echo "ok."
    echo "Nightly-Deploy wird ausgeführt."
  fi
  git log -n 1 --format="%H" > ${UPLOAD_LASTCOMMITDATEI}
  # Status auf fertig setzen für nightly
  echo 2 > $STATUSDATEI
fi

echo "Deploy zum Hauptserver"
# Rechte am Key nur dem Benutzer geben, ansonsten meckert ssh
chmod 600 $KEYFILE

# Ins Verzeichnis wechseln Befehl
echo "cd $REMOTE" >> $BATCHDATEI

for i in `ls -x -1 $LOCAL`; do
  # einzelne fertige Dateien hochladen
  echo "put $LOCAL/$i" >> $BATCHDATEI
done

echo "cd ../" >> $BATCHDATEI
echo "put $UPLOAD_LASTCOMMITDATEI" >> $BATCHDATEI
# Upload fertig bestätigen
echo "put $STATUSDATEI" >> $BATCHDATEI

echo "exit" >> $BATCHDATEI

# SFTP Batchdatei ausführen
sftp -b $BATCHDATEI -o PubkeyAuthentication=yes -o IdentityFile=$KEYFILE -o Port=$PORT $ADRESSE

# Aufräumen
rm $BATCHDATEI $STATUSDATEI $LASTCOMMITDATEI
