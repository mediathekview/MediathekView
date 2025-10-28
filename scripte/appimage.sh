#!/bin/bash

########################################################################
# Package the binaries built on Gitlab-CI as an AppImage
# By Alexander Finkhaeuser 2019
# For more information, see http://appimage.org/
########################################################################

export ARCH=$(arch)
export APPIMAGE_EXTRACT_AND_RUN=1

APP=MediathekView
LOWERAPP=${APP,,}
if [ "$1" = "release" ]; then
  echo "Baue AppImage Release Version"
  UPDATESTR="zsync|https://download.mediathekview.de/stabil/MediathekView.AppImage.zsync"
else
  echo "Baue AppImage Nightly Version"
  UPDATESTR="zsync|https://download.mediathekview.de/unstabil/MediathekView.AppImage.zsync"
fi

NO_GLIBC_VERSION=true

mkdir -p Appimage/$APP.AppDir/usr/bin

cd Appimage


cp ../target/media/MediathekView*.tar.gz ./
tar -xzf MediathekView*.tar.gz
# Beispiel Datiename MediathekView-13.6.0-SNAPSHOT-linux.tar.gz
VERSION=$(ls |grep MediathekView-|grep "tar" | cut -d "-" -f 2)

rm ./MediathekView*.tar.gz

# Aktuelle JRE URL ermitteln
#URL_JRE=$(curl -s https://java.com/de/download/linux_manual.jsp |grep -m1 "<a title=\"Download der Java-Software für Linux x64\""|cut -d'"' -f4)
#wget $URL_JRE -O jre-linux-x64.tar.gz

#wget javadl.oracle.com/webapps/download/AutoDL?BundleId=220305_d54c1d3a095b4ff2b6607d096fa80163 -O jre-linux-x64.tar.gz

wget -q https://github.com/probonopd/AppImages/raw/master/functions.sh -O ./functions.sh
. ./functions.sh

cd $APP.AppDir


########################################################################
# Copy desktop and icon file to AppDir for AppRun to pick them up
########################################################################

get_apprun

cat > $LOWERAPP.desktop <<EOF
[Desktop Entry]
Name=$APP
Icon=$LOWERAPP
Exec=$LOWERAPP
Type=Application
Categories=AudioVideo;
Comment=Mediatheken
EOF

#get_icon

########################################################################
# Other appliaction-specific finishing touches
########################################################################

mkdir -p usr/lib/jvm/
cp -R ../MediathekView/jre usr/lib/jvm/
#tar -xzf ../jre* -C usr/lib/jvm/
( cd usr/bin ; ln -s ../lib/jvm/jre*/bin/java ./MediathekView )

cp -R ../MediathekView/bin usr/bin
chmod +x usr/bin/bin/*.sh
cp ../MediathekView/MediathekView.svg $LOWERAPP.svg

cp ../MediathekView/MediathekView.jar usr/bin
chmod +x usr/bin/MediathekView.jar
cp -r ../MediathekView/dependency usr/bin/
#cp ../MediathekView/MediathekView.sh usr/bin/$LOWERAPP

# Copy appdata.xml
mkdir -p usr/share/metainfo
cp ../../res/MediathekView.appdata.xml usr/share/metainfo/MediathekView.appdata.xml

cat > usr/bin/$LOWERAPP <<'EOF'
#!/bin/sh

dir=$(dirname $(readlink -f "$0"))
cd "$dir"
MediathekView -XX:+UseShenandoahGC -XX:ShenandoahGCHeuristics=compact -XX:+UseStringDeduplication -XX:MaxRAMPercentage=50.0 --enable-native-access=ALL-UNNAMED --add-modules jdk.incubator.vector --add-exports=java.desktop/sun.swing=ALL-UNNAMED --add-opens java.desktop/sun.awt.X11=ALL-UNNAMED -ea -cp "MediathekView.jar:dependency/*" -DexternalUpdateCheck mediathek.Main "$@"
cd $OLDPWD
EOF

chmod +x usr/bin/$LOWERAPP

########################################################################
# Delete stuff that should not go into the AppImage
########################################################################

# Delete dangerous libraries; see
# https://github.com/probonopd/AppImages/blob/master/excludelist
delete_blacklisted

########################################################################
# AppDir complete
# Now packaging it as an AppImage
########################################################################

cd .. # Go out of AppImage
echo "--------------------------------------------------------------------------------------------------------"
echo "generate_type2_appimage -u ${UPDATESTR}"

tmp_v1=$CI_COMMIT_REF_NAME
tmp_v2=$CI_JOB_NAME
tmp_v3=$CI_PROJECT_URL
unset CI_COMMIT_REF_NAME
unset CI_JOB_NAME
unset CI_PROJECT_URL

generate_type2_appimage -u ${UPDATESTR}

export CI_COMMIT_REF_NAME=$tmp_v1
export CI_JOB_NAME=$tmp_v2
export CI_PROJECT_URL=$tmp_v3

cd .. # Go out of AppImage

rm -Rf Appimage

cp out/MediathekView*.AppImage target/

rm -Rf out
