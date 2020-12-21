#!/bin/bash

########################################################################
# Package the binaries built on Travis-CI as an AppImage
# By Alexander Finkhaeuser 2019
# For more information, see http://appimage.org/
########################################################################

export ARCH=$(arch)

APP=MediathekView
LOWERAPP=${APP,,}

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
#cp ../MediathekView/MediathekView.sh usr/bin/$LOWERAPP

cat > usr/bin/$LOWERAPP <<'EOF'
#!/bin/sh

dir=$(dirname $(readlink -f "$0"))
cd "$dir"
MediathekView -Xmx2G --enable-preview -jar MediathekView.jar "$@"
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

generate_type2_appimage

cd .. # Go out of AppImage

rm -Rf Appimage

cp out/MediathekView*.AppImage target/

rm -Rf out
