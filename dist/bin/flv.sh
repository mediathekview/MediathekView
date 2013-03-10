#! /bin/sh
#

# $1 ist der flvstreamer
# $2 ist der vlc-player
# $3 .. ist die url und parameter
# der Aufruf könnte also so aussehen:

# flvstreamer -r URL -o zielpfad/datei --resume | vlc -

$1 -q $3 $4 $5 $6 $7 $8 $9 | $2 -
