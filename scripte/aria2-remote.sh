#!/bin/bash

# Das Skript muss evtl. noch 
# "ausführbar" gemacht werden!!!

# Programmparameter in MediathekView
# %f ** <URL of aria2 daemon> <secret for aria2 daemon>

# oder ohne gesetztes Passwort
# %f ** <URL of aria2 daemon>

# Start von aria2 auf dem Server
# aria2c --enable-rpc --rpc-listen-all --dir=/ZIELPFAD/FILME


log=/tmp/MediathekView-aria2.log
echo "Running $0 $*" >$log

url=$1
filename=$(basename "$2")

aria2_server_url=$3
aria2_server_secret=$4

#echo >>$log
#echo "MediathekView-aria2 parameters:" >>$log
#while [ -n "$1" ]
#do
#  echo "$1" >>$log
#  shift
#done

echo >>$log
echo "url: " 					$url >>$log
echo "filename: " 				$filename >>$log
echo "aria2_server_url: " 		$aria2_server_url >>$log
echo "aria2_server_secret: " 	$aria2_server_secret >>$log

id=medview
method=aria2.addUri

if [ -n "$filename" ]
then
  options=",{\"out\":\"${filename}\"}"
else
  options=""
fi

params="[\"token:${aria2_server_secret}\",[\"${url}\"]${options}]"
params_base64enc=$(echo "${params}" | base64 -w 0 -)
params_base64enc_urlenc=${params_base64enc//=/%3D}

get="${aria2_server_url}?id=${id}&method=${method}&params=${params_base64enc_urlenc}"

echo >>$log
echo "executing: $get" >>$log

result=$(curl -S -k "$get")

echo >>$log
echo "result: $result" >>$log

if [[ $result =~ '"result":' ]]
then
  exit 0
else
  exit 1
fi

