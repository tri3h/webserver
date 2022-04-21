#! /bin/sh

while getopts t:d:i: flag
do
 case "$flag" in
  t) token=${OPTARG};;
  d) draft_id=${OPTARG};;
  i) minor_photo_id=${OPTARG};;
 esac
done

while read name_ n value
do 
case "$name_" in 
 host) host=${value//\"};;
 port) port=$value;;
esac
done < ../Configs/Server.config

curl "$host:$port/drafts/minor_photo?token=$token&draft_id=$draft_id&minor_photo_id=$minor_photo_id" -X DELETE
