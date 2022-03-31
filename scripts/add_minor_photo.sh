#! /bin/sh

while getopts t:d:p:y: flag
do
 case "$flag" in
  t) token=${OPTARG};;
  d) draft_id=${OPTARG};;
  p) minor_photo=${OPTARG};;
  y) minor_photo_image_type=${OPTARG};;
 esac
done

minor_photo_base64=( base64 $minor_photo )

while read name n value
do 
case "$name" in 
 host) host=${value//\"};;
 port) port=$value;;
esac
done < ../Server.config

curl "$host:$port/drafts/minor_photo?token=$token&draft_id=$draft_id" -X POST -d "{\"minor_photo\" : \"$minor_photo_base64\", \"image_type\" : \"$minor_photo_image_type\"}"
