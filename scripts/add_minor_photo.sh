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

while read name_ n value
do 
case "$name_" in 
 host) host=${value//\"};;
 port) port=$value;;
esac
done < ../Configs/Server.config

base64 $minor_photo > "image.dat"

curl "$host:$port/drafts/minor_photo?token=$token&draft_id=$draft_id&image_type=$minor_photo_image_type" -X POST -F minor_photo=@image.dat

rm "image.dat"
