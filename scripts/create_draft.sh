#! /bin/sh

while getopts t:c:i:n:d:m:y: flag
do
 case "$flag" in
  t) token=${OPTARG};;
  c) category_id=${OPTARG};;
  i) tag_id=${OPTARG};;
  n) name=${OPTARG// /+};;
  d) description=${OPTARG};;
  m) main_photo=${OPTARG};;
  y) main_photo_image_type=${OPTARG};;
 esac
done

while read name_ n value
do 
case "$name_" in 
 host) host=${value//\"};;
 port) port=$value;;
esac
done < ../Configs/Server.config

base64 $main_photo > "image.dat"

curl "$host:$port/drafts?token=$token&category_id=$category_id&description=$description&tag_id=$tag_id&name=$name&image_type=$main_photo_image_type" -X POST -F main_photo=@image.dat

rm "image.dat"
