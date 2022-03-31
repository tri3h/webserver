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

main_photo_base64=( base64 $main_photo )

while read name n value
do 
case "$name" in 
 host) host=${value//\"};;
 port) port=$value;;
esac
done < ../Server.config

curl "$host:$port/drafts?token=$token&category_id=$category_id&tag_id=$tag_id&name=$name" -X POST -d "{\"main_photo\": \"$main_photo_base64\", \"description\": \"$description\", \"image_type\" : \"$main_photo_image_type\"}"
