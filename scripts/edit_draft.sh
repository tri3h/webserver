#! /bin/sh

while getopts t:d:c:i:n:d:m: flag
do
 case "$flag" in
  t) token=${OPTARG};;
  a) draft_id=${OPTARG};;
  c) category_id=${OPTARG};;
  i) tag_id=${OPTARG};;
  n) name=${OPTARG// /+};;
  d) description=${OPTARG};;
  m) main_photo=${OPTARG};;
 esac
done

while read name n value
do 
case "$name" in 
 host) host=${value//\"};;
 port) port=$value;;
esac
done < ../Server.config

base64 $main_photo > "image.dat"

curl "$host:$port/drafts?token=$token&draft_id=$draft_id&category_id=$category_id&tag_id=$tag_id&name=$name&description=$description&image_type=$main_photo_image_type" -X PUT -F main_photo=@image.dat

rm "image.dat"
