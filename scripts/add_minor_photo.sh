#! /bin/sh

token=$(<utility/token.txt)
draft_id="1"
minor_photo="utility/image.png"
minor_photo_image_type="png"

#All parameters are required
while getopts t:d:p:y: flag
do
 case "$flag" in
  t) token=${OPTARG};;
  d) draft_id=${OPTARG};;
  p) minor_photo=${OPTARG};;
  y) minor_photo_image_type=${OPTARG};;
 esac
done

source utility/load_config.sh

base64 $minor_photo > "image.dat"

cat "image.dat"

curl "$host:$port/drafts/minor_photo?token=$token&draft_id=$draft_id&image_type=$minor_photo_image_type" -X POST -F minor_photo=@image.dat

rm "image.dat"
