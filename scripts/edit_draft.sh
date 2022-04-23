#! /bin/sh

#Required parameters: t, a. Others are optional. 
while getopts t:a:c:i:n:d:m:b: flag
do
 case "$flag" in
  t) token=${OPTARG};;
  a) draft_id=${OPTARG};;
  c) category_id=${OPTARG};;
  i) tag_id=${OPTARG};;
  n) name=${OPTARG// /+};;
  d) description=${OPTARG};;
  m) main_photo=${OPTARG};;
  b) main_photo_image_type=${OPTARG};;
 esac
done

source utility/load_config.sh

if [ ! -z ${main_photo+x} ];

then base64 $main_photo > "image.dat"
curl "$host:$port/drafts?token=$token&draft_id=$draft_id&category_id=$category_id&tag_id=$tag_id&name=$name&description=$description&image_type=$main_photo_image_type" -X PUT -F main_photo=@image.dat
rm "image.dat"; 

else curl "$host:$port/drafts?token=$token&draft_id=$draft_id&category_id=$category_id&tag_id=$tag_id&name=$name&description=$description" -X PUT;
fi

