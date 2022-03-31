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

main_photo_base64=( base64 $main_photo )

while read name n value
do 
case "$name" in 
 host) host=${value//\"};;
 port) port=$value;;
esac
done < ../Server.config

curl "$host:$port/drafts?token=$token&draft_id=$draft_id&category_id=$category_id&tag_id=$tag_id&name=$name&description=$description" -X PUT -d "{\"main_photo\" : \"$main_photo_base64\", \"description\": \"$description\"}"
