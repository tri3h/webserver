#! /bin/sh

#All parameters are required
while getopts t:d:i: flag
do
 case "$flag" in
  t) token=${OPTARG};;
  d) draft_id=${OPTARG};;
  i) minor_photo_id=${OPTARG};;
 esac
done

source utility/load_config.sh

curl "$host:$port/drafts/minor_photo?token=$token&draft_id=$draft_id&minor_photo_id=$minor_photo_id" -X DELETE
