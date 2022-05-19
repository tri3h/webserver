#! /bin/sh

token=$(<utility/token.txt)
draft_id="1"
minor_photo="utility/image.png"

#All parameters are required
while getopts t:d:p: flag
do
 case "$flag" in
  t) token=${OPTARG};;
  d) draft_id=${OPTARG};;
  p) minor_photo=${OPTARG};;
 esac
done

source utility/load_config.sh

curl "$host:$port/drafts/minor_photo?token=$token&draft_id=$draft_id" -X POST -F "minor_photo=@$minor_photo"

