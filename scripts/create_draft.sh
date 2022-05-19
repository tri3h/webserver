#! /bin/sh

token=$(<utility/token.txt)
category_id="1"
name="draft"
description="text"

#Required parameters: t, c, n, d. Others are optional
while getopts t:c:i:n:d:m:y: flag
do
 case "$flag" in
  t) token=${OPTARG};;
  c) category_id=${OPTARG};;
  i) tag_id=${OPTARG};;
  n) name=${OPTARG// /+};;
  d) description=${OPTARG};;
  m) main_photo=${OPTARG};;
 esac
done

source utility/load_config.sh

curl "$host:$port/drafts?token=$token&category_id=$category_id&description=$description&tag_id=$tag_id&name=$name" -X POST -F "main_photo=@$main_photo"
