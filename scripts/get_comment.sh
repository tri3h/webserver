#! /bin/sh

#Required parameters: i. Others are optional
while getopts i:l:o: flag
do
 case "$flag" in
  i) id=${OPTARG};;
  l) limit=${OPTARG};;
  o) offset=${OPTARG};;
 esac
done

source utility/load_config.sh

curl "$host:$port/comments?post_id=$id&limit=$limit&offset=$offset" -X GET
