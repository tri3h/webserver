#! /bin/sh

#Required parameters: t. Others are optional
while getopts t:l:o: flag
do
 case "$flag" in
  t) token=${OPTARG};;
  l) limit=${OPTARG};;
  o) offset=${OPTARG};;
 esac
done

source utility/load_config.sh

curl "$host:$port/drafts/id?token=$token&limit=$limit&offset=$offset" -X GET
