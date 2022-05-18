#! /bin/sh

#Required parameters: t, i. Others are optional.
while getopts t:n:i: flag
do
 case "$flag" in
  t) token=${OPTARG};;
  n) name=${OPTARG// /+};;
  i) id=${OPTARG};;
 esac
done

source utility/load_config.sh

curl "$host:$port/tags?token=$token&name=$name&tag_id=$id" -X PUT
