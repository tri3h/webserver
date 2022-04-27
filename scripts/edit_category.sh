#! /bin/sh

token=$(<utility/token.txt)
category_id="1"

#Required parameters: t, c. Others are optional.
while getopts t:c:n:p: flag
do
 case "$flag" in
  t) token=${OPTARG};;
  c) category_id=${OPTARG};;
  p) parent_id=${OPTARG};;
  n) name=${OPTARG// /+};;
 esac
done

source utility/load_config.sh

curl "$host:$port/categories?token=$token&category_id=$category_id&name=$name&parent_id=$parent_id" -X PUT
