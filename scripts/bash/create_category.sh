#! /bin/sh

#Required parameters: t, n. Others are optional.
while getopts t:n:p: flag;
do
 case "$flag" in
  t) token=${OPTARG};;
  p) parent_id=${OPTARG};;
  n) name=${OPTARG// /+};;
 esac
done

source utility/load_config.sh

curl "$host:$port/categories?token=$token&name=$name&parent_id=$parent_id" -X POST
