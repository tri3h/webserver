#! /bin/sh

#All parameters are required
while getopts i: flag
do
 case "$flag" in
  i) id=${OPTARG};;
 esac
done

source utility/load_config.sh

curl "$host:$port/tags?tag_id=$id" -X GET
