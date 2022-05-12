#! /bin/sh

#Parameters are optional
while getopts i: flag
do
 case "$flag" in
  i) id=${OPTARG};;
 esac
done

source utility/load_config.sh

curl "$host:$port/categories?category_id=$id" -X GET
