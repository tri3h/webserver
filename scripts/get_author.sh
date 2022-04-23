#! /bin/sh

#All parameters are required
while getopts t:i: flag
do
 case "$flag" in
  t) token=${OPTARG};;
  i) id=${OPTARG};;
 esac
done

source utility/load_config.sh

curl "$host:$port/authors?token=$token&author_id=$id" -X GET
