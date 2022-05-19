#! /bin/sh

token=$(<utility/token.txt)
id="1"
description="description"

#All parameters are required
while getopts t:i:d: flag
do
 case "$flag" in
  t) token=${OPTARG};;
  i) id=${OPTARG};;
  d) description=${OPTARG// /+};;
 esac
done

source utility/load_config.sh

curl "$host:$port/authors?token=$token&author_id=$id&description=$description" -X PUT
