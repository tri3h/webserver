#! /bin/sh

token=$(<utility/token.txt)
draft_id="1"

#All parameters are required
while getopts t:d: flag
do
 case "$flag" in
  t) token=${OPTARG};;
  d) draft_id=${OPTARG};;
 esac
done

source utility/load_config.sh

curl "$host:$port/publish?token=$token&draft_id=$draft_id"
