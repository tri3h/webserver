#! /bin/sh

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
