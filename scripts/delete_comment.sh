#! /bin/sh

#All parameters are required
while getopts t:c: flag
do
 case "$flag" in
  t) token=${OPTARG};;
  c) comment_id=${OPTARG};;
 esac
done

source utility/load_config.sh

curl "$host:$port/comments?token=$token&comment_id=$comment_id" -X DELETE
