#! /bin/sh

#All parameters are required
while getopts t:p:u:x: flag
do
 case "$flag" in
  t) token=${OPTARG};;
  p) post_id=${OPTARG};;
  u) user_id=${OPTARG};;
  x) text=${OPTARG// /+};;
 esac
done

source utility/load_config.sh

curl "$host:$port/comments?token=$token&post_id=$post_id&user_id=$user_id&text=$text" -X POST
