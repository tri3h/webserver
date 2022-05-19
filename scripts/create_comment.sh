#! /bin/sh

token=$(<utility/token.txt)
post_id="1"
user_id="1"
text="text"

#All parameters are required
while getopts t:p:x: flag
do
 case "$flag" in
  t) token=${OPTARG};;
  p) post_id=${OPTARG};;
  x) text=${OPTARG// /+};;
 esac
done

source utility/load_config.sh

curl "$host:$port/comments?token=$token&post_id=$post_id&text=$text" -X POST
