#! /bin/sh

token=$(<utility/token.txt)
id="1"

#All parameters are required
while getopts t:i: flag
do
 case "$flag" in
  t) token=${OPTARG};;
  i) id=${OPTARG};;
 esac
done

source utility/load_config.sh

curl "$host:$port/users?token=$token&user_id=$id" -X DELETE
