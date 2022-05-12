#! /bin/sh

#All parameters are required
while getopts t:a: flag
do
 case "$flag" in
  t) token=${OPTARG};;
  a) avatar=${OPTARG};;
 esac
done

source utility/load_config.sh

curl "$host:$port/users/avatar?token=$token" -X POST -F "avatar=@$avatar"

