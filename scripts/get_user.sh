#! /bin/sh

#All parameters are required
while getopts t: flag
do
 case "$flag" in
  t) token=${OPTARG};;
 esac
done

source utility/load_config.sh

curl "$host:$port/users?token=$token" -X GET
