#! /bin/sh

#All parameters are required
while getopts t:n: flag
do
 case "$flag" in
  t) token=${OPTARG};;
  n) name=${OPTARG// /+};;
 esac
done

source utility/load_config.sh

curl "$host:$port/tags?token=$token&name=$name" -X POST
