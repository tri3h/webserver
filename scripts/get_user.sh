#! /bin/sh

while getopts t: flag
do
 case "$flag" in
  t) token=${OPTARG};;
 esac
done

while read name n value
do 
case "$name" in 
 host) host=${value//\"};;
 port) port=$value;;
esac
done < ../Server.config

curl "$host:$port/users?token=$token" -X GET