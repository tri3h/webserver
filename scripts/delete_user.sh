#! /bin/sh

while getopts t:i: flag
do
 case "$flag" in
  t) token=${OPTARG};;
  i) id=${OPTARG};;
 esac
done

while read name n value
do 
case "$name" in 
 host) host=${value//\"};;
 port) port=$value;;
esac
done < ../Server.config

curl "$host:$port/users?token=$token&user_id=$id" -X DELETE
