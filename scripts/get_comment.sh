#! /bin/sh

while getopts t:i: flag
do
 case "$flag" in
  t) token=${OPTARG};;
  i) id=${OPTARG};;
 esac
done

while read name_ n value
do 
case "$name_" in 
 host) host=${value//\"};;
 port) port=$value;;
esac
done < ../Server.config

curl "$host:$port/comments?token=$token&post_id=$id" -X GET
