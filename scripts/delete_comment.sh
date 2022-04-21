#! /bin/sh

while getopts t:c: flag
do
 case "$flag" in
  t) token=${OPTARG};;
  c) comment_id=${OPTARG};;
 esac
done

while read name_ n value
do 
case "$name_" in 
 host) host=${value//\"};;
 port) port=$value;;
esac
done < ../Configs/Server.config

curl "$host:$port/comments?token=$token&comment_id=$comment_id" -X DELETE
