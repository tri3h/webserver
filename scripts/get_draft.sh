#! /bin/sh

while getopts t:d: flag
do
 case "$flag" in
  t) token=${OPTARG};;
  d) draft_id=${OPTARG};;
 esac
done

while read name_ n value
do 
case "$name_" in 
 host) host=${value//\"};;
 port) port=$value;;
esac
done < ../Server.config

curl "$host:$port/drafts?token=$token&draft_id=$draft_id" -X GET
