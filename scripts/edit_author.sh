#! /bin/sh

while getopts t:i:d: flag
do
 case "$flag" in
  t) token=${OPTARG};;
  i) id=${OPTARG};;
  d) description=${OPTARG// /+};;
 esac
done

while read name_ n value
do 
case "$name_" in 
 host) host=${value//\"};;
 port) port=$value;;
esac
done < ../Configs/Server.config

curl "$host:$port/authors?token=$token&author_id=$id&description=$description" -X PUT
