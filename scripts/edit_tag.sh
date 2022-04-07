#! /bin/sh

while getopts t:n:i: flag
do
 case "$flag" in
  t) token=${OPTARG};;
  n) name=${OPTARG// /+};;
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

curl "$host:$port/tags?token=$token&name=$name&tag_id=$id" -X PUT
