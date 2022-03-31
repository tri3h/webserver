#! /bin/sh

while getopts t:i:d: flag
do
 case "$flag" in
  t) token=${OPTARG};;
  i) id=${OPTARG};;
  d) description=${OPTARG// /+};;
 esac
done

while read name n value
do 
case "$name" in 
 host) host=${value//\"};;
 port) port=$value;;
esac
done < ../Server.config

curl "$host:$port/authors?token=$token&user_id=$id&description=$description" -X POST
