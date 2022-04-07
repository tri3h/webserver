#! /bin/sh

while getopts t:n: flag
do
 case "$flag" in
  t) token=${OPTARG};;
  n) name=${OPTARG// /+};;
 esac
done

while read name_ n value
do 
case "$name_" in 
 host) host=${value//\"};;
 port) port=$value;;
esac
done < ../Server.config

curl "$host:$port/tags?token=$token&name=$name" -X POST
