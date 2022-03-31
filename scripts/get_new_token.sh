#! /bin/sh

while getopts l:p: flag
do
 case "$flag" in
  l) login=${OPTARG};;
  p) password=${OPTARG};;
 esac
done

while read name n value
do 
case "$name" in 
 host) host=${value//\"};;
 port) port=$value;;
esac
done < ../Server.config

curl "$host:$port/tokens?login=$login&password=$password" -X GET
