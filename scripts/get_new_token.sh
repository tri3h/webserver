#! /bin/sh

while getopts l:p: flag
do
 case "$flag" in
  l) login=${OPTARG};;
  p) password=${OPTARG};;
 esac
done

while read name_ n value
do 
case "$name_" in 
 host) host=${value//\"};;
 port) port=$value;;
esac
done < ../Configs/Server.config

curl "$host:$port/tokens?login=$login&password=$password" -X GET
