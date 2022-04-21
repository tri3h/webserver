#! /bin/sh

while getopts t:n:p: flag;
do
 case "$flag" in
  t) token=${OPTARG};;
  p) parent_id=${OPTARG};;
  n) name=${OPTARG// /+};;
 esac
done

while read name_ n value
do 
case "$name_" in 
 host) host=${value//\"};;
 port) port=$value;;
esac
done < ../Configs/Server.config

curl "$host:$port/categories?token=$token&name=$name&parent_id=$parent_id" -X POST
