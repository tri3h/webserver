#! /bin/sh

while getopts t:c:n:p: flag
do
 case "$flag" in
  t) token=${OPTARG};;
  c) category_id=${OPTARG};;
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

curl "$host:$port/categories?token=$token&category_id=$category_id&name=$name&parent_id=$parent_id" -X PUT
