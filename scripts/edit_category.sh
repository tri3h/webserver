#! /bin/sh

while getopts t:c:n:p flag
do
 case "$flag" in
  t) token=${OPTARG};;
  c) category_id=${OPTARG};;
  p) parent_d=${OPTARG};;
  n) name=${OPTARG// /+};;
 esac
done

while read name n value
do 
case "$name" in 
 host) host=${value//\"};;
 port) port=$value;;
esac
done < ../Server.config

curl "$host:$port/categories?token=$token&category_id=$category_id&name=$name&parent_id=$parent_id" -X PUT
