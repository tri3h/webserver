#! /bin/sh

while getopts n:s:a:l:p:y: flag
do
 case "$flag" in
  n) name=${OPTARG// /+};;
  s) surname=${OPTARG// /+};;
  a) avatar=${OPTARG};;
  y) avatar_image_type=${OPTARG};;
  l) login=${OPTARG};;
  p) password=${OPTARG};;
 esac
done

avatar_base64=( base64 $avatar )

while read name n value
do 
case "$name" in 
 host) host=${value//\"};;
 port) port=$value;;
esac
done < ../Server.config

curl "$host:$port/users?name=$name&surname=$surname&login=$login&password=$password" -X POST -d "{\"avatar\" : \"$avatar_base64\", \"image_type\" : \"$avatar_image_type\"}"
