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

while read key n value
do 
case "$key" in 
 host) host=${value//\"};;
 port) port=$value;;
esac
done < ../Server.config

base64 $avatar > "image.dat"

curl "$host:$port/users?name=$name&surname=$surname&login=$login&password=$password&image_type=$avatar_image_type" -X POST -F avatar=@image.dat

rm "image.dat"
