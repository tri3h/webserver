#! /bin/sh

name="ivan"
surname="invanov"
login="login"
password="password"
avatar="utility/image.png"
avatar_image_type="png"

#All parameters are required
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

source utility/load_config.sh

base64 $avatar > "image.dat"

curl "$host:$port/users?name=$name&surname=$surname&login=$login&password=$password&image_type=$avatar_image_type" -X POST -F avatar=@image.dat | sed 's/"//g' > utility/token.txt

rm "image.dat"
