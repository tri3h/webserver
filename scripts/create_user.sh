#! /bin/sh

name="ivan"
surname="invanov"
login="login"
password="password"

#Required parameters: n, s, l, p. Others are optional.
while getopts n:s:a:l:p:y: flag
do
 case "$flag" in
  n) name=${OPTARG// /+};;
  s) surname=${OPTARG// /+};;
  a) avatar=${OPTARG};;
  l) login=${OPTARG};;
  p) password=${OPTARG};;
 esac
done

source utility/load_config.sh

curl "$host:$port/users?name=$name&surname=$surname&login=$login&password=$password" -X POST -F "avatar=@$avatar"

