#! /bin/sh

login="login"
password="password"

#All parameters are required
while getopts l:p: flag
do
 case "$flag" in
  l) login=${OPTARG};;
  p) password=${OPTARG};;
 esac
done

source utility/load_config.sh

curl "$host:$port/tokens?login=$login&password=$password" -X GET
