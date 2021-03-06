#! /bin/sh

#Parameters are optional
while getopts a:c:g:i:n:p:e:s:z:y:x:b:o:l:h: flag
do
 case "$flag" in
  a) author_name=${OPTARG// /+};;
  c) category_id=${OPTARG};;
  g) tag=${OPTARG// /+};;
  i) tag_id=${OPTARG};;
  n) tag_in=${OPTARG};;
  h) tag_all=${OPTARG};;
  p) post_name=${OPTARG// /+};;
  e) text=${OPTARG// /+};;
  s) substring=${OPTARG// /+};;
  z) date_after=${OPTARG};;
  y) date_at=${OPTARG};;
  x) date_before=${OPTARG};;
  b) sort_by=${OPTARG};;
  l) limit=${OPTARG};;
  o) offset=${OPTARG};;
 esac
done

source utility/load_config.sh

curl "$host:$port/posts?author_name=$author_name&category_id=$category_id&tag=$tag&tag_id=$tag_id&tag_in=$tag_in&tag_all=$tag_all&post_name=$post_name&text=$text&substring=$substring&date_after=$date_after&date_at=$date_at&date_before=$date_before&sort_by=$sort_by&offset=$offset&limit=$limit" -X GET
