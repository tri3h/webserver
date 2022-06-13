{-# LANGUAGE OverloadedStrings #-}

module Data where

import Data.ByteString (ByteString)
import Network.HTTP.Types (QueryItem)

token :: QueryItem
token = ("token", Just "token")

name :: QueryItem
name = ("name", Just "name")

description :: QueryItem
description = ("description", Just "description")

userId :: QueryItem
userId = ("user_id", Just "1")

authorId :: QueryItem
authorId = ("author_id", Just "1")

parentId :: QueryItem
parentId = ("parent_id", Just "1")

categoryId :: QueryItem
categoryId = ("category_id", Just "1")

postId :: QueryItem
postId = ("post_id", Just "1")

text :: QueryItem
text = ("text", Just "text")

commentId :: QueryItem
commentId = ("comment_id", Just "1")

draftId :: QueryItem
draftId = ("draft_id", Just "1")

tagId :: QueryItem
tagId = ("tag_id", Just "1")

minorPhotoId :: QueryItem
minorPhotoId = ("minor_photo_id", Just "1")

mainPhoto :: ByteString
mainPhoto = "Content-Type: image/png \r name=\"main_photo\" 12345 \r\n-"

minorPhoto :: ByteString
minorPhoto = "Content-Type: image/png \r name=\"minor_photo\" 12345 \r\n-"

imageId :: QueryItem
imageId = ("image_id", Just "1")

tag :: QueryItem
tag = ("tag", Just "tag")

tagIn :: QueryItem
tagIn = ("tag_in", Just "1,2")

tagAll :: QueryItem
tagAll = ("tag_all", Just "1,2")

postName :: QueryItem
postName = ("post_name", Just "name")

substring :: QueryItem
substring = ("substring", Just "substring")

dateAfter :: QueryItem
dateAfter = ("date_after", Just "01-01-2000")

dateAt :: QueryItem
dateAt = ("date_at", Just "01-01-2000")

dateBefore :: QueryItem
dateBefore = ("date_before", Just "01-01-2000")

sortBy :: QueryItem
sortBy = ("sort_by", Just "by_author")

surname :: QueryItem
surname = ("surname", Just "surname")

login :: QueryItem
login = ("login", Just "login")

password :: QueryItem
password = ("password", Just "password")

avatar :: ByteString
avatar = "Content-Type: image/png \r name=\"avatar\" 12345 \r\n-"
