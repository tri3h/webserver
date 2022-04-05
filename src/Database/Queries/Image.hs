{-# LANGUAGE OverloadedStrings #-}
module Database.Queries.Image where

import Database.PostgreSQL.Simple ( Connection, query, Only(Only) )
import Database.PostgreSQL.Simple.Time (Date)
import Types.Image ( Image(Image) )

get :: Integer -> Connection -> IO Image
get imageId conn = do
    [(image, imageType)] <- query conn "SELECT translate(encode(image,'base64'), \
        \E'\n', '') AS image, image_type FROM \
        \images WHERE images.image_id = ?" (Only imageId)
    return (Image image imageType)