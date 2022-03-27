{-# LANGUAGE OverloadedStrings #-}
module Database.Queries.Image where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Time (Date)
import Types.Image

get :: Integer -> Connection -> IO Image
get imageId conn = do
    [Only image] <- query conn "SELECT encode(image,'base64') AS image FROM \
    \images WHERE images.image_id = ?" (Only imageId)
    return $ Image image