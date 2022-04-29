{-# LANGUAGE OverloadedStrings #-}

module Database.Queries.Image where

import Data.Text (Text)
import Database.PostgreSQL.Simple (Connection, Only (Only), query)
import Types.Image (Image (Image), imageNotExist)

get :: Integer -> Connection -> IO Image
get imageId conn = do
  [(image, imageType)] <-
    query
      conn
      "SELECT image, image_type FROM \
      \images WHERE images.image_id = ?"
      (Only imageId)
  return (Image image imageType)

doesExist :: Integer -> Connection -> IO (Either Text ())
doesExist imageId conn = do
  [Only n] <- query conn "SELECT COUNT(image_id) FROM images WHERE image_id = ?" (Only imageId)
  if (n :: Integer) == 1
    then return $ Right ()
    else return $ Left imageNotExist
