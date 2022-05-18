{-# LANGUAGE OverloadedStrings #-}

module Database.Queries.Image where

import Database.PostgreSQL.Simple (Connection, Only (Only), query)
import Error (Error, imageNotExist)
import Types.Image (Image (Image))

get :: Integer -> Connection -> IO (Either Error Image)
get imageId conn = do
  result <-
    query
      conn
      "SELECT image, image_type FROM \
      \images WHERE images.image_id = ?"
      (Only imageId)
  return $
    if null result
      then Left imageNotExist
      else Right $ (\[(image, imageType)] -> Image image imageType) result
