{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Types.Image where

import Data.Aeson
  ( KeyValue ((.=)),
    ToJSON (toJSON),
    object,
  )
import Data.ByteString (ByteString)
import Data.Text (Text)
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.ToField (ToField (..))

type ImageType = Text

type ImageBody = ByteString

newtype Link = Link Text deriving (Show, Eq, Read)

newtype ImageId = ImageId Integer deriving (Show, Eq, Read, ToField, FromField)

data Image = Image ImageBody ImageType deriving (Show, Eq, Read)

instance ToJSON Link where
  toJSON (Link x) =
    object
      ["link" .= x]

imageAddress :: Text
imageAddress = "/images?image_id="
