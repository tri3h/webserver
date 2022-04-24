{-# LANGUAGE OverloadedStrings #-}

module Types.Image where

import Data.Aeson
  ( KeyValue ((.=)),
    ToJSON (toJSON),
    object,
  )
import Data.Text (Text)
import Database.PostgreSQL.Simple.ToField (ToField (..))

type ImageId = Integer

type ImageType = Text

data Image = Id ImageId | Link Text | Image Text ImageType
  deriving (Show, Eq, Read)

instance ToJSON Image where
  toJSON (Id x) =
    object
      ["image_id" .= x]
  toJSON (Link x) = object ["image_link" .= x]
  toJSON (Image x _) = object ["image" .= x]

instance ToField Image where
  toField (Image image _) = toField image
  toField (Link x) = toField x
  toField (Id x) = toField x

malformedImage :: Text
malformedImage = "Malformed image"

imageNotExist :: Text
imageNotExist = "Image with such id doesn't exist"

imageAddress :: Text
imageAddress = "/images?image_id="
