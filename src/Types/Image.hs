{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Types.Image where

import Data.Aeson
  ( Options (sumEncoding),
    SumEncoding (UntaggedValue),
    ToJSON (toEncoding),
    defaultOptions,
    genericToEncoding,
  )
import Data.Text (Text)
import Database.PostgreSQL.Simple.ToField (ToField (..))
import GHC.Generics (Generic)

type ImageId = Integer

type ImageType = Text

data Image = Link Text | Image Text ImageType
  deriving (Show, Eq, Read, Generic)

instance ToJSON Image where
  toEncoding = genericToEncoding defaultOptions {sumEncoding = UntaggedValue}

instance ToField Image where
  toField (Image image _) = toField image
  toField (Link x) = toField x

malformedImage :: Text
malformedImage = "Malformed image"

imageNotExist :: Text
imageNotExist = "Image with such id doesn't exist"
