{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Types.Tag where

import Data.Aeson
  ( Options (sumEncoding),
    SumEncoding (UntaggedValue),
    ToJSON (toEncoding),
    defaultOptions,
    genericToEncoding,
  )
import Data.Text (Text)
import GHC.Generics (Generic)

type TagId = Integer

type Name = Text

data Tag = Tag
  { tagId :: Integer,
    name :: Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON Tag where
  toEncoding = genericToEncoding defaultOptions {sumEncoding = UntaggedValue}

tagNotExist :: Text
tagNotExist = "Tag with such id doesn't exist"
