{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Types.Tag where

import Data.Aeson
  ( KeyValue ((.=)),
    ToJSON (toJSON),
    object,
  )
import Data.Text (Text)
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.ToField (ToField)

newtype TagId = TagId {getTagId :: Integer} deriving (Show, Eq, ToField, FromField, ToJSON)

newtype Name = Name {getName :: Text} deriving (Show, Eq, ToField, FromField, ToJSON)

data Tag = Tag
  { tagId :: TagId,
    name :: Name
  }
  deriving (Show, Eq)

instance ToJSON Tag where
  toJSON tag =
    object
      [ "tag_id" .= tagId tag,
        "name" .= name tag
      ]
