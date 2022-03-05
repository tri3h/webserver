{-# LANGUAGE DeriveGeneric #-}
module Types.Tag where

import Data.Text
import Data.Aeson
import GHC.Generics

data Tag = EditTag {
    tagId :: Integer,
    name :: Text
} | CreateTag {
    name :: Text
} deriving (Show, Generic)

instance ToJSON Tag where
    toEncoding = genericToEncoding defaultOptions {sumEncoding = UntaggedValue}

type TagId = Integer