{-# LANGUAGE DeriveGeneric #-}
module Types.Category where

import Data.Text
import Data.Aeson
import GHC.Generics

data Category = GetCategory {
    categoryId :: Integer,
    name :: Text,
    parentId :: Maybe Integer
} | CreateCategory {
    name :: Text,
    parentId :: Maybe Integer
} deriving (Show, Generic)

instance ToJSON Category where
    toEncoding = genericToEncoding defaultOptions {sumEncoding = UntaggedValue}

type CategoryId = Integer
type Name = Text