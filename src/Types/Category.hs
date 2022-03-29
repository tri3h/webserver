{-# LANGUAGE DeriveGeneric #-}
module Types.Category where

import Data.Text
import Data.Aeson
import GHC.Generics

data Category = CategoryToGet {
    categoryId :: Integer,
    name :: Text,
    parentId :: Maybe Integer
} | CategoryToCreate {
    name :: Text,
    parentId :: Maybe Integer
} deriving (Show, Generic)

instance ToJSON Category where
    toEncoding = genericToEncoding defaultOptions {sumEncoding = UntaggedValue}

type CategoryId = Integer
type Name = Text