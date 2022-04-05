{-# LANGUAGE DeriveGeneric #-}
module Types.Category where

import Data.Text ( Text )
import Data.Aeson
    ( defaultOptions,
      genericToEncoding,
      SumEncoding(UntaggedValue),
      Options(sumEncoding),
      ToJSON(toEncoding) )
import GHC.Generics ( Generic )

type CategoryId = Integer
type Name = Text

data Category = Category {
    categoryId :: Integer,
    name :: Text,
    parentId :: Maybe Integer
} | CategoryToCreate {
    name :: Text,
    parentId :: Maybe Integer
} deriving (Show, Generic)

instance ToJSON Category where
    toEncoding = genericToEncoding defaultOptions {sumEncoding = UntaggedValue}