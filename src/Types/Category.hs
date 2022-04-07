{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
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
type ParentId = Integer

data Category = Category {
    categoryId :: Integer,
    name :: Text,
    parentId :: Maybe Integer
} | CategoryToCreate {
    name :: Text,
    parentId :: Maybe Integer
} deriving (Show, Eq, Generic)

instance ToJSON Category where
    toEncoding = genericToEncoding defaultOptions {sumEncoding = UntaggedValue}

categoryNotExist :: Text
categoryNotExist = "Category with such id doesn't exist"

invalidParent :: Text
invalidParent = "Invalid parent id"

malformedCategory :: Text 
malformedCategory = "Malformed category"

noDeleteHasChildren :: Text 
noDeleteHasChildren = "Impossible to delete: the category has children"