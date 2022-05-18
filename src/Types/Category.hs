{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Types.Category where

import Data.Aeson
  ( KeyValue ((.=)),
    ToJSON (toJSON),
    object,
  )
import Data.Text (Text)
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.ToField (ToField)
import Types.Limit (Limit (Limit))

newtype CategoryId = CategoryId {getCategoryId :: Integer} deriving (Show, Eq, ToField, FromField, ToJSON)

newtype Name = Name {getName :: Text} deriving (Show, Eq, ToField, FromField, ToJSON)

type ParentId = CategoryId

data CreateCategory = CreateCategory
  { cName :: Name,
    cParentId :: Maybe CategoryId
  }
  deriving (Show, Eq)

data GetCategory = GetCategory
  { gCategoryId :: CategoryId,
    gName :: Name,
    gParentId :: Maybe ParentId
  }
  deriving (Show, Eq)

instance ToJSON GetCategory where
  toJSON categ =
    object
      [ "category_id" .= gCategoryId categ,
        "name" .= gName categ,
        "parent_id" .= gParentId categ
      ]

categoriesOnPage :: Limit
categoriesOnPage = Limit 10
