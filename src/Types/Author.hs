{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Types.Author where

import Data.Aeson
  ( KeyValue ((.=)),
    ToJSON (toJSON),
    object,
  )
import Data.Text (Text)
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.ToField (ToField)
import Types.User (UserId)

newtype AuthorId = AuthorId {getAuthorId :: Integer} deriving (Show, Eq, ToField, FromField, ToJSON)

newtype Description = Description {getDescirption :: Text} deriving (Show, Eq, ToField, FromField, ToJSON)

data GetAuthor = GetAuthor
  { gAuthorId :: AuthorId,
    gUserId :: Maybe UserId,
    gDescription :: Description
  }
  deriving (Show, Eq)

data EditAuthor = EditAuthor
  { eAuthorId :: AuthorId,
    eDescription :: Description
  }
  deriving (Show, Eq)

data CreateAuthor = CreateAuthor
  { cUserId :: UserId,
    cDescription :: Description
  }
  deriving (Show, Eq)

instance ToJSON GetAuthor where
  toJSON author =
    object
      [ "author_id" .= gAuthorId author,
        "user_id" .= gUserId author,
        "description" .= gDescription author
      ]

authorNotExist :: Text
authorNotExist = "Author with such id doesn't exist"

alreadyAuthor :: Text
alreadyAuthor = "This user is already author"
