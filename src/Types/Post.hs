{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Types.Post where

import Data.Aeson
  ( KeyValue ((.=)),
    ToJSON (toJSON),
    object,
  )
import Data.Text (Text)
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.ToField (ToField)
import Types.Author (AuthorId, GetAuthor)
import Types.Category (CategoryId, GetCategory)
import Types.Comment (GetComment)
import Types.Image (Link)
import Types.PostComment (PostId)
import Types.Tag (Tag)
import Types.User (GetUser)

newtype Name = Name {getName :: Text} deriving (Show, Eq, ToField, FromField, ToJSON)

newtype Date = Date {getDate :: Text} deriving (Show, Eq, ToField, FromField, ToJSON)

data ShortPost = ShortPost
  { sPostId :: PostId,
    sAuthorId :: Maybe AuthorId,
    sCategoryId :: Maybe CategoryId,
    sName :: Name,
    sDate :: Date,
    sText :: Text,
    sMainPhoto :: Maybe Link
  }
  deriving (Show, Eq)

data FullPost = FullPost
  { fPostId :: PostId,
    fAuthor :: Maybe GetAuthor,
    fUser :: Maybe GetUser,
    fCategory :: [GetCategory],
    fTag :: [Tag],
    fComment :: [GetComment],
    fName :: Name,
    fDate :: Date,
    fText :: Text,
    fMainPhoto :: Maybe Link,
    fMinorPhoto :: [Link]
  }
  deriving (Show, Eq)

instance ToJSON FullPost where
  toJSON post =
    object
      [ "post_id" .= fPostId post,
        "author" .= fAuthor post,
        "user" .= fUser post,
        "category" .= fCategory post,
        "tag" .= fTag post,
        "comment" .= fComment post,
        "name" .= fName post,
        "date" .= fDate post,
        "text" .= fText post,
        "main_photo" .= fMainPhoto post,
        "minor_photo" .= fMinorPhoto post
      ]
