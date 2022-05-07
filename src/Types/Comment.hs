{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Types.Comment where

import Data.Aeson
  ( KeyValue ((.=)),
    ToJSON (toJSON),
    object,
  )
import Data.Text (Text)
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.ToField (ToField)
import Types.PostComment (PostId)
import Types.User (UserId)

newtype CommentId = CommentId {getCommentId :: Integer} deriving (Show, Eq, ToField, FromField, ToJSON)

data CreateComment = CreateComment
  { cPostId :: PostId,
    cUserId :: UserId,
    cText :: Text
  }
  deriving (Show, Eq)

data GetComment = GetComment
  { gCommentId :: CommentId,
    gUserId :: Maybe UserId,
    gText :: Text
  }
  deriving (Show, Eq)

instance ToJSON GetComment where
  toJSON comment =
    object
      [ "comment_id" .= gCommentId comment,
        "user_id" .= gUserId comment,
        "text" .= gText comment
      ]
