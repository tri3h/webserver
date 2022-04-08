{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Types.Comment where

import Data.Aeson
  ( Options (sumEncoding),
    SumEncoding (UntaggedValue),
    ToJSON (toEncoding),
    defaultOptions,
    genericToEncoding,
  )
import Data.Text (Text)
import GHC.Generics (Generic)

type CommentId = Integer

data Comment
  = CommentToCreate
      { postId :: Integer,
        userId :: Integer,
        text :: Text
      }
  | CommentToGet
      { commentId :: Integer,
        userId :: Integer,
        text :: Text
      }
  deriving (Show, Eq, Generic)

instance ToJSON Comment where
  toEncoding = genericToEncoding defaultOptions {sumEncoding = UntaggedValue}

commentNotExist :: Text
commentNotExist = "Comment with such id doesn't exist"

malformedComment :: Text
malformedComment = "Malformed comments"
