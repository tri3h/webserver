{-# LANGUAGE DeriveGeneric #-}
module Types.Comment where

import Data.Text
import Data.Aeson
import GHC.Generics

data Comment = CommentToCreate {
    postId :: Integer,
    userId :: Integer,
    text :: Text
} | CommentToGet {
    commentId :: Integer,
    userId :: Integer,
    text :: Text
} deriving (Show, Generic)

type CommentId = Integer

instance ToJSON Comment where
    toEncoding = genericToEncoding defaultOptions {sumEncoding = UntaggedValue}