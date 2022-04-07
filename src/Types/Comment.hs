{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Types.Comment where

import Data.Text ( Text )
import Data.Aeson
    ( defaultOptions,
      genericToEncoding,
      SumEncoding(UntaggedValue),
      Options(sumEncoding),
      ToJSON(toEncoding) )
import GHC.Generics ( Generic )

type CommentId = Integer

data Comment = CommentToCreate {
    postId :: Integer,
    userId :: Integer,
    text :: Text
} | CommentToGet {
    commentId :: Integer,
    userId :: Integer,
    text :: Text
} deriving (Show, Eq, Generic)

instance ToJSON Comment where
    toEncoding = genericToEncoding defaultOptions {sumEncoding = UntaggedValue}