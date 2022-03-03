{-# LANGUAGE DeriveGeneric #-}
module Types.Author where

import Data.Text
import Data.Aeson
import GHC.Generics

data Author = GetAuthor {
    authorId :: Integer,
    userId :: Integer,
    description :: Text
} | EditAuthor {
    authorId :: Integer,
    description :: Text
} | CreateAuthor {
    userId :: Integer,
    description :: Text
} deriving (Show, Generic)

type AuthorId = Integer

instance ToJSON Author where
    toEncoding = genericToEncoding defaultOptions {sumEncoding = UntaggedValue}

instance FromJSON Author where