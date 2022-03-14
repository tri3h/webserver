{-# LANGUAGE DeriveGeneric #-}
module Types.Post where

import Data.Text
import Data.Aeson
import GHC.Generics
import Types.Author 
import Types.Category 
import Types.User 
import Types.Tag

data Post = PartialPost {
    postId :: Integer,
    authorId :: Integer,
    categoryId :: Integer,
    name :: Text,
    date :: Text,
    text :: Text,
    mainPhoto :: Text
} | FullPost {
    author :: Author,
    user :: User,
    category :: [Category],
    tag :: [Tag],
    --commentary :: [Commentary]
    postId :: Integer,
    name :: Text,
    date :: Text,
    text :: Text,
    mainPhoto :: Text,
    minorPhoto :: [Text]
} deriving (Show, Generic)

instance ToJSON Post where
    toEncoding = genericToEncoding defaultOptions {sumEncoding = UntaggedValue}

type PostId = Integer