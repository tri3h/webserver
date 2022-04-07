{-# LANGUAGE DeriveGeneric #-}
module Types.Post where

import Data.Text ( Text )
import Data.Aeson
    ( defaultOptions,
      genericToEncoding,
      SumEncoding(UntaggedValue),
      Options(sumEncoding),
      ToJSON(toEncoding) )
import GHC.Generics ( Generic )
import Types.Author ( Author ) 
import Types.Category ( Category ) 
import Types.User ( User ) 
import Types.Tag ( Tag )
import Types.Comment ( Comment )
import Types.Image ( Image )

postsOnPage :: Integer
postsOnPage = 10

type PostId = Integer
type Date = Text

data Post = ShortPost {
    postId :: Integer,
    authorId :: Integer,
    categoryId :: Integer,
    name :: Text,
    date :: Text,
    text :: Text,
    mainPhoto :: Image
} | FullPost {
    postId :: Integer,
    author :: Maybe Author,
    user :: Maybe User,
    category :: [Category],
    tag :: [Tag],
    comment :: [Comment],
    name :: Text,
    date :: Text,
    text :: Text,
    mainPhoto :: Image,
    minorPhoto :: [Image]
} deriving (Show, Eq, Generic)

instance ToJSON Post where
    toEncoding = genericToEncoding defaultOptions {sumEncoding = UntaggedValue}
