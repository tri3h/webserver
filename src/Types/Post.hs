{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Types.Post where

import Data.Aeson
  ( Options (sumEncoding),
    SumEncoding (UntaggedValue),
    ToJSON (toEncoding),
    defaultOptions,
    genericToEncoding,
  )
import Data.Text (Text)
import GHC.Generics (Generic)
import Types.Author (Author)
import Types.Category (Category)
import Types.Comment (Comment)
import Types.Image (Image)
import Types.Tag (Tag)
import Types.User (User)

postsOnPage :: Integer
postsOnPage = 10

type PostId = Integer

type Date = Text

data Post
  = ShortPost
      { postId :: Integer,
        authorId :: Integer,
        categoryId :: Integer,
        name :: Text,
        date :: Text,
        text :: Text,
        mainPhoto :: Image
      }
  | FullPost
      { postId :: Integer,
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
      }
  deriving (Show, Eq, Generic)

instance ToJSON Post where
  toEncoding = genericToEncoding defaultOptions {sumEncoding = UntaggedValue}

postNotExist :: Text
postNotExist = "Post with such id doesn't exist"

malformedPost :: Text
malformedPost = "Malformed posts"

noPost :: Text
noPost = "No posts with such parameters"
