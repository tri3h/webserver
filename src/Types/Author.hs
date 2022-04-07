{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Types.Author where

import Data.Text ( Text )
import Data.Aeson
    ( defaultOptions,
      genericToEncoding,
      SumEncoding(UntaggedValue),
      Options(sumEncoding),
      ToJSON(toEncoding),
      FromJSON )
import GHC.Generics ( Generic )

type AuthorId = Integer

data Author = AuthorToGet {
    authorId :: Integer,
    userId :: Integer,
    description :: Text
} | AuthorToEdit {
    authorId :: Integer,
    description :: Text
} | AuthorToCreate {
    userId :: Integer,
    description :: Text
} deriving (Show, Eq, Generic)

instance ToJSON Author where
    toEncoding = genericToEncoding defaultOptions {sumEncoding = UntaggedValue}

instance FromJSON Author where

authorNotExist :: Text
authorNotExist = "Author with such id doesn't exist"

malformedAuthor :: Text
malformedAuthor = "Malformed author"