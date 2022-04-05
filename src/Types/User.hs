{-# LANGUAGE DeriveGeneric #-}
module Types.User where

import Data.Text ( Text )
import Data.Aeson
    ( defaultOptions,
      genericToEncoding,
      SumEncoding(UntaggedValue),
      Options(sumEncoding),
      ToJSON(toEncoding) )
import GHC.Generics ( Generic )
import Types.Image ( Image )

type Login = Text
type Token = Text 
type UserId = Integer
type Password = Text
type Name = Text
type Surname = Text 

data User = User {
    name :: Text,
    surname :: Text,
    avatar :: Image,
    login :: Text,
    password :: Text,
    date :: Text,
    admin :: Bool,
    token :: Text
} | UserToGet {
    userId :: Integer,
    name :: Text,
    surname :: Text,
    avatar :: Image,
    login :: Text,
    date :: Text
} | UserToCreate {
    name :: Text,
    surname :: Text,
    login :: Text,
    password :: Text,
    avatar :: Image
} deriving (Show, Generic)

instance ToJSON User where
    toEncoding = genericToEncoding defaultOptions {sumEncoding = UntaggedValue}
