{-# LANGUAGE DeriveGeneric #-}
module Types.User where

import Data.Text
import Data.Aeson
import GHC.Generics
import Types.Image

data User = UserToDatabase {
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

type Login = Text
type Token = Text 
type UserId = Integer
type Password = Text
type Name = Text
type Surname = Text 

instance ToJSON User where
    toEncoding = genericToEncoding defaultOptions {sumEncoding = UntaggedValue}
