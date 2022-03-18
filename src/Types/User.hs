{-# LANGUAGE DeriveGeneric #-}
module Types.User where

import Data.Text
import Data.Aeson
import GHC.Generics

data User = FullUser {
    name :: Text,
    surname :: Text,
    avatar :: Text,
    login :: Text,
    password :: Text,
    date :: Text,
    admin :: Bool,
    token :: Text
} | GetUser {
    userId :: Integer,
    name :: Text,
    surname :: Text,
    avatar :: Text,
    login :: Text,
    date :: Text
} | CreateUser {
    name :: Text,
    surname :: Text,
    login :: Text,
    password :: Text,
    avatar :: Text
} deriving (Show, Generic)

type Login = Text
type Token = Text 
type UserId = Integer
type Password = Text

instance ToJSON User where
    toEncoding = genericToEncoding defaultOptions {sumEncoding = UntaggedValue}
