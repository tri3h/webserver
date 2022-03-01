module Types.User.FullUser where

import Data.Text
import Data.Aeson

data FullUser = FullUser {
    name :: Text,
    surname :: Text,
    avatar :: Text,
    login :: Text,
    password :: Text,
    date :: Text,
    admin :: Bool,
    token :: Text
} deriving Show

type Login = Text
type Token = Text 
type UserID = Integer
type Password = Text
