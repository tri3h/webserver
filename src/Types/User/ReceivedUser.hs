{-# LANGUAGE DeriveGeneric #-}
module Types.User.ReceivedUser where

import Data.Text
import Data.Aeson
import GHC.Generics

data ReceivedUser = ReceivedUser {
    name :: Text,
    surname :: Text,
    avatar :: Text,
    login :: Text,
    password :: Text
} deriving (Show, Generic) 

instance FromJSON ReceivedUser where