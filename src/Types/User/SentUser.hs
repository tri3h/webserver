{-# LANGUAGE DeriveGeneric #-}
module Types.User.SentUser where

import Data.Text
import Data.Aeson
import GHC.Generics

data SentUser = SentUser {
    userId :: Integer,
    name :: Text,
    surname :: Text,
    avatar :: Text,
    login :: Text,
    date :: Text
} deriving (Show, Generic) 

instance ToJSON SentUser where