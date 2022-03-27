{-# LANGUAGE DeriveGeneric #-}
module Types.Image where

import Data.Text 
import GHC.Generics
import Data.Aeson
import Database.PostgreSQL.Simple.ToField

data Image = Image Text | Link Text
    deriving (Show, Read, Generic)

instance ToJSON Image where 
    toEncoding = genericToEncoding defaultOptions {sumEncoding = UntaggedValue}

instance ToField Image where 
    toField (Image x) = toField x 
    toField (Link x) = toField x
