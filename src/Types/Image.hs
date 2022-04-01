{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Types.Image where

import Data.Text 
import GHC.Generics
import Data.Aeson
import Database.PostgreSQL.Simple.ToField

data Image = Link Text | Image Text ImageType
    deriving (Show, Read, Generic)

type ImageId = Integer
type ImageType = Text

instance ToJSON Image where 
    toEncoding = genericToEncoding defaultOptions {sumEncoding = UntaggedValue}
    
instance ToField Image where 
    toField (Image image imageType) = toField image
    toField (Link x) = toField x
