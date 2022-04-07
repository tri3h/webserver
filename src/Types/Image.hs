{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Types.Image where

import GHC.Generics ( Generic )
import Data.Aeson
    ( defaultOptions,
      genericToEncoding,
      SumEncoding(UntaggedValue),
      Options(sumEncoding),
      ToJSON(toEncoding) )
import Database.PostgreSQL.Simple.ToField ( ToField(..) )
import Data.Text ( Text )

type ImageId = Integer
type ImageType = Text

data Image = Link Text | Image Text ImageType
    deriving (Show, Eq, Read, Generic)

instance ToJSON Image where 
    toEncoding = genericToEncoding defaultOptions {sumEncoding = UntaggedValue}
    
instance ToField Image where 
    toField (Image image imageType) = toField image
    toField (Link x) = toField x

malformedImage :: Text
malformedImage = "Malformed image"