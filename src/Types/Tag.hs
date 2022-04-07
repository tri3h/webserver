{-# LANGUAGE DeriveGeneric #-}
module Types.Tag where

import Data.Text ( Text )
import Data.Aeson
    ( defaultOptions,
      genericToEncoding,
      SumEncoding(UntaggedValue),
      Options(sumEncoding),
      ToJSON(toEncoding) )
import GHC.Generics ( Generic )

type TagId = Integer
type Name = Text

data Tag = Tag {
    tagId :: Integer,
    name :: Text
} deriving (Show, Eq, Generic)

instance ToJSON Tag where
    toEncoding = genericToEncoding defaultOptions {sumEncoding = UntaggedValue}
