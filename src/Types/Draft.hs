{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Types.Draft where

import Database.PostgreSQL.Simple.ToField
import Data.Text
import Data.Aeson
import GHC.Generics
import Types.Author (AuthorId)
import Types.Category (CategoryId)
import Types.Tag (TagId)
import Types.Post (PostId)
import Types.Image

type DraftId = Integer
type Name = Text
newtype Description = Description Text deriving (Show, Generic)

data Draft = Draft {
    draftId :: Maybe DraftId,
    postId :: Maybe PostId,
    authorId :: AuthorId,
    categoryId :: CategoryId,
    tagId :: [TagId],
    name :: Name,
    description :: Description,
    mainPhoto :: Image,
    minorPhoto :: [Image] 
} deriving (Show, Generic)

data EditParams = EditParams {
    eCategoryId :: Maybe CategoryId,
    eTagId :: Maybe [TagId],
    eName :: Maybe Name,
    eDescription :: Maybe Description,
    eMainPhoto :: Maybe Image
}

instance ToJSON Description where
    toEncoding = genericToEncoding defaultOptions {sumEncoding = UntaggedValue}

instance FromJSON Description where 
    parseJSON = withObject "Draft description" $ \obj -> do 
        descr <- obj .: "description"
        return $ Description descr

instance ToField Description where 
    toField (Description x) = toField x 

instance ToJSON Draft where
    toEncoding = genericToEncoding defaultOptions {sumEncoding = UntaggedValue}
