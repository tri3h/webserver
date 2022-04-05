{-# LANGUAGE DeriveGeneric #-}
module Types.Draft where

import Database.PostgreSQL.Simple.ToField ()
import Data.Text ( Text )
import Data.Aeson
    ( defaultOptions,
      genericToEncoding,
      SumEncoding(UntaggedValue),
      Options(sumEncoding),
      ToJSON(toEncoding) )
import GHC.Generics ( Generic )
import Types.Author (AuthorId)
import Types.Category (CategoryId)
import Types.Tag (TagId)
import Types.Post (PostId)
import Types.Image ( Image )

type DraftId = Integer
type Name = Text
type Description = Text

data Draft = Draft {
    draftId :: Maybe DraftId,
    postId :: Maybe PostId,
    authorId :: AuthorId,
    categoryId :: CategoryId,
    tagId :: [TagId],
    name :: Name,
    description :: Text,
    mainPhoto :: Image,
    minorPhoto :: [Image] 
} deriving (Show, Generic)

data EditParams = EditParams {
    eCategoryId :: Maybe CategoryId,
    eTagId :: Maybe [TagId],
    eName :: Maybe Name,
    eDescription :: Maybe Text,
    eMainPhoto :: Maybe Image
}

instance ToJSON Draft where
    toEncoding = genericToEncoding defaultOptions {sumEncoding = UntaggedValue}
