{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Types.Draft where

import Data.Aeson
  ( Options (sumEncoding),
    SumEncoding (UntaggedValue),
    ToJSON (toEncoding),
    defaultOptions,
    genericToEncoding,
  )
import Data.Text (Text)
import Database.PostgreSQL.Simple.ToField ()
import GHC.Generics (Generic)
import Types.Author (AuthorId)
import Types.Category (CategoryId)
import Types.Image (Image)
import Types.Post (PostId)
import Types.Tag (TagId)

type DraftId = Integer

type Name = Text

type Description = Text

data Draft = Draft
  { draftId :: Maybe DraftId,
    postId :: Maybe PostId,
    authorId :: AuthorId,
    categoryId :: CategoryId,
    tagId :: [TagId],
    name :: Name,
    description :: Text,
    mainPhoto :: Image,
    minorPhoto :: [Image]
  }
  deriving (Show, Eq, Generic)

data EditParams = EditParams
  { eCategoryId :: Maybe CategoryId,
    eTagId :: Maybe [TagId],
    eName :: Maybe Name,
    eDescription :: Maybe Text,
    eMainPhoto :: Maybe Image
  }

instance ToJSON Draft where
  toEncoding = genericToEncoding defaultOptions {sumEncoding = UntaggedValue}

draftNotExist :: Text
draftNotExist = "Draft with such id doesn't exist"

noDeleteHasPost :: Text
noDeleteHasPost = "Impossible to delete: has post"

noDraftAuthor :: Text
noDraftAuthor = "This author isn't author of the draft"

userNotAuthor :: Text
userNotAuthor = "The user isn't author"
