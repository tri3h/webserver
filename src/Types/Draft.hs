{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Types.Draft where

import Data.Aeson
  ( KeyValue ((.=)),
    ToJSON,
    object,
  )
import Data.Aeson.Types (ToJSON (toJSON))
import Data.Text (Text)
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.ToField (ToField)
import Types.Author (AuthorId)
import Types.Category (CategoryId)
import Types.Image (Image, Link)
import Types.Limit (Limit (..))
import Types.PostComment (PostId)
import Types.Tag (TagId)
import Types.User (Token)

newtype DraftId = DraftId {getDraftId :: Integer} deriving (Show, Eq, ToField, FromField, ToJSON)

newtype Name = Name {getName :: Text} deriving (Show, Eq, ToField, FromField, ToJSON)

data GetDraft = GetDraft
  { gDraftId :: DraftId,
    gPostId :: Maybe PostId,
    gAuthorId :: Maybe AuthorId,
    gCategoryId :: Maybe CategoryId,
    gTagId :: [TagId],
    gName :: Name,
    gText :: Text,
    gMainPhoto :: Maybe Link,
    gMinorPhoto :: [Link]
  }
  deriving (Show, Eq)

data CreateDraft = CreateDraft
  { cToken :: Token,
    cCategoryId :: CategoryId,
    cTagId :: [TagId],
    cName :: Name,
    cText :: Text,
    cMainPhoto :: Maybe Image
  }
  deriving (Show, Eq)

data EditParams = EditParams
  { eCategoryId :: Maybe CategoryId,
    eTagId :: [TagId],
    eName :: Maybe Name,
    eText :: Maybe Text,
    eMainPhoto :: Maybe Image
  }
  deriving (Show, Eq)

instance ToJSON GetDraft where
  toJSON draft =
    object
      [ "draft_id" .= gDraftId draft,
        "post_id" .= gPostId draft,
        "author_id" .= gAuthorId draft,
        "category_id" .= gCategoryId draft,
        "tag_id" .= gTagId draft,
        "name" .= gName draft,
        "text" .= gText draft,
        "main_photo" .= gMainPhoto draft,
        "minor_photo" .= gMinorPhoto draft
      ]

draftsOnPage :: Limit
draftsOnPage = Limit 10
