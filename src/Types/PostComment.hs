{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types.PostComment where

import Data.Aeson (ToJSON)
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.ToField (ToField)

newtype PostId = PostId {getPostId :: Integer} deriving (Show, Eq, ToField, FromField, ToJSON)
