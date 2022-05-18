{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types.Limit where

import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.ToField (ToField)

newtype Offset = Offset Integer deriving (Show, Eq, ToField, FromField)

newtype Limit = Limit Integer deriving (Show, Eq, Ord, ToField, FromField)
