{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Types.User where

import Data.Aeson
  ( KeyValue ((.=)),
    ToJSON (toJSON),
    object,
  )
import Data.Text (Text)
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.ToField (ToField)
import Types.Image (Image, Link)

newtype Login = Login {getLogin :: Text} deriving (Show, Eq, ToField, FromField, ToJSON)

newtype Token = Token {getToken :: Text} deriving (Show, Eq, ToField, FromField, ToJSON)

newtype UserId = UserId {getUserId :: Integer} deriving (Show, Eq, ToField, FromField, ToJSON)

newtype Password = Password {getPassword :: Text} deriving (Show, Eq, ToField, FromField)

newtype Name = Name {getName :: Text} deriving (Show, Eq, ToField, FromField, ToJSON)

newtype Surname = Surname {getSurname :: Text} deriving (Show, Eq, ToField, FromField, ToJSON)

newtype Admin = Admin {getAdmin :: Bool} deriving (Show, Eq, ToField, FromField)

newtype Date = Date {getDate :: Text} deriving (Show, Eq, ToField, FromField, ToJSON)

data GetUser = GetUser
  { gUserId :: UserId,
    gName :: Name,
    gSurname :: Surname,
    gAvatar :: Link,
    gLogin :: Login,
    gDate :: Date
  }
  deriving (Show, Eq)

data CreateUser = CreateUser
  { cName :: Name,
    cSurname :: Surname,
    cLogin :: Login,
    cPassword :: Password,
    cAvatar :: Image
  }
  deriving (Show, Eq)

data FullUser = FullUser
  { fName :: Name,
    fSurname :: Surname,
    fAvatar :: Image,
    fLogin :: Login,
    fPassword :: Password,
    fAdmin :: Admin,
    fToken :: Token
  }
  deriving (Show, Eq)

instance ToJSON GetUser where
  toJSON user =
    object
      [ "user_id" .= gUserId user,
        "name" .= gName user,
        "surname" .= gSurname user,
        "avatar" .= gAvatar user,
        "login" .= gLogin user,
        "date" .= gDate user
      ]
