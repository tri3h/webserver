{-# LANGUAGE RecordWildCards #-}

module Admin where

import Control.Monad (unless)
import Data.Pool (Pool, withResource)
import Data.Text (pack)
import Database.PostgreSQL.Simple (Connection)
import qualified Database.Queries.User as UserDb
import Types.User (CreateUser (..), Login (Login), Name (Name), Password (Password), Surname (Surname))
import qualified User

check :: Pool Connection -> IO ()
check pool = do
  hasAdmin <- withResource pool UserDb.hasAdmin
  unless hasAdmin (make pool)

make :: Pool Connection -> IO ()
make pool = do
  print "There is no admin in the database. Do you want to create one? (y/n)"
  answer <- getLine
  case answer of
    "y" -> set pool
    "n" -> return ()
    _ -> do
      notRecognized
      make pool

set :: Pool Connection -> IO ()
set pool = do
  print $ "Do you want to use default parameters? (y/n) Default parameters are: " ++ show defaultAdmin
  answer <- getLine
  case answer of
    "y" -> do
      result <- User.makeAdmin pool defaultAdmin
      print result
    "n" -> do
      print "Type name"
      cName <- Name . pack <$> getLine
      print "Type surname"
      cSurname <- Surname . pack <$> getLine
      print "Type login"
      cLogin <- Login . pack <$> getLine
      print "Type password"
      cPassword <- Password . pack <$> getLine
      result <- User.makeAdmin pool $ CreateUser {cAvatar = Nothing, ..}
      print result
    _ -> do
      notRecognized
      set pool

notRecognized :: IO ()
notRecognized = print "Answer is not recognized"

defaultAdmin :: CreateUser
defaultAdmin =
  CreateUser
    { cName = Name $ pack "admin",
      cSurname = Surname $ pack "none",
      cLogin = Login $ pack "admin",
      cPassword = Password $ pack "adminpassword",
      cAvatar = Nothing
    }
