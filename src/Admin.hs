{-# LANGUAGE OverloadedStrings #-}

module Admin where

import Config (loggerError)
import Control.Monad (unless)
import qualified Data.Configurator as C
import Data.Pool (Pool, withResource)
import Data.Text (pack)
import Database.PostgreSQL.Simple (Connection)
import qualified Database.Queries.User as UserDb
import qualified Handlers.Logger as Logger
import Types.User (CreateUser (..), Login (Login), Name (Name), Password (Password), Surname (Surname))
import qualified User

check :: Logger.Handle IO -> Pool Connection -> IO ()
check logger pool = do
  hasAdmin <- withResource pool UserDb.hasAdmin
  unless hasAdmin (make logger pool)

make :: Logger.Handle IO -> Pool Connection -> IO ()
make logger pool = do
  admin <- getDefaultAdmin logger
  case admin of
    Just x -> do
      result <- User.makeAdmin pool x
      Logger.info logger $ show result
    Nothing -> return ()

getDefaultAdmin :: Logger.Handle IO -> IO (Maybe CreateUser)
getDefaultAdmin logger = do
  config <- C.load [C.Required "Configs/Server.config"]
  isNeeded <- C.lookup config "admin.is_needed"
  case isNeeded of
    Just True -> do
      maybeName <- C.lookup config "admin.name"
      name <- case maybeName of
        Just x@(_ : _) -> return . Name $ pack x
        _ -> loggerError logger "Admin name has invalid format"
      maybeSurname <- C.lookup config "admin.surname"
      surname <- case maybeSurname of
        Just x@(_ : _) -> return . Surname $ pack x
        _ -> loggerError logger "Admin surname has invalid format"
      maybeLogin <- C.lookup config "admin.login"
      login <- case maybeLogin of
        Just x@(_ : _) -> return . Login $ pack x
        _ -> loggerError logger "Admin login has invalid format"
      maybePassword <- C.lookup config "admin.password"
      password <- case maybePassword of
        Just x@(_ : _) -> return . Password $ pack x
        _ -> loggerError logger "Admin password has invalid format"
      return $
        Just
          CreateUser
            { cAvatar = Nothing,
              cName = name,
              cLogin = login,
              cPassword = password,
              cSurname = surname
            }
    _ -> return Nothing
