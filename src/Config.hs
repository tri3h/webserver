{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Config (make, loggerError) where

import qualified Data.Configurator as C
import Data.Text (append, pack)
import qualified Handlers.Logger as Logger
import System.Exit (exitFailure)
import Types.Config
  ( Config (..),
    DatabaseConfig (..),
    ServerConfig (..),
  )

make :: Logger.Handle IO -> IO Config
make logger = do
  database <- getDatabaseConfig logger
  server <- getServerConfig logger
  return Config {..}

getDatabaseConfig :: Logger.Handle IO -> IO DatabaseConfig
getDatabaseConfig logger = do
  config <- C.load [C.Required "Configs/Server.config"]
  maybeHost <- C.lookup config "db.host"
  dHost <- case maybeHost of
    Just x -> return x
    Nothing -> loggerError logger "Database host has invalid format"
  maybePort <- C.lookup config "db.port"
  dPort <- case maybePort of
    Just x -> return x
    Nothing -> loggerError logger "Database port has invalid format"
  maybeUser <- C.lookup config "db.user"
  dUser <- case maybeUser of
    Just x -> return x
    Nothing -> loggerError logger "User name for access to a database has invalid format"
  maybePassword <- C.lookup config "db.password"
  dPassword <- case maybePassword of
    Just x -> return x
    Nothing -> loggerError logger "Password for access to a database has invalid format"
  maybeName <- C.lookup config "db.name"
  dName <- case maybeName of
    Just x -> return x
    Nothing -> loggerError logger "Database name has invalid format"
  return DatabaseConfig {..}

getServerConfig :: Logger.Handle IO -> IO ServerConfig
getServerConfig logger = do
  config <- C.load [C.Required "Configs/Server.config"]
  maybeHost <- C.lookup config "server.host"
  sHost <- case maybeHost of
    Just x -> return x
    Nothing -> loggerError logger "Server host has invalid format"
  maybePort <- C.lookup config "server.port"
  sPort <- case maybePort of
    Just x -> return x
    Nothing -> loggerError logger "Server port has invalid format"
  return $
    ServerConfig
      { sAddress = "http://" `append` sHost `append` ":" `append` pack (show (sPort :: Integer)),
        ..
      }

loggerError :: Logger.Handle IO -> String -> IO b
loggerError logger msg = do
  Logger.error logger msg
  exitFailure
