{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Config (make) where

import qualified Data.Configurator as C
import Data.Text (append, pack)
import qualified Handlers.Logger as Logger
import System.Exit (exitFailure)
import Types.Config
  ( Config (Config, database, server),
    DatabaseConfig (DatabaseConfig, dHost, dName, dPassword, dPort, dUser),
    ServerConfig (ServerConfig, sAddress, sHost, sPort),
  )

make :: Logger.Handle IO -> IO Config
make logger = do
  database <- getDatabaseConfig logger
  server <- getServerConfig logger
  return Config {..}

getDatabaseConfig :: Logger.Handle IO -> IO DatabaseConfig
getDatabaseConfig logger = do
  config <- C.load [C.Required "Configs/Database.config"]
  maybeHost <- C.lookup config "host"
  dHost <- case maybeHost of
    Just x -> return x
    Nothing -> do
      Logger.error logger "Database host has invalid format"
      exitFailure
  maybePort <- C.lookup config "port"
  dPort <- case maybePort of
    Just x -> return x
    Nothing -> do
      Logger.error logger "Database port has invalid format"
      exitFailure
  maybeUser <- C.lookup config "user"
  dUser <- case maybeUser of
    Just x -> return x
    Nothing -> do
      Logger.error logger "User name for access to a database has invalid format"
      exitFailure
  maybePassword <- C.lookup config "password"
  dPassword <- case maybePassword of
    Just x -> return x
    Nothing -> do
      Logger.error logger "Password for access to a database has invalid format"
      exitFailure
  maybeName <- C.lookup config "database"
  dName <- case maybeName of
    Just x -> return x
    Nothing -> do
      Logger.error logger "Database name has invalid format"
      exitFailure
  return DatabaseConfig {..}

getServerConfig :: Logger.Handle IO -> IO ServerConfig
getServerConfig logger = do
  config <- C.load [C.Required "Configs/Server.config"]
  maybeHost <- C.lookup config "host"
  sHost <- case maybeHost of
    Just x -> return x
    Nothing -> do
      Logger.error logger "Server host has invalid format"
      exitFailure
  maybePort <- C.lookup config "port"
  sPort <- case maybePort of
    Just x -> return x
    Nothing -> do
      Logger.error logger "Server port has invalid format"
      exitFailure
  return $
    ServerConfig
      { sAddress = "http://" `append` sHost `append` ":" `append` pack (show (sPort :: Integer)),
        ..
      }
