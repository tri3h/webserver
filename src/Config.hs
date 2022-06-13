{-# LANGUAGE OverloadedStrings #-}

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
  db <- getDatabaseConfig logger
  serv <- getServerConfig logger
  return
    Config
      { database = db,
        server = serv
      }

getDatabaseConfig :: Logger.Handle IO -> IO DatabaseConfig
getDatabaseConfig logger = do
  config <- C.load [C.Required "Configs/Server.config"]
  maybeHost <- C.lookup config "db.host"
  host <- case maybeHost of
    Just x -> return x
    Nothing -> loggerError logger "Database host has invalid format"
  maybePort <- C.lookup config "db.port"
  port <- case maybePort of
    Just x -> return x
    Nothing -> loggerError logger "Database port has invalid format"
  maybeUser <- C.lookup config "db.user"
  user <- case maybeUser of
    Just x -> return x
    Nothing -> loggerError logger "User name for access to a database has invalid format"
  maybePassword <- C.lookup config "db.password"
  password <- case maybePassword of
    Just x -> return x
    Nothing -> loggerError logger "Password for access to a database has invalid format"
  maybeName <- C.lookup config "db.name"
  name <- case maybeName of
    Just x -> return x
    Nothing -> loggerError logger "Database name has invalid format"
  return
    DatabaseConfig
      { dPort = port,
        dUser = user,
        dHost = host,
        dPassword = password,
        dName = name
      }

getServerConfig :: Logger.Handle IO -> IO ServerConfig
getServerConfig logger = do
  config <- C.load [C.Required "Configs/Server.config"]
  maybeHost <- C.lookup config "server.host"
  host <- case maybeHost of
    Just x -> return x
    Nothing -> loggerError logger "Server host has invalid format"
  maybePort <- C.lookup config "server.port"
  port <- case maybePort of
    Just x -> return x
    Nothing -> loggerError logger "Server port has invalid format"
  return $
    ServerConfig
      { sAddress = "http://" `append` host `append` ":" `append` pack (show (port :: Integer)),
        sPort = port,
        sHost = host
      }

loggerError :: Logger.Handle IO -> String -> IO b
loggerError logger msg = do
  Logger.error logger msg
  exitFailure
