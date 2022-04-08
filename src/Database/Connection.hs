{-# LANGUAGE OverloadedStrings #-}

module Database.Connection (manage, open, serverAddress) where

import qualified Data.Configurator as Config
import Data.Text (Text, append, pack)
import Database.PostgreSQL.Simple
  ( ConnectInfo
      ( connectDatabase,
        connectHost,
        connectPassword,
        connectPort,
        connectUser
      ),
    Connection,
    close,
    connect,
    defaultConnectInfo,
  )

open :: IO Connection
open = do
  config <- Config.load [Config.Required "Database.config"]
  host <- Config.require config "host"
  port <- Config.require config "port"
  user <- Config.require config "user"
  password <- Config.require config "password"
  database <- Config.require config "database"
  connect
    defaultConnectInfo
      { connectHost = host,
        connectPort = port,
        connectUser = user,
        connectPassword = password,
        connectDatabase = database
      }

serverAddress :: IO Text
serverAddress = do
  config <- Config.load [Config.Required "Server.config"]
  host <- Config.require config "host"
  port <- Config.require config "port"
  return $ "http://" `append` host `append` ":" `append` pack (show (port :: Integer))

manage :: (Connection -> IO b) -> IO b
manage f = do
  conn <- open
  result <- f conn
  close conn
  return result
