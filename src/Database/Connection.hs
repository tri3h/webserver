{-# LANGUAGE OverloadedStrings #-}
module Database.Connection (manage) where

import Database.PostgreSQL.Simple
import qualified Data.Configurator as Config

open :: IO Connection
open = do
    config <- Config.load [Config.Required "Database.config"]
    host <- Config.require config "host"
    port <- Config.require config "port"
    user <- Config.require config "user"
    password <- Config.require config "password"
    database <- Config.require config "database"
    connect defaultConnectInfo {connectHost = host, 
                                connectPort = port,
                                connectUser = user,
                                connectPassword = password,
                                connectDatabase = database}

manage :: (Connection -> IO b) -> IO b
manage f = do 
    conn <- open
    result <- f conn 
    close conn
    return result