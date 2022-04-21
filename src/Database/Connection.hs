module Database.Connection (manage, open) where

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
import Types.Config (DatabaseConfig (dHost, dName, dPassword, dPort, dUser))

open :: DatabaseConfig -> IO Connection
open config =
  connect
    defaultConnectInfo
      { connectHost = dHost config,
        connectPort = dPort config,
        connectUser = dUser config,
        connectPassword = dPassword config,
        connectDatabase = dName config
      }

manage :: DatabaseConfig -> (Connection -> IO b) -> IO b
manage config f = do
  conn <- open config
  result <- f conn
  close conn
  return result
