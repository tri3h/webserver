module Database.Connection (makePool, openConnection) where

import Data.Pool (Pool, createPool)
import Database.PostgreSQL.Simple
  ( ConnectInfo (..),
    Connection,
    close,
    connect,
    defaultConnectInfo,
  )
import Types.Config (DatabaseConfig (..))

openConnection :: DatabaseConfig -> IO Connection
openConnection config =
  connect
    defaultConnectInfo
      { connectHost = dHost config,
        connectPort = dPort config,
        connectUser = dUser config,
        connectPassword = dPassword config,
        connectDatabase = dName config
      }

makePool :: DatabaseConfig -> IO (Pool Connection)
makePool config = createPool (openConnection config) close 1 10 10
