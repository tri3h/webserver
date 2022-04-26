module Database.Connection (makePool, tryOpenConnection) where

import Data.Pool (Pool, createPool)
import Database.PostgreSQL.Simple
  ( ConnectInfo (..),
    Connection,
    close,
    connect,
    defaultConnectInfo,
  )
import GHC.IO (catchAny)
import qualified Handlers.Logger as Logger
import System.Exit (exitFailure)
import Types.Config (DatabaseConfig (..))

tryOpenConnection :: DatabaseConfig -> Logger.Handle IO -> IO Connection
tryOpenConnection config logger =
  catchAny
    (openConnection config)
    ( \e -> do
        Logger.error logger $ show e
        exitFailure
    )

openConnection :: DatabaseConfig -> IO Connection
openConnection config =
  connect $
    defaultConnectInfo
      { connectHost = dHost config,
        connectPort = dPort config,
        connectUser = dUser config,
        connectPassword = dPassword config,
        connectDatabase = dName config
      }

makePool :: DatabaseConfig -> Logger.Handle IO -> IO (Pool Connection)
makePool config logger = createPool (tryOpenConnection config logger) close 1 10 10
