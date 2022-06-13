module Main where

import qualified Config
import Control.Monad (void)
import qualified Database.Connection as Connection
import qualified Database.Migration
import qualified Logger
import qualified Server
import Types.Config (database)

main :: IO ()
main = do
  logger <- Logger.make
  config <- Config.make logger
  let db = database config
  void $ Database.Migration.execute db logger
  pool <- Connection.makePool db
  Server.run logger config pool
