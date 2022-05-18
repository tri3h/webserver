module Main where

import qualified Admin
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
  void $ Database.Migration.execute db
  pool <- Connection.makePool db
  Admin.check pool
  Server.run logger config pool
