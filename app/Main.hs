module Main where

import qualified Config
import qualified Database.Migration
import qualified Logger
import qualified Server
import Types.Config (database)

main :: IO ()
main = do
  logger <- Logger.make
  config <- Config.make logger
  _ <- Database.Migration.execute (database config)
  Server.run logger config
