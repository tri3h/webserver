module Main where

import qualified Config
import qualified Database.Migration
import qualified Logger
import qualified Server
import System.Environment (getArgs)
import Types.Config (database)

main :: IO ()
main = do
  args <- getArgs
  logger <- Logger.make
  config <- Config.make logger
  case args of
    ["1"] -> do
      _ <- Database.Migration.execute (database config)
      return ()
    _ -> return ()
  Server.run logger config
