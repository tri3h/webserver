module Main where

import qualified Config
import Control.Monad (unless, void)
import Data.Pool (Pool, withResource)
import qualified Database.Connection as Connection
import qualified Database.Migration
import Database.PostgreSQL.Simple (Connection)
import qualified Database.Queries.User as UserDb
import qualified Logger
import qualified Server
import Types.Config (database)
import qualified User

main :: IO ()
main = do
  logger <- Logger.make
  config <- Config.make logger
  let db = database config
  void $ Database.Migration.execute db
  pool <- Connection.makePool db
  makeAdmin pool
  Server.run logger config pool

makeAdmin :: Pool Connection -> IO ()
makeAdmin pool = do
  hasAdmin <- withResource pool UserDb.hasAdmin
  unless hasAdmin askUser
  where
    askUser = do
      print "There is no admin in the database. Do you want to create a default one? (y/n)"
      answer <- getLine
      case answer of
        "y" -> do
          result <- User.makeDefaultAdmin pool
          print result
        "n" -> return ()
        _ -> do
          print "Answer is not recognized"
          askUser
