module Database.Migration where

import Database.Connection (tryOpenConnection)
import Database.PostgreSQL.Simple (close, withTransaction)
import Database.PostgreSQL.Simple.Migration
  ( MigrationCommand (MigrationDirectory, MigrationInitialization),
    MigrationContext (MigrationContext),
    MigrationResult,
    runMigration,
  )
import qualified Handlers.Logger as Logger
import Types.Config (DatabaseConfig)

execute :: DatabaseConfig -> Logger.Handle IO -> IO (MigrationResult String)
execute config logger = do
  conn <- tryOpenConnection config logger
  _ <-
    withTransaction conn $
      runMigration $
        MigrationContext MigrationInitialization True conn
  result <-
    withTransaction conn $
      runMigration $
        MigrationContext (MigrationDirectory "DatabaseMigrations") True conn
  close conn
  return result
