module Database.Migration where

import Database.Connection (openConnection)
import Database.PostgreSQL.Simple (close, withTransaction)
import Database.PostgreSQL.Simple.Migration
  ( MigrationCommand (MigrationDirectory, MigrationInitialization),
    MigrationContext (MigrationContext),
    MigrationResult,
    runMigration,
  )
import Types.Config (DatabaseConfig)

execute :: DatabaseConfig -> IO (MigrationResult String)
execute config = do
  conn <- openConnection config
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
