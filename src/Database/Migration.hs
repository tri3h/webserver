module Database.Migration where

import Database.Connection (open)
import Database.PostgreSQL.Simple (withTransaction)
import Database.PostgreSQL.Simple.Migration
  ( MigrationCommand (MigrationDirectory, MigrationInitialization),
    MigrationContext (MigrationContext),
    MigrationResult,
    runMigration,
  )

execute :: IO (MigrationResult String)
execute = do
  conn <- open
  _ <-
    withTransaction conn $
      runMigration $
        MigrationContext MigrationInitialization True conn
  withTransaction conn $
    runMigration $
      MigrationContext (MigrationDirectory "DatabaseMigrations") True conn
