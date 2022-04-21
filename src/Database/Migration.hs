module Database.Migration where

import Database.Connection (open)
import Database.PostgreSQL.Simple (withTransaction)
import Database.PostgreSQL.Simple.Migration
  ( MigrationCommand (MigrationDirectory, MigrationInitialization),
    MigrationContext (MigrationContext),
    MigrationResult,
    runMigration,
  )
import Types.Config (DatabaseConfig)

execute :: DatabaseConfig -> IO (MigrationResult String)
execute config = do
  conn <- open config
  _ <-
    withTransaction conn $
      runMigration $
        MigrationContext MigrationInitialization True conn
  withTransaction conn $
    runMigration $
      MigrationContext (MigrationDirectory "DatabaseMigrations") True conn
