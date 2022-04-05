module Database.Migration where

import Database.PostgreSQL.Simple.Migration
    ( MigrationResult,
      runMigration,
      MigrationCommand(MigrationDirectory, MigrationInitialization),
      MigrationContext(MigrationContext) )
import Database.Connection ( open )
import Database.PostgreSQL.Simple (withTransaction)

execute :: IO (MigrationResult String)
execute = do 
    conn <- open 
    withTransaction conn $ runMigration $
        MigrationContext MigrationInitialization True conn
    withTransaction conn $ runMigration $
        MigrationContext (MigrationDirectory "DatabaseMigrations") True conn
