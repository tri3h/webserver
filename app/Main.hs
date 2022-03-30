module Main where

import qualified Server
import qualified Database.Migration

main :: IO ()
main = do 
    Database.Migration.execute
    Server.main

