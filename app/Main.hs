module Main where

import qualified Server
import qualified Database.Migration
import System.Environment ( getArgs )

main :: IO ()
main = do
    args <- getArgs
    case args of 
        ["1"] -> do 
            Database.Migration.execute
            return ()
        _ -> return ()
    Server.main