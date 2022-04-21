{-# LANGUAGE OverloadedStrings #-}

module Logger where

import qualified Data.Configurator as Config
import Data.Maybe (fromMaybe)
import qualified Handlers.Logger as Logger

defaultLogVerbosity :: Logger.Verbosity
defaultLogVerbosity = Logger.Error

make :: IO (Logger.Handle IO)
make = do
  config <- Config.load [Config.Required "Configs/Server.config"]
  maybeLogVerbosity <- Config.lookup config "log_verbosity"
  let logVerbosity = case maybeLogVerbosity of
        Just x -> fromMaybe defaultLogVerbosity $ Logger.fromString x
        Nothing -> defaultLogVerbosity
  return
    Logger.Handle
      { Logger.hWriteLog = putStrLn,
        Logger.hVerbosity = logVerbosity
      }
