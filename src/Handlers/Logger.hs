module Handlers.Logger (Handle (..), Verbosity (..), debug, info, warning, error, fromString) where

import Control.Monad (when)
import Prelude hiding (error, log)

data Handle m = Handle
  { hWriteLog :: String -> m (),
    hVerbosity :: Verbosity
  }

data Verbosity = Debug | Info | Warning | Error deriving (Eq, Ord, Show)

log :: Monad m => Handle m -> Verbosity -> String -> m ()
log handle v str = when (v >= hVerbosity handle) $ hWriteLog handle $ toString v str

debug :: Monad m => Handle m -> String -> m ()
debug handle = log handle Debug

info :: Monad m => Handle m -> String -> m ()
info handle = log handle Info

warning :: Monad m => Handle m -> String -> m ()
warning handle = log handle Warning

error :: Monad m => Handle m -> String -> m ()
error handle = log handle Error

fromString :: String -> Maybe Verbosity
fromString "Info" = Just Info
fromString "Debug" = Just Debug
fromString "Warning" = Just Warning
fromString "Error" = Just Error
fromString _ = Nothing

toString :: Verbosity -> String -> String
toString v str = show v ++ ": " ++ str
