module Handler.Logger(Handle(..), Verbosity(..), debug, warning, error) where

import Prelude hiding (log, error)
import Control.Monad ( when )

data Handle m = Handle {
    hWriteLog :: String -> m (),
    hVerbosity :: Verbosity
}

data Verbosity = DEBUG | WARNING | ERROR deriving (Eq, Ord, Show)

log :: Monad m => Handle m -> Verbosity -> String -> m ()
log handle v str = when (v >= hVerbosity handle) $ hWriteLog handle $ toString v str

debug :: Monad m => Handle m -> String -> m ()
debug handle = log handle DEBUG

warning :: Monad m => Handle m -> String -> m ()
warning handle = log handle WARNING

error :: Monad m => Handle m -> String -> m ()
error handle = log handle ERROR

toString :: Verbosity -> String -> String
toString v str = show v ++ ": " ++ str