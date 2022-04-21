module Types.Config where

import Data.Binary (Word16)
import Data.Text (Text)

type ServerAddress = Text

data Config = Config
  { database :: DatabaseConfig,
    server :: ServerConfig
  }

data DatabaseConfig = DatabaseConfig
  { dHost :: String,
    dPort :: Word16,
    dUser :: String,
    dPassword :: String,
    dName :: String
  }

data ServerConfig = ServerConfig
  { sAddress :: ServerAddress,
    sHost :: Text,
    sPort :: Integer
  }
