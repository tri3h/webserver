{-# LANGUAGE OverloadedStrings #-}

module Handlers.Image where

import Error (Error)
import qualified Handlers.Logger as Logger
import Network.HTTP.Types (QueryText)
import Network.Wai (Response)
import Types.Image (Image (Image), ImageType)
import Utility (getInteger, getText, response200Image, response400)

newtype Handle m = Handle
  { hGet :: Integer -> m (Either Error Image)
  }

get :: Monad m => Handle m -> Logger.Handle m -> QueryText -> m Response
get handle logger query = do
  let info = getInteger query "image_id"
  Logger.debug logger $ "Tried to parse query and got: " ++ show info
  case info of
    Right imageId -> do
      result <- hGet handle imageId
      return $ case result of
        Right (Image image imageType) -> response200Image imageType image
        Left l -> response400 l
    Left l -> return $ response400 l

getImageType :: QueryText -> Either Error ImageType
getImageType query = getText query "image_type"
