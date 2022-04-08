module Handlers.Tag where

import Data.Text (Text)
import Types.Tag (Name, Tag (tagId), TagId)

data Handle m = Handle
  { hCreate :: Name -> m (),
    hGet :: TagId -> m Tag,
    hDelete :: TagId -> m (),
    hEdit :: Tag -> m (),
    hDoesExist :: TagId -> m (Either Text ())
  }

create :: Monad m => Handle m -> Name -> m (Either Text ())
create handle name = Right <$> hCreate handle name

get :: Monad m => Handle m -> TagId -> m (Either Text Tag)
get handle tId = do
  exist <- hDoesExist handle tId
  case exist of
    Right _ -> Right <$> hGet handle tId
    Left l -> return $ Left l

edit :: Monad m => Handle m -> Tag -> m (Either Text ())
edit handle tag = do
  exist <- hDoesExist handle $ tagId tag
  case exist of
    Right _ -> Right <$> hEdit handle tag
    Left l -> return $ Left l

delete :: Monad m => Handle m -> TagId -> m (Either Text ())
delete handle tId = do
  exist <- hDoesExist handle tId
  case exist of
    Right _ -> Right <$> hDelete handle tId
    Left l -> return $ Left l
