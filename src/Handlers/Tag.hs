module Handlers.Tag where

import Error (Error)
import Types.Tag (Name, Tag (tagId), TagId)

data Handle m = Handle
  { hCreate :: Name -> m (Either Error ()),
    hGet :: TagId -> m Tag,
    hGetAll :: m [Tag],
    hDelete :: TagId -> m (),
    hEdit :: Tag -> m (),
    hDoesExist :: TagId -> m (Either Error ())
  }

get :: Monad m => Handle m -> TagId -> m (Either Error Tag)
get handle tId = do
  exist <- hDoesExist handle tId
  case exist of
    Right _ -> Right <$> hGet handle tId
    Left l -> return $ Left l

edit :: Monad m => Handle m -> Tag -> m (Either Error ())
edit handle tag = do
  exist <- hDoesExist handle $ tagId tag
  case exist of
    Right _ -> Right <$> hEdit handle tag
    Left l -> return $ Left l

delete :: Monad m => Handle m -> TagId -> m (Either Error ())
delete handle tId = do
  exist <- hDoesExist handle tId
  case exist of
    Right _ -> Right <$> hDelete handle tId
    Left l -> return $ Left l
