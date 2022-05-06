module Handlers.Author where

import Data.Text (Text)
import Types.Author
  ( AuthorId,
    CreateAuthor,
    EditAuthor (eAuthorId),
    GetAuthor,
  )

data Handle m = Handle
  { hCreate :: CreateAuthor -> m (Either Text ()),
    hGet :: AuthorId -> m GetAuthor,
    hDelete :: AuthorId -> m (),
    hEdit :: EditAuthor -> m (),
    hDoesExist :: AuthorId -> m (Either Text ())
  }

get :: Monad m => Handle m -> AuthorId -> m (Either Text GetAuthor)
get handle authId = do
  exist <- hDoesExist handle authId
  case exist of
    Right _ -> Right <$> hGet handle authId
    Left l -> return $ Left l

delete :: Monad m => Handle m -> AuthorId -> m (Either Text ())
delete handle authId = do
  exist <- hDoesExist handle authId
  case exist of
    Right () -> do
      hDelete handle authId
      return $ Right ()
    Left l -> return $ Left l

edit :: Monad m => Handle m -> EditAuthor -> m (Either Text ())
edit handle author = do
  exist <- hDoesExist handle $ eAuthorId author
  case exist of
    Right () -> do
      hEdit handle author
      return $ Right ()
    Left l -> return $ Left l
