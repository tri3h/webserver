module Handlers.Category where

import Error (Error, invalidParent, noDeleteHasChildren)
import Types.Category
  ( CategoryId,
    Name,
    ParentId,
  )

data Handle m = Handle
  { hDelete :: CategoryId -> m (Either Error ()),
    hEditName :: CategoryId -> Name -> m (Either Error ()),
    hEditParent :: CategoryId -> ParentId -> m (Either Error ()),
    hGetChildren :: CategoryId -> m [CategoryId]
  }

delete :: Monad m => Handle m -> CategoryId -> m (Either Error ())
delete handle categId = do
  children <- hGetChildren handle categId
  if null children
    then hDelete handle categId
    else return $ Left noDeleteHasChildren

edit :: Monad m => Handle m -> CategoryId -> Maybe Name -> Maybe ParentId -> m (Either Error ())
edit handle categId name parId = do
  resName <- case name of
    Nothing -> return $ Right ()
    Just n -> hEditName handle categId n
  resParent <- case parId of
    Nothing -> return $ Right ()
    Just p -> do
      children <- hGetChildren handle categId
      if p `notElem` children
        then hEditParent handle categId p
        else return $ Left invalidParent
  return (resName >> resParent)
