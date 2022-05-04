module Handlers.Category where

import Data.Text (Text)
import Types.Category
  ( CategoryId,
    CreateCategory (cParentId),
    GetCategory,
    Name,
    ParentId,
    invalidParent,
    noDeleteHasChildren,
  )

data Handle m = Handle
  { hCreate :: CreateCategory -> m (),
    hGet :: CategoryId -> m GetCategory,
    hGetAll :: m [GetCategory],
    hDelete :: CategoryId -> m (),
    hEditName :: CategoryId -> Name -> m (),
    hEditParent :: CategoryId -> ParentId -> m (),
    hDoesExist :: CategoryId -> m (Either Text ()),
    hGetChildren :: CategoryId -> m [CategoryId]
  }

create :: Monad m => Handle m -> CreateCategory -> m (Either Text ())
create handle categ = do
  correct <- isParentCorrect
  if correct
    then Right <$> hCreate handle categ
    else return $ Left invalidParent
  where
    isParentCorrect =
      case cParentId categ of
        Nothing -> return True
        Just b -> do
          exist <- hDoesExist handle b
          return $ case exist of
            Right () -> True
            _ -> False

get :: Monad m => Handle m -> CategoryId -> m (Either Text GetCategory)
get handle categId = do
  exist <- hDoesExist handle categId
  case exist of
    Right _ -> Right <$> hGet handle categId
    Left l -> return $ Left l

delete :: Monad m => Handle m -> CategoryId -> m (Either Text ())
delete handle categId = do
  exist <- hDoesExist handle categId
  case exist of
    Right _ -> do
      children <- hGetChildren handle categId
      if null children
        then Right <$> hDelete handle categId
        else return $ Left noDeleteHasChildren
    Left l -> return $ Left l

edit :: Monad m => Handle m -> CategoryId -> Maybe Name -> Maybe ParentId -> m (Either Text ())
edit handle categId name parId = do
  doesExist <- hDoesExist handle categId
  case doesExist of
    Right _ ->
      case parId of
        Nothing -> editName
        Just p -> editParent p >> editName
    Left l -> return $ Left l
  where
    editName = case name of
      Nothing -> return $ Right ()
      Just n -> Right <$> hEditName handle categId n
    editParent p = do
      existParent <- hDoesExist handle p
      let doesExist = case existParent of Right _ -> True; Left _ -> False
      children <- hGetChildren handle categId
      let isParentCorrect = doesExist && (p `notElem` children)
      if isParentCorrect
        then Right <$> hEditParent handle categId p
        else return $ Left invalidParent
