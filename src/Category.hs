{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Category where

import Data.Aeson (encode)
import qualified Data.Text.Lazy as LazyText
import Data.Text.Lazy.Encoding (encodeUtf8)
import Database.Connection (manage)
import qualified Database.Queries.Category as Db
import qualified Handlers.Category as Handler
import qualified Handlers.Logger as Logger
import Network.HTTP.Types.Header (hContentType)
import Network.HTTP.Types.Status
  ( status200,
    status201,
    status204,
    status400,
  )
import Network.HTTP.Types.URI (QueryText)
import Network.Wai (Response, responseLBS)
import Types.Category
  ( Category
      ( CategoryToCreate,
        name,
        parentId
      ),
  )
import Types.Config (Config (database))
import Utility (getInteger, getMaybeInteger, getMaybeText, getText)

create :: Logger.Handle IO -> Config -> QueryText -> IO Response
create logger config query = do
  let info = getText query "name"
  Logger.debug logger $ "Tried to parse query and got: " ++ show info
  case info of
    Right name -> do
      let category =
            CategoryToCreate
              { parentId = getMaybeInteger query "parent_id",
                ..
              }
      result <- Handler.create (handle config) category
      Logger.debug logger $ "Tried to create category and got: " ++ show result
      case result of
        Left l ->
          return $
            responseLBS
              status400
              []
              . encodeUtf8
              $ LazyText.fromStrict l
        Right _ -> return $ responseLBS status201 [] ""
    Left l -> return $ responseLBS status400 [] . encodeUtf8 $ LazyText.fromStrict l

get :: Logger.Handle IO -> Config -> QueryText -> IO Response
get logger config query = do
  let info = getInteger query "category_id"
  Logger.debug logger $ "Tried to parse query and got: " ++ show info
  case info of
    Right categoryId -> do
      result <- Handler.get (handle config) categoryId
      Logger.debug logger $ "Tried to get category and got: " ++ show result
      case result of
        Right r ->
          return $
            responseLBS
              status200
              [(hContentType, "application/json")]
              $ encode r
        Left l ->
          return $
            responseLBS
              status400
              []
              . encodeUtf8
              $ LazyText.fromStrict l
    Left l -> return $ responseLBS status400 [] . encodeUtf8 $ LazyText.fromStrict l

edit :: Logger.Handle IO -> Config -> QueryText -> IO Response
edit logger config query = do
  let info = getInteger query "category_id"
  Logger.debug logger $ "Tried to parse query and got: " ++ show info
  case info of
    Right categoryId -> do
      let parentId = getMaybeInteger query "parent_id"
      let name = getMaybeText query "name"
      result <- Handler.edit (handle config) categoryId name parentId
      Logger.debug logger $ "Tried to edit category and got: " ++ show result
      case result of
        Right _ -> return $ responseLBS status201 [] ""
        Left l ->
          return $
            responseLBS
              status400
              []
              . encodeUtf8
              $ LazyText.fromStrict l
    Left l -> return $ responseLBS status400 [] . encodeUtf8 $ LazyText.fromStrict l

delete :: Logger.Handle IO -> Config -> QueryText -> IO Response
delete logger config query = do
  let info = getInteger query "category_id"
  Logger.debug logger $ "Tried to parse query and got: " ++ show info
  case info of
    Right categoryId -> do
      result <- Handler.delete (handle config) categoryId
      Logger.debug logger $ "Tried to delete category and got: " ++ show result
      case result of
        Right _ -> return $ responseLBS status204 [] ""
        Left l ->
          return $
            responseLBS
              status400
              []
              . encodeUtf8
              $ LazyText.fromStrict l
    Left l -> return $ responseLBS status400 [] . encodeUtf8 $ LazyText.fromStrict l

handle :: Config -> Handler.Handle IO
handle config =
  let db = database config
   in Handler.Handle
        { Handler.hCreate = manage db . Db.create,
          Handler.hGet = manage db . Db.get,
          Handler.hDelete = manage db . Db.delete,
          Handler.hEditName = \a b -> manage db $ Db.editName a b,
          Handler.hEditParent = \a b -> manage db $ Db.editParent a b,
          Handler.hDoesExist = manage db . Db.doesExist,
          Handler.hGetChildren = manage db . Db.getChildren
        }
