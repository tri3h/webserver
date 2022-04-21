{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Comment where

import Data.Aeson (encode)
import qualified Data.Text.Lazy as LazyText
import Data.Text.Lazy.Encoding (encodeUtf8)
import Database.Connection (manage)
import qualified Database.Queries.Comment as Db
import qualified Database.Queries.Post as Db.Post
import qualified Handlers.Comment as Handler
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
import Types.Comment
  ( Comment (CommentToCreate, postId, text, userId),
  )
import Types.Config (Config (database))
import Utility (getInteger, getText)

create :: Logger.Handle IO -> Config -> QueryText -> IO Response
create logger config query = do
  let info = do
        postId <- getInteger query "post_id"
        userId <- getInteger query "user_id"
        text <- getText query "text"
        Right CommentToCreate {..}
  Logger.debug logger $ "Tried to parse query and got: " ++ show info
  case info of
    Right comment -> do
      result <- Handler.create (handle config) comment
      Logger.debug logger $ "Tried to create comment and got: " ++ show result
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

get :: Logger.Handle IO -> Config -> QueryText -> IO Response
get logger config query = do
  let info = getInteger query "post_id"
  Logger.debug logger $ "Tried to parse query and got: " ++ show info
  case info of
    Right postId -> do
      result <- Handler.get (handle config) postId
      Logger.debug logger $ "Tried to get comment and got: " ++ show result
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

delete :: Logger.Handle IO -> Config -> QueryText -> IO Response
delete logger config query = do
  let info = getInteger query "comment_id"
  Logger.debug logger $ "Tried to parse query and got: " ++ show info
  case info of
    Right commentId -> do
      result <- Handler.delete (handle config) commentId
      Logger.debug logger $ "Tried to delete comment and got: " ++ show result
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
        { Handler.hGet = manage db . Db.get,
          Handler.hCreate = manage db . Db.create,
          Handler.hDelete = manage db . Db.delete,
          Handler.hDoesPostExist = manage db . Db.Post.doesExist,
          Handler.hDoesExist = manage db . Db.doesExist
        }
