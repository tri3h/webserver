{-# LANGUAGE OverloadedStrings #-}

module Server where

import qualified Author
import qualified Category
import qualified Comment
import qualified Data.ByteString as BS
import Data.ByteString.Lazy (toStrict)
import Data.Pool (Pool)
import Data.String (IsString (fromString))
import Data.Text (unpack)
import Database.PostgreSQL.Simple (Connection)
import qualified Draft
import Error (invalidToken)
import qualified Handlers.Logger as Logger
import qualified Image
import Network.HTTP.Types (queryToQueryText)
import Network.Wai
  ( Request (pathInfo, queryString, requestMethod),
    Response,
    strictRequestBody,
  )
import qualified Network.Wai.Handler.Warp as Warp
import qualified Post
import qualified Tag
import Types.Config (Config (server), ServerConfig (sAddress, sHost, sPort))
import qualified Types.Config as Config
import Types.User (Token)
import qualified User
import Utility (response400, response404)

run :: Logger.Handle IO -> Config.Config -> Pool Connection -> IO ()
run logger config pool = do
  let port = fromInteger . sPort $ server config
  let host = fromString . unpack . sHost $ server config
  let onExp _ e = Logger.error logger $ show e
  let settings = Warp.setOnException onExp $ Warp.setHost host $ Warp.setPort port Warp.defaultSettings
  Logger.info logger "Starting server"
  Warp.runSettings settings $ \req f -> do
    let query = queryToQueryText $ queryString req
    Logger.debug logger $ "Received query: " ++ show query
    body <- toStrict <$> strictRequestBody req
    Logger.debug logger $ "Received body: " ++ show body
    response <- makeNoTokenResponse logger config pool req body
    f response

makeNoTokenResponse :: Logger.Handle IO -> Config.Config -> Pool Connection -> Request -> BS.ByteString -> IO Response
makeNoTokenResponse logger config pool req body = do
  let query = queryToQueryText $ queryString req
  let address = sAddress $ server config
  case pathInfo req of
    ["users"] -> case requestMethod req of
      "POST" -> User.create logger pool query body
      _ -> chooseResponse query
    ["tokens"] -> User.getNewToken logger pool query
    ["images"] -> case requestMethod req of
      "GET" -> Image.get logger pool query
      _ -> chooseResponse query
    ["posts"] -> case requestMethod req of
      "GET" -> Post.get logger pool address query
      _ -> chooseResponse query
    ["tags"] -> case requestMethod req of
      "GET" -> Tag.get logger pool query
      _ -> chooseResponse query
    ["categories"] -> case requestMethod req of
      "GET" -> Category.get logger pool query
      _ -> chooseResponse query
    _ -> chooseResponse query
  where
    chooseResponse query = case User.getToken query of
      Just token -> do
        isValid <- User.isTokenValid pool token
        if isValid
          then makeTokenResponse logger config pool req body token
          else do
            Logger.debug logger $ show invalidToken
            return $ response400 invalidToken
      Nothing -> return response404

makeTokenResponse :: Logger.Handle IO -> Config.Config -> Pool Connection -> Request -> BS.ByteString -> Token -> IO Response
makeTokenResponse logger config pool req body token = do
  isAdmin <- User.isAdmin pool token
  let query = queryToQueryText $ queryString req
  let address = sAddress $ server config
  case pathInfo req of
    ["users"] -> case requestMethod req of
      "GET" -> User.get logger pool address token
      _ -> chooseResponse isAdmin
    ["users", "avatar"] -> case requestMethod req of
      "POST" -> User.addAvatar pool token body
      _ -> return response404
    ["comments"] -> case requestMethod req of
      "GET" -> Comment.get logger pool query
      "POST" -> Comment.create logger pool query token
      _ -> chooseResponse isAdmin
    ["drafts"] -> case requestMethod req of
      "GET" -> Draft.get logger pool address query
      "POST" -> Draft.create logger pool query body token
      "PUT" -> Draft.edit logger pool query body
      "DELETE" -> Draft.delete logger pool query
      _ -> return response404
    ["drafts", "id"] -> case requestMethod req of
      "GET" -> Draft.getAllByAuthor logger pool token query
      _ -> return response404
    ["drafts", "minor_photo"] -> case requestMethod req of
      "POST" -> Draft.addMinorPhoto logger pool query body
      "DELETE" -> Draft.deleteMinorPhoto logger pool query
      _ -> return response404
    ["publish"] -> Draft.publish logger pool query
    _ -> chooseResponse isAdmin
  where
    chooseResponse isAdmin =
      if isAdmin
        then makeAdminResponse logger pool req
        else return response404

makeAdminResponse :: Logger.Handle IO -> Pool Connection -> Request -> IO Response
makeAdminResponse logger pool req = do
  let query = queryToQueryText $ queryString req
  case pathInfo req of
    ["users"] -> case requestMethod req of
      "DELETE" -> User.delete logger pool query
      _ -> return response404
    ["authors"] -> case requestMethod req of
      "POST" -> Author.create logger pool query
      "PUT" -> Author.edit logger pool query
      "GET" -> Author.get logger pool query
      "DELETE" -> Author.delete logger pool query
      _ -> return response404
    ["tags"] -> case requestMethod req of
      "POST" -> Tag.create logger pool query
      "PUT" -> Tag.edit logger pool query
      "DELETE" -> Tag.delete logger pool query
      _ -> return response404
    ["categories"] -> case requestMethod req of
      "POST" -> Category.create logger pool query
      "PUT" -> Category.edit logger pool query
      "DELETE" -> Category.delete logger pool query
      _ -> return response404
    ["comments"] -> case requestMethod req of
      "DELETE" -> Comment.delete logger pool query
      _ -> return response404
    _ -> return response404
