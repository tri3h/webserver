{-# LANGUAGE OverloadedStrings #-}

module Server where

import qualified Author
import qualified Category
import qualified Comment
import Control.Monad (join)
import qualified Data.ByteString as BS
import Data.ByteString.Lazy (toStrict)
import Data.String (IsString (fromString))
import Data.Text (Text, unpack)
import qualified Draft
import qualified Handlers.Logger as Logger
import qualified Image
import Network.HTTP.Types (queryToQueryText)
import Network.HTTP.Types.Status (status400, status404)
import Network.Wai
  ( Request (pathInfo, queryString, requestMethod),
    Response,
    responseLBS,
    strictRequestBody,
  )
import qualified Network.Wai.Handler.Warp as Warp
import qualified Post
import qualified Tag
import Types.Config (Config (database, server), ServerConfig (sHost, sPort))
import qualified Types.Config as Config
import qualified User

run :: Logger.Handle IO -> Config.Config -> IO ()
run logger config = do
  let port = fromInteger . sPort $ server config
  let host = fromString . unpack . sHost $ server config
  let settings = Warp.setHost host $ Warp.setPort port Warp.defaultSettings
  Warp.runSettings settings $ \req f -> do
    let query = queryToQueryText $ queryString req
    Logger.debug logger $ "Received query: " ++ show query
    body <- toStrict <$> strictRequestBody req
    Logger.debug logger $ "Received body: " ++ show body
    response <- case join $ lookup "token" query of
      Just t -> do
        isValid <- User.isTokenValid (database config) t
        if isValid
          then makeTokenResponse logger config req body t
          else do
            let err = "Invalid token"
            Logger.debug logger $ show err
            return $ responseLBS status400 [] err
      Nothing -> makeNoTokenResponse logger config req body
    f response

makeNoTokenResponse :: Logger.Handle IO -> Config.Config -> Request -> BS.ByteString -> IO Response
makeNoTokenResponse logger config req body = do
  let query = queryToQueryText $ queryString req
  case pathInfo req of
    ["users"] ->
      if requestMethod req == "POST"
        then User.create logger config query body
        else return $ responseLBS status400 [] "No token"
    ["tokens"] -> User.getNewToken logger config query
    ["images"] -> case requestMethod req of
      "GET" -> Image.get logger config query
      _ -> return $ responseLBS status404 [] ""
    _ -> return $ responseLBS status404 [] ""

makeTokenResponse :: Logger.Handle IO -> Config.Config -> Request -> BS.ByteString -> Text -> IO Response
makeTokenResponse logger config req body token = do
  isAdmin <- User.isAdmin (database config) token
  let query = queryToQueryText $ queryString req
  case pathInfo req of
    ["users"] -> case requestMethod req of
      "GET" -> User.get logger config token
      _ -> chooseResponse isAdmin
    ["tags"] -> case requestMethod req of
      "GET" -> Tag.get logger config query
      _ -> chooseResponse isAdmin
    ["categories"] -> case requestMethod req of
      "GET" -> Category.get logger config query
      _ -> chooseResponse isAdmin
    ["posts"] -> case requestMethod req of
      "GET" -> Post.get logger config query
      _ -> chooseResponse isAdmin
    ["comments"] -> case requestMethod req of
      "GET" -> Comment.get logger config query
      "POST" -> Comment.create logger config query
      _ -> chooseResponse isAdmin
    ["drafts"] -> case requestMethod req of
      "GET" -> Draft.get logger config query
      "POST" -> Draft.create logger config query body token
      "PUT" -> Draft.edit logger config query body
      "DELETE" -> Draft.delete logger config query
      _ -> return $ responseLBS status404 [] ""
    ["drafts", "minor_photo"] -> case requestMethod req of
      "POST" -> Draft.addMinorPhoto logger config query body
      "DELETE" -> Draft.deleteMinorPhoto logger config query
      _ -> return $ responseLBS status404 [] ""
    ["publish"] -> Draft.publish logger config query
    _ -> chooseResponse isAdmin
  where
    chooseResponse isAdmin =
      if isAdmin
        then makeAdminResponse logger config req
        else return $ responseLBS status404 [] ""

makeAdminResponse :: Logger.Handle IO -> Config.Config -> Request -> IO Response
makeAdminResponse logger config req = do
  let query = queryToQueryText $ queryString req
  case pathInfo req of
    ["users"] -> case requestMethod req of
      "DELETE" -> User.delete logger config query
      _ -> return $ responseLBS status404 [] ""
    ["authors"] -> case requestMethod req of
      "POST" -> Author.create logger config query
      "PUT" -> Author.edit logger config query
      "GET" -> Author.get logger config query
      "DELETE" -> Author.delete logger config query
      _ -> return $ responseLBS status404 [] ""
    ["tags"] -> case requestMethod req of
      "POST" -> Tag.create logger config query
      "PUT" -> Tag.edit logger config query
      "DELETE" -> Tag.delete logger config query
      _ -> return $ responseLBS status404 [] ""
    ["categories"] -> case requestMethod req of
      "POST" -> Category.create logger config query
      "PUT" -> Category.edit logger config query
      "DELETE" -> Category.delete logger config query
      _ -> return $ responseLBS status404 [] ""
    ["comments"] -> case requestMethod req of
      "DELETE" -> Comment.delete logger config query
      _ -> return $ responseLBS status404 [] ""
    _ -> return $ responseLBS status404 [] ""
