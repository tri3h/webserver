{-# LANGUAGE OverloadedStrings #-}

module Handlers.Server where

import qualified Data.ByteString as BS
import Error (invalidToken)
import qualified Handlers.Author as Author
import qualified Handlers.Category as Category
import qualified Handlers.Comment as Comment
import qualified Handlers.Draft as Draft
import qualified Handlers.Image as Image
import qualified Handlers.Logger as Logger
import qualified Handlers.Post as Post
import qualified Handlers.Tag as Tag
import qualified Handlers.User as User
import Network.HTTP.Types (queryToQueryText)
import Network.Wai (Request (pathInfo, queryString, requestMethod), Response)
import Types.Config (ServerAddress)
import Types.User (Token)
import Utility (response400, response404)

data Handle m = Handle
  { hUser :: User.Handle m,
    hAuthor :: Author.Handle m,
    hCategory :: Category.Handle m,
    hComment :: Comment.Handle m,
    hDraft :: Draft.Handle m,
    hImage :: Image.Handle m,
    hPost :: Post.Handle m,
    hTag :: Tag.Handle m
  }

makeNoTokenResponse :: Monad m => Handle m -> Logger.Handle m -> ServerAddress -> Request -> BS.ByteString -> m Response
makeNoTokenResponse handle logger server req body = do
  let query = queryToQueryText $ queryString req
  let user = hUser handle
  case pathInfo req of
    ["users"] -> case requestMethod req of
      "POST" -> User.create user logger query body
      _ -> chooseResponse query
    ["tokens"] -> User.getNewToken user logger query
    ["images"] -> case requestMethod req of
      "GET" -> Image.get (hImage handle) logger query
      _ -> chooseResponse query
    ["posts"] -> case requestMethod req of
      "GET" -> Post.get (hPost handle) logger server query
      _ -> chooseResponse query
    ["tags"] -> case requestMethod req of
      "GET" -> Tag.get (hTag handle) logger query
      _ -> chooseResponse query
    ["comments"] -> case requestMethod req of
      "GET" -> Comment.get (hComment handle) logger query
      _ -> chooseResponse query
    ["categories"] -> case requestMethod req of
      "GET" -> Category.get (hCategory handle) logger query
      _ -> chooseResponse query
    _ -> chooseResponse query
  where
    chooseResponse query = case User.getToken query of
      Just token -> do
        isValid <- User.hIsTokenValid (hUser handle) token
        if isValid
          then makeTokenResponse handle logger server req body token
          else do
            Logger.debug logger $ show invalidToken
            return $ response400 invalidToken
      Nothing -> return response404

makeTokenResponse :: Monad m => Handle m -> Logger.Handle m -> ServerAddress -> Request -> BS.ByteString -> Token -> m Response
makeTokenResponse handle logger server req body token = do
  let user = hUser handle
  isAdmin <- User.hIsAdmin user token
  let query = queryToQueryText $ queryString req
  let draft = hDraft handle
  case pathInfo req of
    ["users"] -> case requestMethod req of
      "GET" -> User.get user logger server token
      _ -> chooseResponse isAdmin
    ["users", "avatar"] -> case requestMethod req of
      "POST" -> User.addAvatar user token body
      _ -> return response404
    ["comments"] -> case requestMethod req of
      "POST" -> Comment.create (hComment handle) logger query token
      _ -> chooseResponse isAdmin
    ["drafts"] -> case requestMethod req of
      "GET" -> Draft.get draft logger server query
      "POST" -> Draft.create draft logger query body token
      "PUT" -> Draft.edit draft logger query body
      "DELETE" -> Draft.delete draft logger query
      _ -> return response404
    ["drafts", "id"] -> case requestMethod req of
      "GET" -> Draft.getAllByAuthor draft logger token query
      _ -> return response404
    ["drafts", "minor_photo"] -> case requestMethod req of
      "POST" -> Draft.addMinorPhoto draft logger query body
      "DELETE" -> Draft.deleteMinorPhoto draft logger query
      _ -> return response404
    ["publish"] -> Draft.publish draft logger query
    _ -> chooseResponse isAdmin
  where
    chooseResponse isAdmin =
      if isAdmin
        then makeAdminResponse handle logger req
        else return response404

makeAdminResponse :: Monad m => Handle m -> Logger.Handle m -> Request -> m Response
makeAdminResponse handle logger req = do
  let query = queryToQueryText $ queryString req
  let author = hAuthor handle
  let tag = hTag handle
  let category = hCategory handle
  case pathInfo req of
    ["users"] -> case requestMethod req of
      "DELETE" -> User.delete (hUser handle) logger query
      _ -> return response404
    ["authors"] -> case requestMethod req of
      "POST" -> Author.create author logger query
      "PUT" -> Author.edit author logger query
      "GET" -> Author.get author logger query
      "DELETE" -> Author.delete author logger query
      _ -> return response404
    ["tags"] -> case requestMethod req of
      "POST" -> Tag.create tag logger query
      "PUT" -> Tag.edit tag logger query
      "DELETE" -> Tag.delete tag logger query
      _ -> return response404
    ["categories"] -> case requestMethod req of
      "POST" -> Category.create category logger query
      "PUT" -> Category.edit category logger query
      "DELETE" -> Category.delete category logger query
      _ -> return response404
    ["comments"] -> case requestMethod req of
      "DELETE" -> Comment.delete (hComment handle) logger query
      _ -> return response404
    _ -> return response404
