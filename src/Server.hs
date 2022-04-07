{-# LANGUAGE OverloadedStrings #-}
module Server where

import qualified User
import qualified Author
import qualified Tag
import qualified Category
import qualified Post
import qualified Comment
import qualified Draft
import Network.Wai.Handler.Warp ( run )
import Network.Wai
    ( Response,
      responseLBS,
      strictRequestBody,
      Request(pathInfo, requestMethod, queryString) )
import Network.HTTP.Types.Status ( status400, status404 )
import Data.Text.Encoding ( decodeUtf8 )
import Data.Text ( Text )
import Control.Monad ( join )
import qualified Data.ByteString as BS
import Data.ByteString.Lazy (toStrict, fromStrict)
import Network.HTTP.Types (queryToQueryText, QueryText)
import qualified Image
import qualified Handlers.Logger

main :: IO ()
main = run 3000 $ \req f -> do
    let query = queryToQueryText $ queryString req
    Handlers.Logger.debug logger $ "Received query: " ++ show query
    body <- toStrict <$> strictRequestBody req
    Handlers.Logger.debug logger $ "Received body: " ++ show body
    response <- case join $ lookup "token" query of
        Just t -> do
            isValid <- User.isTokenValid t
            if isValid
            then makeTokenResponse req body t
            else do 
                let err = "Invalid token"
                Handlers.Logger.debug logger $ show err
                return $ responseLBS status400 [] err
        Nothing -> makeNoTokenResponse req body
    f response

makeNoTokenResponse :: Request -> BS.ByteString -> IO Response
makeNoTokenResponse req body = do 
    let query = queryToQueryText $ queryString req
    case pathInfo req of
        ["users"] -> if requestMethod req == "POST"
            then User.create logger query body
            else return $ responseLBS status400 [] "No token"
        ["tokens"] -> User.getNewToken logger query
        ["images"] -> case requestMethod req of 
            "GET" -> Image.get logger query
            _ -> return $ responseLBS status404 [] ""
        _ -> return $ responseLBS status404 [] ""

makeTokenResponse :: Request -> BS.ByteString -> Text -> IO Response
makeTokenResponse req body token = do
    isAdmin <- User.isAdmin token
    let query = queryToQueryText $ queryString req
    case pathInfo req of
        ["users"] -> case requestMethod req of
            "GET" -> User.get logger query token
            _ -> chooseResponse isAdmin
        ["tags"] -> case requestMethod req of 
            "GET" -> Tag.get logger query
            _ -> chooseResponse isAdmin
        ["categories"] -> case requestMethod req of 
            "GET" -> Category.get logger query
            _ -> chooseResponse isAdmin
        ["posts"] -> case requestMethod req of 
            "GET" -> Post.get logger query
            _ -> chooseResponse isAdmin
        ["comments"] -> case requestMethod req of 
            "GET" -> Comment.get logger query
            "POST" -> Comment.create logger query 
            _ -> chooseResponse isAdmin
        ["drafts"] -> case requestMethod req of 
            "GET" -> Draft.get logger query
            "POST" -> Draft.create logger query body token
            "PUT" -> Draft.edit logger query body
            "DELETE" -> Draft.delete logger query
            _ -> return $ responseLBS status404 [] ""
        ["drafts","minor_photo"] -> case requestMethod req of 
            "POST" -> Draft.addMinorPhoto logger query body
            "DELETE" -> Draft.deleteMinorPhoto logger query
            _ -> return $ responseLBS status404 [] ""
        ["publish"] -> Draft.publish logger query
        _ -> chooseResponse isAdmin
        where chooseResponse isAdmin = if isAdmin
                then makeAdminResponse req
                else return $ responseLBS status404 [] ""

makeAdminResponse :: Request -> IO Response
makeAdminResponse req = do
    let query = queryToQueryText $ queryString req
    case pathInfo req of
        ["users"] -> case requestMethod req of
            "DELETE" -> User.delete logger query
            _ -> return $ responseLBS status404 [] ""
        ["authors"] -> case requestMethod req of 
            "POST" -> Author.create logger query
            "PUT" -> Author.edit logger query 
            "GET" -> Author.get logger query
            "DELETE" -> Author.delete logger query 
            _ -> return $ responseLBS status404 [] ""
        ["tags"] -> case requestMethod req of 
            "POST" -> Tag.create logger query 
            "PUT" -> Tag.edit logger query 
            "DELETE" -> Tag.delete logger query 
            _ -> return $ responseLBS status404 [] ""
        ["categories"] -> case requestMethod req of 
            "POST" -> Category.create logger query 
            "PUT" -> Category.edit logger query 
            "DELETE" -> Category.delete logger query 
            _ -> return $ responseLBS status404 [] ""
        ["comments"] -> case requestMethod req of 
            "DELETE" -> Comment.delete logger query
            _ -> return $ responseLBS status404 [] ""
        _ -> return $ responseLBS status404 [] ""

logger :: Handlers.Logger.Handle IO
logger = Handlers.Logger.Handle { 
    Handlers.Logger.hWriteLog = putStrLn,
    Handlers.Logger.hVerbosity = Handlers.Logger.DEBUG
} 