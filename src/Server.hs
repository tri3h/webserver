{-# LANGUAGE OverloadedStrings #-}
module Server where

import qualified User
import qualified Author
import qualified Tag
import qualified Category
import qualified Post
import qualified Comment
import qualified Draft
import Network.Wai.Handler.Warp
import Network.Wai
import Network.HTTP.Types.Status
import Data.Text.Encoding ( decodeUtf8 )
import Data.Text ( Text )
import Control.Monad
import qualified Data.ByteString as BS
import Data.ByteString.Lazy (toStrict)
import Network.HTTP.Types (queryToQueryText)
import qualified Image

makeAdminResponse :: Request -> IO Response
makeAdminResponse req = do
                let query = queryToQueryText $ queryString req
                case pathInfo req of
                    ["users"] -> case requestMethod req of
                        "DELETE" -> User.deleteUser query
                        _ -> return $ responseLBS status404 [] ""
                    ["authors"] -> case requestMethod req of 
                        "POST" -> Author.create query 
                        "PUT" -> Author.edit query 
                        "GET" -> Author.get query
                        "DELETE" -> Author.delete query 
                        _ -> return $ responseLBS status404 [] ""
                    ["tags"] -> case requestMethod req of 
                        "POST" -> Tag.create query 
                        "PUT" -> Tag.edit query 
                        "DELETE" -> Tag.delete query 
                        _ -> return $ responseLBS status404 [] ""
                    ["categories"] -> case requestMethod req of 
                        "POST" -> Category.create query 
                        "PUT" -> Category.edit query 
                        "DELETE" -> Category.delete query 
                        _ -> return $ responseLBS status404 [] ""
                    ["comments"] -> case requestMethod req of 
                        "DELETE" -> Comment.delete query
                        _ -> return $ responseLBS status404 [] ""
                    _ -> return $ responseLBS status404 [] ""

makeTokenResponse :: Request -> Text -> IO Response
makeTokenResponse req token = do
                    isAdmin <- User.isAdmin token
                    body <- strictRequestBody req
                    let query = queryToQueryText $ queryString req
                    case pathInfo req of
                        ["users"] -> case requestMethod req of
                            "GET" -> User.getUser query token
                            _ -> f isAdmin
                        ["tags"] -> case requestMethod req of 
                            "GET" -> Tag.get query
                            _ -> f isAdmin
                        ["categories"] -> case requestMethod req of 
                            "GET" -> Category.get query
                            _ -> f isAdmin
                        ["posts"] -> case requestMethod req of 
                            "GET" -> Post.get query
                            _ -> f isAdmin
                        ["comments"] -> case requestMethod req of 
                                "GET" -> Comment.get query
                                "POST" -> Comment.create query 
                                _ -> f isAdmin
                        ["drafts"] -> case requestMethod req of 
                            "GET" -> Draft.get query
                            "POST" -> Draft.create query (toStrict body)
                            "PUT" -> Draft.edit query (toStrict body)
                            "DELETE" -> Draft.delete query
                            _ -> return $ responseLBS status404 [] ""
                        ["drafts","minor_photo"] -> case requestMethod req of 
                            "POST" -> Draft.addMinorPhoto query (toStrict body)
                            "DELETE" -> Draft.deleteMinorPhoto query
                            _ -> return $ responseLBS status404 [] ""
                        ["publish"] -> Draft.publish query
                        _ -> f isAdmin
            where f isAdmin = if isAdmin
                                then makeAdminResponse req
                                else return $ responseLBS status404 [] ""

makeNoTokenResponse :: Request -> IO Response
makeNoTokenResponse req = do 
        let query = queryToQueryText $ queryString req
        body <- strictRequestBody req
        case pathInfo req of
            ["users"] -> if requestMethod req == "POST"
                then User.createUser query (toStrict body)
                else return $ responseLBS status400 [] "No token"
            ["tokens"] -> User.getNewToken query
            ["images"] -> case requestMethod req of 
                    "GET" -> Image.get query
                    _ -> return $ responseLBS status404 [] ""
            _ -> return $ responseLBS status404 [] ""

main :: IO ()
main = run 3000 $ \req f -> do
    let query = queryToQueryText $ queryString req
    response <- case join $ lookup "token" query of
        Just t -> do
            isValid <- User.isTokenValid t
            if isValid
                then makeTokenResponse req t
                else return $ responseLBS status400 [] "Invalid token"
        Nothing -> makeNoTokenResponse req
    f response