{-# LANGUAGE OverloadedStrings #-}
module Server where

import qualified User
import qualified Author
import qualified Tag
import qualified Category
import qualified Post
import qualified Comment
import Network.Wai.Handler.Warp
import Network.Wai
import Network.HTTP.Types.Status
import Data.Text.Encoding ( decodeUtf8 )
import Data.Text ( Text )
import Control.Monad
import qualified Data.ByteString as BS


makeAdminResponse :: Request -> Text -> IO Response
makeAdminResponse req token = do
                let query = queryString req
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
                    let query = queryString req
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
                        ["pictures"] -> case requestMethod req of 
                                "GET" -> User.getAvatar $ queryString req
                                _ -> return $ responseLBS status404 [] ""
                        ["comments"] -> case requestMethod req of 
                                "GET" -> Comment.get query
                                "POST" -> Comment.create query 
                                _ -> f isAdmin
                        _ -> f isAdmin
            where f isAdmin = if isAdmin
                                    then makeAdminResponse req token
                                    else return $ responseLBS status404 [] ""

makeNoTokenResponse :: Request -> IO Response
makeNoTokenResponse req = case pathInfo req of
            ["users"] -> if requestMethod req == "POST"
                then do 
                    body <- getFullBody req BS.empty
                    User.createUser (queryString req) body
                else return $ responseLBS status400 [] "No token"
            ["tokens"] -> User.getNewToken $ queryString req
            _ -> return $ responseLBS status404 [] ""

getFullBody :: Request -> BS.ByteString -> IO BS.ByteString
getFullBody req body = do 
    b <- getRequestBodyChunk req
    if BS.null b 
        then return body 
        else getFullBody req (body `BS.append` b)

main :: IO ()
main = run 3000 $ \req f -> do
    let query = queryString req
    response <- case join $ lookup "token" query of
        Just t -> do
            isValid <- User.isTokenValid $ decodeUtf8 t
            if isValid
                then makeTokenResponse req $ decodeUtf8 t
                else return $ responseLBS status400 [] "Invalid token"
        Nothing -> makeNoTokenResponse req
    f response