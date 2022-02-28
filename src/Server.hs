{-# LANGUAGE OverloadedStrings #-}
module Server where

import qualified User
import Network.Wai.Handler.Warp
import Network.Wai
import Network.HTTP.Types.Status
import Data.Text.Encoding ( decodeUtf8 )
import Data.Text ( Text )
import Control.Monad

makeUserResponse :: Request -> Text -> IO Response
makeUserResponse req token = do
                let query = queryString req
                case pathInfo req of
                    ["users"] -> case requestMethod req of
                        "GET" -> User.getUser query token
                        _ -> return $ responseLBS status404 [] ""
                    _ -> return $ responseLBS status404 [] ""

makeAdminResponse :: Request -> IO Response
makeAdminResponse req = do
                let query = queryString req
                case pathInfo req of
                    ["users"] -> case requestMethod req of
                        "DELETE" -> User.deleteUser query
                        _ -> return $ responseLBS status404 [] ""
                    _ -> return $ responseLBS status404 [] ""

makeTokenResponse :: Request -> Text -> IO Response
makeTokenResponse req token = do
                                isAdmin <- User.isAdmin token
                                if isAdmin
                                    then makeAdminResponse req
                                    else makeUserResponse req token

makeNoTokenResponse :: Request -> IO Response
makeNoTokenResponse req = case pathInfo req of
            ["users"] -> if requestMethod req == "POST"
                then User.createUser $ queryString req
                else return $ responseLBS status400 [] "No token"
            ["auth"] -> undefined
            _ -> return $ responseLBS status404 [] ""

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