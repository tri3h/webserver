{-# LANGUAGE OverloadedStrings #-}
module Server where

import qualified User
import Network.Wai.Handler.Warp
import Network.Wai
import Network.HTTP.Types.Status
import Network.HTTP.Types.Header
import Network.HTTP.Types.URI
import Data.Text.Lazy.Encoding ( encodeUtf8 )
import Data.Text.Encoding ( decodeUtf8 )
import qualified Data.Text.Lazy as LazyText
import Data.Text ( Text )
import Control.Monad
import Data.ByteString.Lazy ( append )
import Data.Aeson
import qualified Types.User.FullUser as F
import qualified Types.User.ReceivedUser as R
import qualified Types.User.SentUser as S

createUser :: Query -> IO Response
createUser query = do
    let name = join $ lookup "name" query
    let surname = join $ lookup "surname" query
    let avatar = join $ lookup "avatar" query
    let login = join $ lookup "login" query
    let password = join $ lookup "password" query
    let arr = [name, surname, avatar, login, password]
    if Nothing `notElem` arr
        then do
            let val' = map (decodeUtf8 . (\(Just a) -> a)) arr
            let user = R.ReceivedUser {
                    R.name = head val',
                    R.surname = val' !! 1,
                    R.avatar = val' !! 2,
                    R.login = val' !! 3,
                    R.password = val' !! 4
            }
            result <- User.create user
            case result of
                Left l -> return $ responseLBS status400 [] . encodeUtf8 $ LazyText.fromStrict l
                Right r -> do 
                    let a = "{ \"token\" : \"" `append` encodeUtf8 (LazyText.fromStrict r) `append` "\"}"
                    return $ responseLBS status200 [(hContentType, "application/json")] a
        else return $ responseLBS status400 [] ""


getUser :: Query -> Text -> IO Response
getUser query token = do
            user <- User.get token
            return $ responseLBS status200 [(hContentType, "application/json")] $ encode user

deleteUser :: Query -> IO Response
deleteUser query = do
        let user_id = join $ lookup "user_id" query
        case user_id of
            Just u -> do
                result <- User.delete $ decodeUtf8 u
                case result of
                    Left l -> return $ responseLBS status400 [] . encodeUtf8 $ LazyText.fromStrict l
                    Right r -> return $ responseLBS status204 [] ""
            Nothing -> return $ responseLBS status400 [] ""


makeUserResponse :: Request -> Text -> IO Response
makeUserResponse req token = do
                let query = queryString req
                case pathInfo req of
                    ["users"] -> case requestMethod req of
                        "GET" -> getUser query token
                        _ -> return $ responseLBS status404 [] ""
                    _ -> return $ responseLBS status404 [] ""

makeAdminResponse :: Request -> IO Response
makeAdminResponse req = do
                let query = queryString req
                case pathInfo req of
                    ["users"] -> case requestMethod req of
                        "DELETE" -> deleteUser query
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
                then createUser $ queryString req
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