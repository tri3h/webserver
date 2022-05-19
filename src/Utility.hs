{-# LANGUAGE OverloadedStrings #-}

module Utility where

import Control.Monad (join)
import Data.Aeson (ToJSON, encode)
import qualified Data.ByteString as BS
import Data.Text (Text, append, null, pack, splitOn, unpack)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Error (Error, noImage, noSpecified)
import Network.HTTP.Types (hContentType, status200, status201, status204, status400, status404)
import Network.HTTP.Types.URI (QueryText)
import Network.Wai (Response, responseLBS)
import Text.Read (readMaybe)
import Types.Config (ServerAddress)
import Types.Image (Image (Image), ImageBody, ImageId (ImageId), ImageType, Link (Link), imageAddress)
import Prelude hiding (null)

getText :: QueryText -> Text -> Either Error Text
getText query name = case join $ lookup name query of
  Nothing -> Left $ noSpecified name
  Just x ->
    if null x
      then Left $ noSpecified name
      else Right x

getInteger :: QueryText -> Text -> Either Error Integer
getInteger query name = case join $ lookup name query of
  Nothing -> Left $ noSpecified name
  Just x -> maybe (Left $ noSpecified name) Right (readMaybe $ unpack x :: Maybe Integer)

getIntegers :: QueryText -> Text -> Either Error [Integer]
getIntegers query name = case join $ lookup name query of
  Nothing -> Left $ noSpecified name
  Just xs ->
    let values = map (\x -> readMaybe $ unpack x :: Maybe Integer) (filter (not . null) $ splitOn "," xs)
     in if Nothing `elem` values
          then Left $ noSpecified name
          else Right $ map (\(Just x) -> x) values

getImage :: BS.ByteString -> BS.ByteString -> Either Error Image
getImage body name =
  let hasImage = "Content-Type: image/" `BS.isInfixOf` body
      isCorrectName = ("name=\"" `BS.append` name) `BS.isInfixOf` body
   in if hasImage && isCorrectName
        then
          let imageType = getImageType body
           in Right $ Image (getImageBody body imageType) imageType
        else Left noImage

getImageBody :: BS.ByteString -> ImageType -> ImageBody
getImageBody body t =
  let delim1 = encodeUtf8 t `BS.append` "\r"
      delim2 = "\r\n-"
   in BS.drop (BS.length delim2) . fst
        . BS.breakSubstring delim2
        . BS.drop (BS.length delim1)
        . snd
        $ BS.breakSubstring delim1 body

getImageType :: BS.ByteString -> ImageType
getImageType =
  let delim1 = "Content-Type: image/"
      delim2 = "\r"
   in decodeUtf8 . fst
        . BS.breakSubstring delim2
        . BS.drop (BS.length delim1)
        . snd
        . BS.breakSubstring delim1

getMaybeText :: QueryText -> Text -> Maybe Text
getMaybeText query name = eitherToMaybe $ getText query name

getMaybeInteger :: QueryText -> Text -> Maybe Integer
getMaybeInteger query name = eitherToMaybe $ getInteger query name

getMaybeIntegers :: QueryText -> Text -> [Integer]
getMaybeIntegers query name =
  case getIntegers query name of
    Left _ -> []
    Right r -> r

getMaybeImage :: BS.ByteString -> BS.ByteString -> Maybe Image
getMaybeImage body name = eitherToMaybe $ getImage body name

imageIdToLink :: ServerAddress -> ImageId -> Link
imageIdToLink server (ImageId x) = Link $ server `append` imageAddress `append` pack (show x)

eitherToMaybe :: Either a1 a2 -> Maybe a2
eitherToMaybe (Right r) = Just r
eitherToMaybe (Left _) = Nothing

response400 :: Error -> Response
response400 e = responseLBS status400 [] $ encode e

response404 :: Response
response404 = responseLBS status404 [] ""

response200JSON :: ToJSON a => a -> Response
response200JSON x = responseLBS status200 [(hContentType, "application/json")] $ encode x

response201 :: Response
response201 = responseLBS status201 [] ""

response204 :: Response
response204 = responseLBS status204 [] ""
