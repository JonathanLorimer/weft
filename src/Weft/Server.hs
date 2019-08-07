{-# LANGUAGE CPP                  #-}

module Weft.Server where

import Weft.Internal.Types
import Weft.Types
import Weft.Generics.QueryParser
import Weft.Generics.Resolve
import GHC.Generics
import Lens.Micro
import Lens.Micro.Aeson
import Network.Wai.Middleware.Cors
import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Types (status200, status500)
import Network.HTTP.Types.Header (hContentType)
import Network.HTTP.Types.Method
import Data.Aeson hiding (json)
import Data.Attoparsec.ByteString.Char8
import Data.ByteString.Char8
import Data.Text.Encoding
import Data.Monoid
import qualified Data.ByteString.Lazy as BL
import Control.Monad.Reader

newtype DataResponse a = DataResponse { _data :: a } 
  deriving (Generic)

instance (Generic a, ToJSON a, GToJSON Zero (Rep a)) => ToJSON (DataResponse a) where
  toJSON d = genericToJSON (defaultOptions { fieldLabelModifier = Prelude.drop 1 }) d

parseReqBody :: (Wefty query)
             => ByteString
             -> Either String ((Gql query m s) 'Query)
parseReqBody reqBody = parseOnly
                       (runReaderT (queryParser) mempty)
                       reqBody

maybeQuery :: ByteString -> Maybe ByteString
maybeQuery rb = do
  json <- decode @Value $ BL.fromStrict rb
  encodeUtf8 <$> ((^? key "query" . _String) json)

note :: Maybe a -> Either String a
note Nothing = Left ""
note (Just x) = Right x

app :: (ToJSON (q 'Response), Wefty q) => Gql q m s 'Resolver -> Application
app resolver req f = do
#if MIN_VERSION_wai(3,2,2)
  rb <- getRequestBodyChunk req
#else
  rb <- requestBody req
#endif
  let _eitherQuery = do
    textQuery <- note . maybeQuery $ rb
    parseReqBody textQuery
  f $ case _eitherQuery of
    Right query' -> do
      res <- resolve resolver query'
      case res of
        Gql q -> successResponse $ DataResponse q
    Left e  -> errorResponse $ BL.fromStrict $ pack e

successResponse :: ToJSON a => a -> Response
successResponse = responseLBS status200 [(hContentType, "application/json")] . encode

errorResponse :: BL.ByteString -> Response
errorResponse = responseLBS status500 [(hContentType, "application/json")]

server :: (Wefty q)
       => [Settings -> Settings]
       -> Gql q m s 'Resolver
       -> IO ()
server s r = runSettings
  (appEndo (foldMap Endo s) defaultSettings)
  (cors extremelyPermissiveCorsPolicy $ app r)

-- TODO(Jonathan): At some point you should make this less permissive
extremelyPermissiveCorsPolicy :: Request -> Maybe CorsResourcePolicy
extremelyPermissiveCorsPolicy _ = Just $ CorsResourcePolicy
  { corsOrigins           = Nothing
  , corsMethods           = [methodGet, methodPost, methodOptions]
  , corsRequestHeaders    = [hContentType]
  , corsExposedHeaders    = Nothing
  , corsMaxAge            = Nothing
  , corsVaryOrigin        = False
  , corsRequireOrigin     = False
  , corsIgnoreFailures    = True }
