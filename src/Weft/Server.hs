{-# LANGUAGE CPP #-}

module Weft.Server where

import Data.Bifunctor
import Text.Megaparsec
import Weft.Internal.Types
import Weft.Types
import Weft.Generics.QueryParser
import Weft.Generics.Resolve
import Weft.Generics.JSONResponse
import Lens.Micro
import Lens.Micro.Aeson
import Network.Wai.Middleware.Cors
import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Types (status200, status500)
import Network.HTTP.Types.Header (hContentType)
import Network.HTTP.Types.Method
import Data.Aeson hiding (json)
import qualified Data.ByteString.Char8 as C8
import Data.Monoid
import Data.Maybe
import qualified Data.ByteString.Lazy as BL
import Control.Monad.Reader
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as M


parseReqBody :: (Wefty query)
             => ClientRequest
             -> Either String ((Gql query m s) 'Query)
parseReqBody (ClientRequest q v _)
  = first errorBundlePretty
  . parse (runReaderT queryParser v) "<server>" $ q

data ClientRequest = 
  ClientRequest { queryContent  :: Text
                , variables     :: Vars
                , operationName :: Maybe Text
                } deriving (Show, Eq, Ord)

maybeClientRequest :: C8.ByteString -> Maybe ClientRequest
maybeClientRequest rb = do
  json  <- decode @Value $ BL.fromStrict rb
  q     <- json ^? key "query" . _String
  let v = fromMaybe M.empty $ do
            vars     <- json ^? key "variables" . _Object
            pure     $  M.fromList . stringify . HM.toList $ vars
  let o = json ^? key "operationName" . _String
  pure $ ClientRequest q v o

stringify :: [(Text, Value)] -> [(String, String)]
stringify = fmap f
  where
    f (t,v) = ( T.unpack t
              , C8.unpack . BL.toStrict . encode $ v
              )


note :: Maybe a -> Either String a
note Nothing = Left "maybeClientRequest failed"
note (Just x) = Right x

app :: (ToJSON (q 'Response), Wefty q, Show (q 'Query)) => Gql q m s 'Resolver -> Application
app resolver req f = do
#if MIN_VERSION_wai(3,2,2)
  rb <- getRequestBodyChunk req
#else
  rb <- requestBody req
#endif
  let _eitherQuery = do
        clientRequest <- note . maybeClientRequest $ rb
        parseReqBody clientRequest
  case _eitherQuery of
          Right query' -> do
            res <- resolve resolver query'
            f $ successResponse res
          Left e  -> f $ errorResponse $ BL.fromStrict $ C8.pack e

successResponse :: HasJSONResponse record
                => record 'Response
                -> Response
successResponse =
  responseLBS status200 [(hContentType, "application/json")]
  . encode
  . jsonResponse

errorResponse :: BL.ByteString -> Response
errorResponse = responseLBS status500 [(hContentType, "application/json")]

server :: (Wefty q, Show (q 'Query))
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
  , corsIgnoreFailures    = True
  }
