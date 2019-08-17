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
import Network.Wai.Handler.WebSockets
import Network.HTTP.Types (status200, status500)
import Network.HTTP.Types.Header (hContentType)
import Network.HTTP.Types.Method
import qualified Network.WebSockets as WS
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


parseReqBody :: (Wefty q, Wefty m)
             => ClientRequest
             -> Either String ((Gql q m s) 'Query)
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


note :: String -> Maybe a -> Either String a
note s Nothing = Left s
note _ (Just x) = Right x

app :: (Wefty q, Wefty m)
    => Gql q m s 'Resolver
    -> Application
app resolver req f = do
#if MIN_VERSION_wai(3,2,2)
  rb <- getRequestBodyChunk req
#else
  rb <- requestBody req
#endif
  let _eitherQuery = do
        clientRequest <- note "maybeClientRequest failed" . maybeClientRequest $ rb
        parseReqBody clientRequest
  case _eitherQuery of
          Right query' -> do
            res <- resolve resolver query'
            f $ successResponse res
          Left e  -> f $ errorResponse $ BL.fromStrict $ C8.pack e

successResponse :: (HasJSONResponse q, HasJSONResponse m)
                => Gql m q s 'Response
                -> Response
successResponse =
  responseLBS status200 [(hContentType, "application/json")]
  . encode
  . jsonify

jsonify :: (HasJSONResponse q, HasJSONResponse m)
        => Gql q m s 'Response
        -> Value
jsonify (Gql q m) = object $ f $ qj <> mj
  where
    f = fmap (first $ const "data") . M.toList
    qj = jsonResponse <$> q
    mj = jsonResponse <$> m

errorResponse :: BL.ByteString -> Response
errorResponse = responseLBS status500 [(hContentType, "application/json")]

server :: (Wefty q, Wefty m)
       => [Settings -> Settings]
       -> Gql q m s 'Resolver
       -> IO ()
server s r = runSettings
  (appEndo (foldMap Endo s) defaultSettings)
  (cors extremelyPermissiveCorsPolicy $ dualApp r)

dualApp :: (Wefty q, Wefty m)
          => Gql q m s 'Resolver
          -> Application
dualApp r = websocketsOr WS.defaultConnectionOptions (wsApp r) (app r)


wsApp :: Gql q m s 'Resolver -> WS.ServerApp
wsApp (Gql _ _ s) pending_conn = do
      conn <- WS.acceptRequest pending_conn
      s $ WS.sendBinaryData conn

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
