{-# LANGUAGE CPP #-}

module Weft.Server where

import Control.Applicative
import Weft.Internal.Types
import Weft.Types
import Weft.Generics.QueryParser
import Weft.Generics.Resolve
import Lens.Micro
import Lens.Micro.Aeson
import Network.Wai.Middleware.Cors
import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Types (status200, status500)
import Network.HTTP.Types.Header (hContentType)
import Network.HTTP.Types.Method
import qualified Data.List as L
import Data.Aeson hiding (json)
import Data.Attoparsec.ByteString.Char8
import Data.ByteString.Char8
import Data.Text.Encoding
import Data.Monoid
import qualified Data.ByteString.Lazy as BL
import Control.Monad.Reader


parseReqBody :: (Wefty query)
             => ByteString
             -> Either String ((Gql query () ()) 'Query)
parseReqBody queryString = parseOnly
                           (runReaderT (queryParser <|> anonymousQueryParser) mempty)
                           queryString

maybeQuery :: ByteString -> Maybe ByteString
maybeQuery rb = do
    json <- decode @Value $ BL.fromStrict rb
    encodeUtf8 <$> ((^? key "query" . _String) json)

note :: Maybe a -> Either String a
note Nothing = Left ""
note (Just x) = Right x

app :: (ToJSON (q 'Response), Wefty q) => Gql q () () 'Resolver -> Application
app resolver req f = do
#if MIN_VERSION_wai(3,2,2)
        rb <- getRequestBodyChunk req
#else
        rb <- requestBody req
#endif
        let _eitherQuery = do
                textQuery <- note . maybeQuery $ rb
                parseReqBody textQuery
        case _eitherQuery of
            Right q -> do
                res <- resolve resolver q
                f $ successResponse res
            Left e  -> f $ errorResponse $ BL.fromStrict $ pack e

successResponse :: ToJSON a => a -> Response
successResponse = responseLBS status200 [(hContentType, "application/json")] . encode

errorResponse :: BL.ByteString -> Response
errorResponse = responseLBS status500 [(hContentType, "application/json")]

server :: (Wefty q)
       => [Settings -> Settings]
       -> Gql q () () 'Resolver
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
