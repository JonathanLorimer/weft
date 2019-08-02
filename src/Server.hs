module Server where

import Weft.Internal.Types
import Weft.Types
import Weft.Generics.QueryParser
import Weft.Generics.Resolve
import Weft.Generics.EmptyQuery
import TestData
import Lens.Micro
import Lens.Micro.Aeson
import Network.Wai.Middleware.Cors
import Network.Wai
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types (status200, status500)
import Network.HTTP.Types.Header (hContentType)
import Network.HTTP.Types.Method
import Data.Aeson hiding (json)
import Data.Attoparsec.ByteString.Char8
import Data.ByteString.Char8
import Data.Text.Encoding
import qualified Data.Map as M
import qualified Data.ByteString.Lazy as BL
import Control.Monad.Reader
import GHC.Generics

parseReqBody :: (Wefty query)
             => ByteString
             -> Either String ((Gql query () ()) 'Query)
parseReqBody queryString = parseOnly
                           (runReaderT (queryParser) mempty)
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
        rb <- getRequestBodyChunk req
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

main :: IO ()
main = do
    let port = 3000
    Prelude.putStrLn $ "Listening on port " ++ show port
    run port $ cors extremelyPermissiveCorsPolicy $ app gqlResolver

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