module Server where

import Weft.Internal.Types
import Weft.Types
import Weft.Generics.QueryParser
import Weft.Generics.Resolve
import Weft.Generics.EmptyQuery
import TestData
import Lens.Micro
import Lens.Micro.Aeson
import Network.Wai (responseLBS, Application, Response, getRequestBodyChunk)
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types (status200, status500)
import Network.HTTP.Types.Header (hContentType)
import Data.Aeson hiding (json)
import Data.Attoparsec.ByteString.Char8
import Data.ByteString.Char8
import Data.Text.Encoding
import qualified Data.Map as M
import qualified Data.ByteString.Lazy as BL
import Control.Monad.Reader

parseReqBody :: (HasEmptyQuery GqlQuery, HasQueryParser GqlQuery)
             => ByteString
             -> Either String ((Gql GqlQuery () ()) 'Query)
parseReqBody query = parseOnly
                     (runReaderT (queryParser) mempty)
                     query

maybeQuery :: ByteString -> Maybe ByteString
maybeQuery rb = do
    json <- decode @Value $ BL.fromStrict rb
    encodeUtf8 <$> ((^? key "query" . _String) json)

note :: Maybe a -> Either String a
note Nothing = Left ""
note (Just x) = Right x

app :: Application
app req f = do
    rb <- getRequestBodyChunk req    
    let _eitherQuery = do
            textQuery <- note . maybeQuery $ rb
            parseReqBody $ Data.ByteString.Char8.concat ["{ ", textQuery, " }"]
    print _eitherQuery
    case _eitherQuery of
        Right q -> do
            res <- resolve gqlResolver q
            f $ successResponse res
        Left e  -> f $ errorResponse $ BL.fromStrict . pack $ e

successResponse :: ToJSON a => a -> Response
successResponse = responseLBS status200 [(hContentType, "application/json")] . encode

errorResponse :: BL.ByteString -> Response
errorResponse = responseLBS status500 [(hContentType, "application/json")]

main :: IO ()
main = do
    let port = 3000
    Prelude.putStrLn $ "Listening on port " ++ show port
    run port app
