module Server where

import Weft.Internal.Types
import Weft.Generics.QueryParser
import TestData
import Lens.Micro
import Lens.Micro.Aeson
import Network.Wai (responseLBS, Application, getRequestBodyChunk)
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types (status200)
import Network.HTTP.Types.Header (hContentType)
import Data.Aeson hiding (json)
import Data.Attoparsec.ByteString.Char8
import Data.ByteString.Char8
import Data.Text.Encoding
import qualified Data.ByteString.Lazy as BL



parseReqBody :: RequestType ByteString -> Either String (record 'Query)
parseReqBody (QueryRequest query)               = parseOnly
                                                  queryParser
                                                  query
parseReqBody (MutationRequest mutation)         = undefined
parseReqBody (SubscriptionRequest subscription) = undefined


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
    let tq = note . maybeQuery $ rb
    Prelude.putStrLn "tq"
    print tq
    Prelude.putStrLn "maybeQuery"
    print $ note . maybeQuery $ rb
    let _eitherQuery = do
            textQuery <- note . maybeQuery $ rb
            reqBody <- parseServerRequest textQuery
            parseReqBody reqBody
    -- print _eitherQuery
    f $ responseLBS status200 [(hContentType, "application/json")] "response"

main :: IO ()
main = do
    let port = 3000
    Prelude.putStrLn $ "Listening on port " ++ show port
    run port app

