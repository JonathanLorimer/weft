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
import Data.Text
import Data.Text.Encoding
import qualified Data.ByteString.Lazy as BL




parseReqBody :: Text -> Either String (User 'Query)
parseReqBody query = parseOnly
                    queryParser
                    (encodeUtf8 $ query)

maybeQuery :: ByteString -> Maybe Text
maybeQuery rb = do
    json <- decode @Value $ BL.fromStrict rb
    (^? key "query" . _String) json


note :: Maybe a -> Either String a
note Nothing = Left ""
note (Just x) = Right x

app :: Application
app req f = do
    rb <- getRequestBodyChunk req
    let tq = note . maybeQuery $ rb
    print tq
    let _eitherQuery = do
            textQuery <- note . maybeQuery $ rb
            -- TODO: actually parse out query, don't just drop it.
            Right $ Data.Text.drop 6 $ textQuery
    print _eitherQuery
    print $ parseReqBody <$> _eitherQuery
    f $ responseLBS status200 [(hContentType, "text/plain")] "response"

main :: IO ()
main = do
    let port = 3000
    Prelude.putStrLn $ "Listening on port " ++ show port
    run port app

-- $> main
