module Server where

import Parser
import TestData
import QueryParser
import SchemaGenerator
import Lens.Micro
import Lens.Micro.Aeson
import Network.Wai (responseLBS, Application, requestBody)
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types (status200)
import Network.HTTP.Types.Header (hContentType)
import Data.Aeson
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Lazy as BL

app :: Application
app req f = do
    let parsedBody = (toJSON $ requestBody req) ^. key "query"
    -- print =<< (requestBody req)
    print $ parseOnly (incrParser (emptyQuery @User)) (BL.toStrict (encode parsedBody))
    f $ responseLBS status200 [(hContentType, "text/plain")] "type Query { getGod: God! } type God { name: String entity_type: String god_of: String parents: [Entity] consorts: [Entity] children: [God] roman: String generation: Int olympian: Boolean }"

main :: IO ()
main = do
    let port = 3000
    putStrLn $ "Listening on port " ++ show port
    run port app

-- $> main