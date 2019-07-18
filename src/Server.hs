module Server where

import Network.Wai (responseLBS, Application, requestBody)
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types (status200)
import Network.HTTP.Types.Header (hContentType)

app :: Application
app req f = do
    print =<< (requestBody req)
    -- print f
    f $ responseLBS status200 [(hContentType, "text/plain")] "type Query { getGod: God! } type God { name: String entity_type: String god_of: String parents: [Entity] consorts: [Entity] children: [God] roman: String generation: Int olympian: Boolean }"

main :: IO ()
main = do
    let port = 3000
    putStrLn $ "Listening on port " ++ show port
    run port app

-- $> main