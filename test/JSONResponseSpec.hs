{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module JSONResponseSpec where

import           Data.Aeson
import           Data.ByteString.Lazy
import qualified Data.Map as M
import           Test.Hspec hiding (Arg)
import           TestData
import           Weft.Generics.Hydrate
import           Weft.Generics.JSONResponse
import           Weft.Internal.Types
import           Weft.Internal.Utils


userQuery :: HKD User (ToMagic 'Query)
userQuery =
  buildQuery @User
    (ToQuery $ M.singleton "id" (Arg Nothing :@@ ANil, ()))
    (ToQuery $ M.singleton "name" (ANil, ()))
    (ToQuery $ M.singleton "bestFriend" (Arg Nothing :@@ ANil, runHKD userBestFriendQ))
    (ToQuery M.empty)
    (ToQuery M.empty)

userBestFriendQ :: HKD User (ToMagic 'Query)
userBestFriendQ =
  buildQuery @User
    (ToQuery M.empty)
    (ToQuery $ M.singleton "name" (ANil, ()))
    (ToQuery M.empty)
    (ToQuery M.empty)
    (ToQuery M.empty)

mockJonathanJSON :: ByteString
mockJonathanJSON = "{\"name\":\"Jonathan\",\"id\":\"1\",\"bestFriend\":{\"name\":\"Sandy\"}}"

mockSandyJSON :: ByteString
mockSandyJSON = "{\"name\":\"Sandy\",\"id\":\"2\",\"bestFriend\":{\"name\":\"Jonathan\"}}"

spec :: Spec
spec = do
  describe "parses json from response" $ do
    it "should parse JSON from Sandy" $
      decode @Value (encode (jsonResponse $ hydrate sandy userQuery))
        `shouldBe` decode mockSandyJSON
    it "should parse JSON from Jonathan" $ do
      decode @Value (encode (jsonResponse $ hydrate jonathan userQuery))
        `shouldBe` decode mockJonathanJSON

