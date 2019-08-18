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
    (ToQuery $ M.singleton "userId" (Arg Nothing :@@ ANil, ()))
    (ToQuery $ M.singleton "userName" (ANil, ()))
    (ToQuery $ M.singleton "userBestFriend" (Arg Nothing :@@ ANil, runHKD userBestFriendQ))
    (ToQuery M.empty)
    (ToQuery M.empty)

userBestFriendQ :: HKD User (ToMagic 'Query)
userBestFriendQ =
  buildQuery @User
    (ToQuery M.empty)
    (ToQuery $ M.singleton "userName" (ANil, ()))
    (ToQuery M.empty)
    (ToQuery M.empty)
    (ToQuery M.empty)

mockJonathanJSON :: ByteString
mockJonathanJSON = "{\"userName\":\"Jonathan\",\"userId\":\"1\",\"userBestFriend\":{\"userName\":\"Sandy\"}}"

mockSandyJSON :: ByteString
mockSandyJSON = "{\"userName\":\"Sandy\",\"userId\":\"2\",\"userBestFriend\":{\"userName\":\"Jonathan\"}}"

spec :: Spec
spec = do
  describe "parses json from response" $ do
    it "should parse JSON from Sandy" $
      encode (jsonResponse $ hydrate sandy userQuery) `shouldBe` mockSandyJSON
    it "should parse JSON from Jonathan" $ do
      encode (jsonResponse $ hydrate jonathan userQuery) `shouldBe` mockJonathanJSON

