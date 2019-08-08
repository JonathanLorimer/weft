{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module JSONResponseSpec where

import           Data.Aeson
import           Data.ByteString.Lazy
import qualified Data.Map as M
import           Test.Hspec hiding (Arg)
import           BizzaroData
import           Weft.Generics.Hydrate
import           Weft.Generics.JSONResponse
import           Weft.Internal.Types
import           Weft.Internal.Utils


userQuery :: HKD User (ToMagic 'Query)
userQuery =
  buildQuery @User
    (ToMagic $ M.singleton "userId" (Arg Nothing :@@ ANil, ()))
    (ToMagic $ M.singleton "userName" (ANil, ()))
    (ToMagic $ M.singleton "userBestFriend" (Arg Nothing :@@ ANil, runHKD userBestFriendQ))
    (ToMagic M.empty)
    (ToMagic M.empty)

userBestFriendQ :: HKD User (ToMagic 'Query)
userBestFriendQ =
  buildQuery @User
    (ToMagic M.empty)
    (ToMagic $ M.singleton "userName" (ANil, ()))
    (ToMagic M.empty)
    (ToMagic M.empty)
    (ToMagic M.empty)

mockJonathanJSON :: ByteString
mockJonathanJSON = "{\"userName\":\"Jonathan\",\"userId\":\"1\",\"userBestFriend\":{\"userName\":\"Sandy\"}}"

mockSandyJSON :: ByteString
mockSandyJSON = "{\"userName\":\"Sandy\",\"userId\":\"2\",\"userBestFriend\":{\"userName\":\"Jonathan\"}}"

spec :: Spec
spec = do
  describe "parses json from response" $ do
    it "should parse JSON from Sandy" $
      encode (
        magicJsonResponse $
          buildResponse @User
            (ToMagic $ M.singleton "userId" $ ID "2")
            (ToMagic $ M.singleton "userName" "Sandy")
            (ToMagic $ M.singleton "userBestFriend"
                     $ runHKD
                     $ buildResponse @User
                         (ToMagic M.empty)
                         (ToMagic $ M.singleton "userName" "Jonathan")
                         (ToMagic M.empty)
                         (ToMagic M.empty)
                         (ToMagic M.empty)
            ) -- runHKD $ buildResponse @User (ToMagic M.empty) ())
            (ToMagic M.empty)
            (ToMagic M.empty)

             ) `shouldBe` mockSandyJSON
    -- it "should parse JSON from Jonathan" $ do
    --   encode (jsonResponse $ hydrate jonathan userQuery) `shouldBe` mockJonathanJSON

