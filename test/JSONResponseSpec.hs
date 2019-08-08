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


userQuery :: User 'Query
userQuery = User
  { userId         = M.singleton "userId" (Arg Nothing :@@ ANil, ())
  , userName       = M.singleton "userName" (ANil, ())
  , userBestFriend = M.singleton "userBestFriend" ( Arg Nothing :@@ ANil
                                                  , userBestFriendQ
                                                  )
  , userFriends    = M.empty
  , userFingers    = M.empty
  }

userBestFriendQ :: User 'Query
userBestFriendQ = User
  { userId         = M.empty
  , userName       = M.singleton "userName" (ANil, ())
  , userBestFriend = M.empty
  , userFriends    = M.empty
  , userFingers    = M.empty
  }

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

