{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module JSONResponseSpec where

import           Test.Hspec hiding (Arg)
import           Weft.Internal.Types
import           Weft.Generics.Hydrate
import           Weft.Generics.JSONResponse
import           Test.QuickCheck
import qualified Data.Map as M
import           Data.Aeson
import           Data.ByteString.Lazy
import           GHC.Generics

newtype Id = Id Int
  deriving stock (Generic, Show, Eq, Ord)
  deriving newtype (Arbitrary, ToJSON)
  deriving Read via (Int)

newtype Name = Name String
  deriving stock (Generic, Show, Eq, Ord)
  deriving newtype (Arbitrary, ToJSON)

data User ts = User
  { userId         :: Magic ts (Arg "arg" (Maybe Int) -> Id)
  , userName       :: Magic ts Name
  , userBestFriend :: Magic ts (Arg "arg" (Maybe Int) -> User ts)
  , userFriends    :: Magic ts [User ts]
  } deriving (Generic)


jonathan :: User 'Data
jonathan = User { userId = (Id 1), userName = ( Name "Jonathan"), userBestFriend = sandy, userFriends = [] }

sandy :: User 'Data
sandy = User { userId = (Id 2), userName = ( Name "Sandy"), userBestFriend = jonathan, userFriends = []}

userQuery :: User 'Query
userQuery = User
  { userId         = M.singleton "userId" (Arg Nothing :@@ ANil, ())
  , userName       = M.singleton "userName" (ANil, ())
  , userBestFriend = M.singleton "userBestFriend" ( Arg Nothing :@@ ANil
                                                  , userBestFriendQ
                                                  )
  , userFriends    = M.empty
  }

userBestFriendQ :: User 'Query
userBestFriendQ = User 
  { userId         = M.empty
  , userName       = M.singleton "userName" (ANil, ())
  , userBestFriend = M.empty
  , userFriends    = M.empty
  }

mockJonathanJSON :: ByteString
mockJonathanJSON = "{\"userName\":\"Jonathan\",\"userId\":1,\"userBestFriend\":{\"userName\":\"Sandy\"}}"

mockSandyJSON :: ByteString
mockSandyJSON = "{\"userName\":\"Sandy\",\"userId\":2,\"userBestFriend\":{\"userName\":\"Jonathan\"}}"

spec :: Spec
spec = do
  describe "parses json from response" $ do
    it "should parse JSON from Sandy" $ 
      encode (jsonResponse (hydrate sandy userQuery)) `shouldBe` mockSandyJSON
    it "should parse JSON from Jonathan" $ do
      encode (jsonResponse (hydrate jonathan userQuery)) `shouldBe` mockJonathanJSON