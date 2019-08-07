{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NoMonoLocalBinds           #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DerivingStrategies         #-}

module ServerSpec where

import           Data.Aeson
import           Data.ByteString.Char8 as C8
import           Data.ByteString.Lazy as BL
import qualified Data.Map as M
import           Test.Hspec hiding (Arg)
import           Test.QuickCheck hiding (Args)
import           Weft.Generics.Hydrate
import           Weft.Generics.Resolve
import           Weft.Internal.Types
import           Weft.Server
import           Weft.Types

------------------------------------------------------------------------------------------
-- | Mock Data
newtype Id = Id Int
  deriving stock (Generic, Show, Eq, Ord)
  deriving newtype (Arbitrary, ToJSON)
  deriving Read via (Int)

newtype Name = Name String
  deriving stock (Generic, Show, Eq, Ord)
  deriving newtype (Arbitrary, ToJSON)

data GqlQuery ts = GqlQuery
    { getUser :: Magic ts (Arg "id" Id -> User ts)
    , getAllUsers :: Magic ts [User ts]
    } deriving (Generic)

deriving instance AllHave Eq (GqlQuery ts)   => Eq (GqlQuery ts)
deriving instance AllHave Show (GqlQuery ts) => Show (GqlQuery ts)
deriving via (NoNothingJSON (GqlQuery 'Response))
  instance AllHave ToJSON (GqlQuery 'Response) => ToJSON (GqlQuery 'Response)

data User ts = User
  { userId         :: Magic ts (Arg "arg" (Maybe Int) -> Id)
  , userName       :: Magic ts Name
  , userBestFriend :: Magic ts (Arg "arg" (Maybe Int) -> User ts)
  , userFriends    :: Magic ts [User ts]
  } deriving (Generic)

deriving instance AllHave Show (User ts) => Show (User ts)
deriving instance AllHave Eq (User ts)   => Eq (User ts)
deriving via (NoNothingJSON (User 'Response))
  instance AllHave ToJSON (User 'Response) => ToJSON (User 'Response)

jonathan :: User 'Data
jonathan =
  User
    { userId         = Id 1
    , userName       = Name "Jonathan"
    , userBestFriend = sandy
    , userFriends    = []
    }

sandy :: User 'Data
sandy =
  User
    { userId         = Id 2
    , userName       = Name "Sandy"
    , userBestFriend = jonathan
    , userFriends    = []
    }

------------------------------------------------------------------------------------------
-- | Mock Resolvers

getUserResolver :: (Arg "id" Id) -> User 'Query -> IO (User 'Response)
getUserResolver a q
    | (getArg a) == Id 1 = pure $ hydrate jonathan q
    | (getArg a) == Id 2 = pure $ hydrate sandy q
    | otherwise = pure $ hydrate jonathan q

getAllUsersResolver :: User 'Query -> IO [User 'Response]
getAllUsersResolver q = pure $ hydrateF [sandy, jonathan] q

queryResolver :: GqlQuery 'Resolver
queryResolver =
  GqlQuery
    { getUser     = getUserResolver
    , getAllUsers = getAllUsersResolver
    }

gqlResolver :: Gql GqlQuery m s 'Resolver
gqlResolver =
  Gql
    { query = resolve queryResolver
    }

------------------------------------------------------------------------------------------
-- | Mock Queries

getAllUsersTestString :: C8.ByteString
getAllUsersTestString =
  "  query {          \
  \    getAllUsers {  \
  \      userId       \
  \      userName     \
  \      userFriends {\
  \        userId     \
  \        userName   \
  \      }            \
  \    }              \
  \  }                \
  \"

getAllUsersTestJson :: BL.ByteString
getAllUsersTestJson = "{\"query\":{\"getAllUsers\":[{\"userName\":\"Sandy\",\"userId\":2,\"userBestFriend\":{\"userName\":\"Jonathan\",\"userId\":1}},{\"userName\":\"Jonathan\",\"userId\":1,\"userBestFriend\":{\"userName\":\"Sandy\",\"userId\":2}}]}}"

getAllUsersTestJsonQuery :: Gql GqlQuery m s 'Query
getAllUsersTestJsonQuery =
  Gql { query = M.singleton "query" (ANil, gqlQ)
      }
  where
    gqlQ =
      GqlQuery
        { getUser     = M.empty
        , getAllUsers = M.singleton "getAllUsers" (ANil, getAllUsersQ)
        }

    getAllUsersQ =
      User
        { userId         = M.singleton "userId" (Arg Nothing :@@ ANil, ())
        , userName       = M.singleton "userName" (ANil, ())
        , userBestFriend = M.singleton "userBestFriend" ( Arg Nothing :@@ ANil
                                                        , userBestFriendQ
                                                        )
        , userFriends    = M.empty
        }

    userBestFriendQ =
      User
        { userId         = M.singleton "userId" (Arg Nothing :@@ ANil, ())
        , userName       = M.singleton "userName" (ANil , ())
        , userBestFriend = M.empty
        , userFriends    = M.empty
        }


getAllUsersTestQuery :: Either String (Gql GqlQuery m s 'Query)
getAllUsersTestQuery = Right (Gql { query = M.singleton "query" (ANil, gqlQ) })
  where
    gqlQ = GqlQuery
      { getUser     = M.empty
      , getAllUsers = M.singleton "getAllUsers" (ANil, getAllUsersQ)
      }
    getAllUsersQ =
      User
        { userId         = M.singleton "userId" (Arg Nothing :@@ ANil, ())
        , userName       = M.singleton "userName" (ANil, ())
        , userBestFriend = M.empty
        , userFriends    = M.singleton "userFriends" (ANil, userFriendsQ)
        }
    userFriendsQ =
      User
        { userId         = M.singleton "userId" (Arg Nothing :@@ ANil ,())
        , userName       = M.singleton "userName" (ANil , ())
        , userBestFriend = M.empty
        , userFriends    = M.empty
        }

getUserTestString :: C8.ByteString
getUserTestString =
  "  query {              \
  \    getUser(id: 1) {   \
  \      userId           \
  \      userName         \
  \      userBestFriend { \
  \        userName       \
  \      }                \
  \    }                  \
  \ }                     \
  \"

getUserTestQuery :: Either String (Gql GqlQuery m s 'Query)
getUserTestQuery = Right (Gql { query = M.singleton "query" (ANil, gqlQ) })
  where
    gqlQ = GqlQuery
      { getAllUsers = M.empty
      , getUser     = M.singleton "getUser" (Arg (Id 1) :@@ ANil, getUserQ)
      }

    getUserQ =
      User
        { userId         = M.singleton "userId" (Arg Nothing :@@ ANil, ())
        , userName       = M.singleton "userName" (ANil , ())
        , userBestFriend = M.singleton "userBestFriend" ( Arg Nothing :@@ ANil
                                                        , bestFriendQ
                                                        )
        , userFriends    = M.empty
        }

    bestFriendQ =
      User
        { userId         = M.empty
        , userName       = M.singleton "userName" (ANil, ())
        , userBestFriend = M.empty
        , userFriends    = M.empty
        }

getUserTestJsonQuery :: Gql GqlQuery m s 'Query
getUserTestJsonQuery =
  Gql
    { query = M.singleton "query" (ANil, gqlQ)
    }
  where
    gqlQ = GqlQuery
      { getAllUsers = M.empty
      , getUser     = M.singleton "getUser" (Arg (Id 1) :@@ ANil, getUserQ)
      }

    getUserQ =
      User
        { userId         = M.singleton "userId" (Arg Nothing :@@ ANil , ())
        , userName       = M.singleton "userName" (ANil , ())
        , userBestFriend = M.singleton "userBestFriend" ( Arg Nothing :@@ ANil
                                                        , bestFriendQ
                                                        )
        , userFriends    = M.empty
        }

    bestFriendQ =
      User
        { userId         = M.empty
        , userName       = M.singleton "userName" (ANil , ())
        , userBestFriend = M.empty
        , userFriends    = M.empty
        }

getUserTestJson :: BL.ByteString
getUserTestJson =
  "{\"query\":{\"getUser\":{\"userName\":\"Jonathan\",\"userId\":1,\"userBestFriend\":{\"userName\":\"Sandy\"}}}}"

instance Arbitrary (User 'Query) where
  arbitrary = recordGen
  shrink = genericShrink

instance Arbitrary (User 'Data) where
  arbitrary = recordGen
  shrink = genericShrink

------------------------------------------------------------------------------------------
-- | Tests

spec :: Spec
spec = describe "server" $ do
  describe "parseReqBody" $ do
    it "should parse query with no args (getUser)" $
      parseReqBody getAllUsersTestString `shouldBe` getAllUsersTestQuery
    it "should parse query with args (getAllUsers)" $
      parseReqBody getUserTestString `shouldBe` getUserTestQuery

  xdescribe "JSON encoding responses" $ do
    it "should encode a response for getUser as JSON" $ do
      response <- resolve gqlResolver $ getUserTestJsonQuery
      (encode response) `shouldBe` getUserTestJson
    it "should encode a response for getAllUsers as JSON" $ do
      response <- resolve gqlResolver $ getAllUsersTestJsonQuery
      (encode response) `shouldBe` getAllUsersTestJson

