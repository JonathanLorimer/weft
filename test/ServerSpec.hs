{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NoMonoLocalBinds           #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DerivingStrategies         #-}

module ServerSpec where

import Test.Hspec hiding (Arg)
import Weft.Server
import Weft.Types
import Weft.Internal.Types
import Weft.Generics.Resolve
import Weft.Generics.Hydrate
import Data.Aeson
import GHC.Generics
import Test.QuickCheck hiding (Args)
import Data.ByteString.Char8 as C8
import Data.ByteString.Lazy as BL

------------------------------------------------------------------------------------------
-- | Mock Data
newtype Id = Id Int 
  deriving          (ToJSON, Generic)
  deriving stock    (Show)
  deriving newtype  (Eq, Ord, Arbitrary)
  deriving Read via (Int)

newtype Name = Name String deriving (Generic, Show, Eq, Ord, Arbitrary, ToJSON)

data GqlQuery ts = GqlQuery
    { getUser :: Magic ts (Arg "id" Id -> User ts)
    , getAllUsers :: Magic ts [User ts]
    } deriving (Generic)

deriving instance AllHave Eq (GqlQuery ts)            => Eq (GqlQuery ts)
deriving instance AllHave Show (GqlQuery ts)          => Show (GqlQuery ts)
deriving via (NoNothingJSON (GqlQuery 'Response)) 
  instance AllHave ToJSON (GqlQuery 'Response)        => ToJSON (GqlQuery 'Response)

data User ts = User
  { userId         :: Magic ts (Arg "arg" (Maybe Int) -> Id)
  , userName       :: Magic ts Name
  , userBestFriend :: Magic ts (Arg "arg" (Maybe Int) -> User ts)
  , userFriends    :: Magic ts [User ts]
  } deriving (Generic)

deriving instance AllHave Show (User ts)     => Show (User ts)
deriving instance AllHave Eq (User ts)       => Eq (User ts)
deriving via (NoNothingJSON (User 'Response)) 
  instance AllHave ToJSON (User 'Response)   => ToJSON (User 'Response)

jonathan :: User 'Data
jonathan = User { userId = (Id 1), userName = ( Name "Jonathan"), userBestFriend = sandy, userFriends = [] }

sandy :: User 'Data
sandy = User { userId = (Id 2), userName = ( Name "Sandy"), userBestFriend = jonathan, userFriends = []}

------------------------------------------------------------------------------------------
-- | Mock Resolvers

getUserResolver :: (Arg "id" Id) -> User 'Query -> IO (User 'Response)
getUserResolver a q
    | (getArg a) == (Id 1) = pure $ hydrate jonathan q
    | (getArg a) == (Id 2) = pure $ hydrate sandy q
    | otherwise = pure $ hydrate jonathan q

getAllUsersResolver :: User 'Query -> IO [User 'Response]
getAllUsersResolver q = pure $ (flip hydrate q) <$> [sandy, jonathan]

queryResolver :: GqlQuery 'Resolver
queryResolver = GqlQuery
            { getUser = getUserResolver
            , getAllUsers = getAllUsersResolver
            }

gqlResolver :: Gql GqlQuery () () 'Resolver
gqlResolver = Gql { query = resolve queryResolver }

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

getAllUsersTestJsonQuery :: Gql GqlQuery () () 'Query
getAllUsersTestJsonQuery = Gql { query = Just (ANil, gqlQ) }
  where
    gqlQ            = GqlQuery         { getUser = Nothing
                                       , getAllUsers = getAllUsersQ }
    getAllUsersQ    = Just (ANil, User { userId = Just (Arg Nothing :@@ ANil , ())
                                       , userName = Just (ANil , ())
                                       , userBestFriend = userBestFriendQ
                                       , userFriends = Nothing }
                           )
    userBestFriendQ = Just (Arg Nothing :@@ ANil, User { userId = Just (Arg Nothing :@@ ANil ,())
                                                       , userName = Just (ANil , ())
                                                       , userBestFriend = Nothing
                                                       , userFriends = Nothing }
                           )


getAllUsersTestQuery :: Either String (Gql GqlQuery () () 'Query)
getAllUsersTestQuery = Right (Gql { query = Just (ANil, gqlQ) })
  where
    gqlQ         = GqlQuery         { getUser = Nothing
                                    , getAllUsers = getAllUsersQ }
    getAllUsersQ = Just (ANil, User { userId = Just (Arg Nothing :@@ ANil , ())
                                    , userName = Just (ANil , ())
                                    , userBestFriend = Nothing
                                    , userFriends = userFriendsQ }
                        )
    userFriendsQ = Just (ANil, User { userId = Just (Arg Nothing :@@ ANil ,())
                                    , userName = Just (ANil , ())
                                    , userBestFriend = Nothing
                                    , userFriends = Nothing }
                        )
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

getUserTestQuery :: Either String (Gql GqlQuery () () 'Query)
getUserTestQuery = Right (Gql { query = Just (ANil, gqlQ) })
  where
      gqlQ        = GqlQuery { getAllUsers = Nothing
                            , getUser = getUserQ }
      getUserQ    = Just (Arg (Id 1) :@@ ANil
                        , User { userId = Just (Arg Nothing :@@ ANil , ())
                                , userName = Just (ANil , ())
                                , userBestFriend = bestFriendQ
                                , userFriends = Nothing }
                        )
      bestFriendQ = Just (Arg Nothing :@@ ANil
                        , User { userId = Nothing
                                , userName = Just (ANil , ())
                                , userBestFriend = Nothing
                                , userFriends = Nothing }
                        )
                        
getUserTestJsonQuery :: Gql GqlQuery () () 'Query
getUserTestJsonQuery = Gql { query = Just (ANil, gqlQ) }
  where
      gqlQ        = GqlQuery { getAllUsers = Nothing
                            , getUser = getUserQ }
      getUserQ    = Just (Arg (Id 1) :@@ ANil
                        , User { userId = Just (Arg Nothing :@@ ANil , ())
                                , userName = Just (ANil , ())
                                , userBestFriend = bestFriendQ
                                , userFriends = Nothing }
                        )
      bestFriendQ = Just (Arg Nothing :@@ ANil
                        , User { userId = Nothing
                                , userName = Just (ANil , ())
                                , userBestFriend = Nothing
                                , userFriends = Nothing }
                        )

getUserTestJson :: BL.ByteString
getUserTestJson = "{\"query\":{\"getUser\":{\"userName\":\"Jonathan\",\"userId\":1,\"userBestFriend\":{\"userName\":\"Sandy\"}}}}"

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
    describe "JSON encoding responses" $ do
        it "should encode a response for getUser as JSON" $ do
            response <- resolve gqlResolver $ getUserTestJsonQuery
            (encode response) `shouldBe` getUserTestJson
        it "should encode a response for getAllUsers as JSON" $ do
            response <- resolve gqlResolver $ getAllUsersTestJsonQuery
            (encode response) `shouldBe` getAllUsersTestJson
