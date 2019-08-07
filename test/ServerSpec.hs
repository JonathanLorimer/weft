{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NoMonoLocalBinds           #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DerivingStrategies         #-}

module ServerSpec where

import           Data.Aeson
import           Data.ByteString.Lazy as BL
import qualified Data.Map as M
-- import           Data.Text (Text)
import           Test.Hspec hiding (Arg)
-- import           Test.QuickCheck hiding (Args)
import           TestData
import           Weft.Generics.Hydrate
import           Weft.Generics.JSONResponse
import           Weft.Generics.Resolve
import           Weft.Internal.Types
import           Weft.Server
-- import           Weft.Types

------------------------------------------------------------------------------------------
-- | Mock Resolvers

getUserResolver :: (Arg "id" ID) -> User 'Query -> IO (User 'Response)
getUserResolver a q
    | (getArg a) == ID "1" = pure $ hydrate jonathan q
    | (getArg a) == ID "2" = pure $ hydrate sandy q
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

getAllUsersTestString :: ClientRequest
getAllUsersTestString = ClientRequest
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
  mempty
  Nothing

getAllUsersTestJson :: BL.ByteString
getAllUsersTestJson = "{\"query\":{\"getAllUsers\":[{\"userName\":\"Sandy\",\"userId\":\"2\",\"userBestFriend\":{\"userName\":\"Jonathan\",\"userId\":\"1\"}},{\"userName\":\"Jonathan\",\"userId\":\"1\",\"userBestFriend\":{\"userName\":\"Sandy\",\"userId\":\"2\"}}]}}"

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
        , userFingers    = M.empty
        }

    userBestFriendQ =
      User
        { userId         = M.singleton "userId" (Arg Nothing :@@ ANil, ())
        , userName       = M.singleton "userName" (ANil , ())
        , userBestFriend = M.empty
        , userFriends    = M.empty
        , userFingers    = M.empty
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
        , userFingers    = M.empty
        }
    userFriendsQ =
      User
        { userId         = M.singleton "userId" (Arg Nothing :@@ ANil ,())
        , userName       = M.singleton "userName" (ANil , ())
        , userBestFriend = M.empty
        , userFriends    = M.empty
        , userFingers    = M.empty
        }

getUserTestString :: ClientRequest
getUserTestString = ClientRequest
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
  mempty
  Nothing

getUserTestQuery :: Either String (Gql GqlQuery m s 'Query)
getUserTestQuery = Right (Gql { query = M.singleton "query" (ANil, gqlQ) })
  where
    gqlQ = GqlQuery
      { getAllUsers = M.empty
      , getUser     = M.singleton "getUser" (Arg (ID "1") :@@ ANil, getUserQ)
      }

    getUserQ =
      User
        { userId         = M.singleton "userId" (Arg Nothing :@@ ANil, ())
        , userName       = M.singleton "userName" (ANil , ())
        , userBestFriend = M.singleton "userBestFriend" ( Arg Nothing :@@ ANil
                                                        , bestFriendQ
                                                        )
        , userFriends    = M.empty
        , userFingers    = M.empty
        }

    bestFriendQ =
      User
        { userId         = M.empty
        , userName       = M.singleton "userName" (ANil, ())
        , userBestFriend = M.empty
        , userFriends    = M.empty
        , userFingers    = M.empty
        }

getUserTestJsonQuery :: Gql GqlQuery m s 'Query
getUserTestJsonQuery =
  Gql
    { query = M.singleton "query" (ANil, gqlQ)
    }
  where
    gqlQ = GqlQuery
      { getAllUsers = M.empty
      , getUser     = M.singleton "getUser" (Arg (ID "1") :@@ ANil, getUserQ)
      }

    getUserQ =
      User
        { userId         = M.singleton "userId" (Arg Nothing :@@ ANil , ())
        , userName       = M.singleton "userName" (ANil , ())
        , userBestFriend = M.singleton "userBestFriend" ( Arg Nothing :@@ ANil
                                                        , bestFriendQ
                                                        )
        , userFriends    = M.empty
        , userFingers    = M.empty
        }

    bestFriendQ =
      User
        { userId         = M.empty
        , userName       = M.singleton "userName" (ANil , ())
        , userBestFriend = M.empty
        , userFriends    = M.empty
        , userFingers    = M.empty
        }

getUserTestJson :: BL.ByteString
getUserTestJson =
  "{\"query\":{\"getUser\":{\"userName\":\"Jonathan\",\"userId\":\"1\",\"userBestFriend\":{\"userName\":\"Sandy\"}}}}"

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
      (encode $ jsonResponse response) `shouldBe` getUserTestJson
    it "should encode a response for getAllUsers as JSON" $ do
      response <- resolve gqlResolver $ getAllUsersTestJsonQuery
      (encode $ jsonResponse response) `shouldBe` getAllUsersTestJson

