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
import           Test.Hspec hiding (Arg)
import           TestData
import           Weft.Generics.Hydrate
import           Weft.Generics.JSONResponse
import           Weft.Generics.Resolve
import           Weft.Internal.Types

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

mutationResolver :: GqlMutation 'Resolver
mutationResolver =
  GqlMutation
    { mutateUser     = getUserResolver
    , mutateAllUsers = getAllUsersResolver
    }

noneResolver :: None 'Resolver
noneResolver = None (pure ())

gqlQueryResolver :: Gql GqlQuery None s 'Resolver
gqlQueryResolver =
  Gql (resolve queryResolver) (resolve noneResolver)

gqlMutationResolver :: Gql None GqlMutation s 'Resolver
gqlMutationResolver =
  Gql (resolve noneResolver) (resolve mutationResolver)

------------------------------------------------------------------------------------------
-- | Mock Queries


-- | getAllUsers Mocks
getAllUsersTestJsonQuery :: Gql GqlQuery None s 'Query
getAllUsersTestJsonQuery =
  Gql (M.singleton "query" (ANil, gqlGetAllUsersQ)) M.empty

getAllUsersTestJsonMutation :: Gql None GqlMutation s 'Query
getAllUsersTestJsonMutation =
  Gql M.empty (M.singleton "mutation" (ANil, gqlGetAllUsersM))

gqlGetAllUsersQ :: GqlQuery 'Query
gqlGetAllUsersQ =
  GqlQuery
    { getUser     = M.empty
    , getAllUsers = M.singleton "getAllUsers" (ANil, getAllUsersQ)
    }

gqlGetAllUsersM :: GqlMutation 'Query
gqlGetAllUsersM =
  GqlMutation
    { mutateUser     = M.empty
    , mutateAllUsers = M.singleton "mutateAllUsers" (ANil, getAllUsersQ)
    }

getAllUsersQ :: User 'Query
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


userBestFriendQ :: User 'Query
userBestFriendQ =
  User
    { userId         = M.singleton "userId" (Arg Nothing :@@ ANil, ())
    , userName       = M.singleton "userName" (ANil , ())
    , userBestFriend = M.empty
    , userFriends    = M.empty
    , userFingers    = M.empty
    }

getAllUsersTestJson :: BL.ByteString
getAllUsersTestJson = "{\"query\":{\"getAllUsers\":[{\"userName\":\"Sandy\",\"userId\":\"2\",\"userBestFriend\":{\"userName\":\"Jonathan\",\"userId\":\"1\"}},{\"userName\":\"Jonathan\",\"userId\":\"1\",\"userBestFriend\":{\"userName\":\"Sandy\",\"userId\":\"2\"}}]}}"

mutateAllUsersTestJson :: BL.ByteString
mutateAllUsersTestJson = "{\"mutation\":{\"mutateAllUsers\":[{\"userName\":\"Sandy\",\"userId\":\"2\",\"userBestFriend\":{\"userName\":\"Jonathan\",\"userId\":\"1\"}},{\"userName\":\"Jonathan\",\"userId\":\"1\",\"userBestFriend\":{\"userName\":\"Sandy\",\"userId\":\"2\"}}]}}"


-- | getUser Mocks

getUserTestJsonQuery :: Gql GqlQuery None s 'Query
getUserTestJsonQuery =
  Gql (M.singleton "query" (ANil, gqlGetUserQ)) M.empty

getUserTestJsonMutation :: Gql None GqlMutation s 'Query
getUserTestJsonMutation =
  Gql M.empty (M.singleton "mutation" (ANil, gqlGetUserM))

gqlGetUserQ :: GqlQuery 'Query
gqlGetUserQ = GqlQuery
  { getAllUsers = M.empty
  , getUser     = M.singleton "getUser" (Arg (ID "1") :@@ ANil, getUserQ)
  }

gqlGetUserM :: GqlMutation 'Query
gqlGetUserM = GqlMutation
  { mutateAllUsers = M.empty
  , mutateUser     = M.singleton "mutateUser" (Arg (ID "1") :@@ ANil, getUserQ)
  }

getUserQ :: User 'Query
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

bestFriendQ :: User 'Query
bestFriendQ =
  User
    { userId         = M.empty
    , userName       = M.singleton "userName" (ANil, ())
    , userBestFriend = M.empty
    , userFriends    = M.empty
    , userFingers    = M.empty
    }

getUserTestJson :: BL.ByteString
getUserTestJson =
  "{\"query\":{\"getUser\":{\"userName\":\"Jonathan\",\"userId\":\"1\",\"userBestFriend\":{\"userName\":\"Sandy\"}}}}"

mutateUserTestJson :: BL.ByteString
mutateUserTestJson =
  "{\"mutation\":{\"mutateUser\":{\"userName\":\"Jonathan\",\"userId\":\"1\",\"userBestFriend\":{\"userName\":\"Sandy\"}}}}"

------------------------------------------------------------------------------------------
-- | Tests

spec :: Spec
spec = describe "server" $ do
  describe "Query" $ do
    describe "JSON encoding responses" $ do
      it "should encode a response for getUser as JSON" $ do
        response <- resolve gqlQueryResolver $ getUserTestJsonQuery
        (encode $ jsonResponse response) `shouldBe` getUserTestJson
      it "should encode a response for getAllUsers as JSON" $ do
        response <- resolve gqlQueryResolver $ getAllUsersTestJsonQuery
        (encode $ jsonResponse response) `shouldBe` getAllUsersTestJson

  describe "Mutation" $ do
    describe "JSON encoding responses" $ do
      it "should encode a response for mutateUser as JSON" $ do
        response <- resolve gqlMutationResolver $ getUserTestJsonMutation
        (encode $ jsonResponse response) `shouldBe` mutateUserTestJson
      it "should encode a response for mutateAllUsers as JSON" $ do
        response <- resolve gqlMutationResolver $ getAllUsersTestJsonMutation
        (encode $ jsonResponse response) `shouldBe` mutateAllUsersTestJson

