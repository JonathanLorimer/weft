{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NoMonoLocalBinds           #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DerivingStrategies        #-}

module ServerSpec where

import Test.Hspec hiding (Arg)
import Server
import Weft.Types
import Weft.Internal.Types
import Weft.Generics.Resolve
import Weft.Generics.Hydrate
import Data.Aeson
import GHC.Generics
import Test.QuickCheck hiding (Args)
import Data.ByteString.Char8

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

deriving instance AllHave Eq (GqlQuery ts)       => Eq (GqlQuery ts)

data User ts = User
  { userId         :: Magic ts (Arg "arg" (Maybe Int) -> Id)
  , userName       :: Magic ts Name
  , userBestFriend :: Magic ts (Arg "arg" (Maybe Int) -> User ts)
  , userFriends    :: Magic ts [User ts]
  } deriving (Generic)

deriving instance AllHave Show (User ts)     => Show (User ts)
deriving instance AllHave Eq (User ts)       => Eq (User ts)
deriving instance AllHave ToJSON (User ts)   => ToJSON (User ts)

getAllUsersTestString :: ByteString
getAllUsersTestString = "query { \n getAllUsers { \n userId \n userName \n userFriends { \n userId \n userName \n } \n } \n } \n "

getAllUsersTestQuery :: Either String (Gql GqlQuery () () 'Query)
getAllUsersTestQuery = Right (Gql { query = Just (ANil
                                             , GqlQuery { getUser = Nothing
                                                        , getAllUsers = Just (ANil
                                                                             , User { userId = Just (Arg Nothing :@@ ANil ,())
                                                                                    , userName = Just (ANil ,())
                                                                                    , userBestFriend = Nothing
                                                                                    , userFriends = Just (ANil
                                                                                                         , User { userId = Just (Arg Nothing :@@ ANil ,())
                                                                                                                , userName = Just (ANil ,())
                                                                                                                , userBestFriend = Nothing
                                                                                                                , userFriends = Nothing }
                                                                                                         )
                                                                                    }
                                                                             )
                                                        }
                                             )
                              }
                         )

getUserTestString :: ByteString
getUserTestString = " query { \n getUser(id: 1) { \n userId \n userName \n userBestFriend { \n userName \n } \n } \n } \n "

getUserTestQuery :: Either String (Gql GqlQuery () () 'Query)
getUserTestQuery = Right (Gql { query = Just (ANil
                                             , GqlQuery { getAllUsers = Nothing
                                                        , getUser = Just (Arg (Id 1) :@@ ANil
                                                                             , User { userId = Just (Arg Nothing :@@ ANil ,())
                                                                                    , userName = Just (ANil ,())
                                                                                    , userBestFriend = Just (Arg Nothing :@@ ANil
                                                                                                            , User { userId = Nothing
                                                                                                                   , userName = Just (ANil ,())
                                                                                                                   , userBestFriend = Nothing
                                                                                                                   , userFriends = Nothing 
                                                                                                                   }
                                                                                                            )
                                                                                    , userFriends = Nothing
                                                                                    }
                                                                             )
                                                        }
                                             )
                              }
                         )


deriving instance Show (GqlQuery 'Response)
deriving instance ToJSON (GqlQuery 'Response)
deriving instance Show (GqlQuery 'Query)

instance Arbitrary (User 'Query) where
  arbitrary = recordGen

instance Arbitrary (User 'Data) where
  arbitrary = recordGen

spec :: Spec
spec = describe "server" $ do
    describe "parseReqBody" $ do
        it "should parse query with no args (getUser)" $
            parseReqBody getAllUsersTestString `shouldBe` getAllUsersTestQuery
        it "should parse query with args (getAllUsers)" $
            parseReqBody getUserTestString `shouldBe` getUserTestQuery

