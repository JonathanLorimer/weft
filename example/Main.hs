{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NoMonoLocalBinds           #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE DeriveAnyClass             #-}

module Main where

import Network.Wai.Handler.Warp
import Weft.Server
import Weft.Types
import Weft.Internal.Types
import Weft.Generics.Resolve
import Weft.Generics.Hydrate
import Data.Aeson
import GHC.Generics
import Test.QuickCheck

newtype Id = Id String deriving (Generic, Show, Read, Eq, Ord, Arbitrary, ToJSON)
newtype Name = Name String deriving (Generic, Show, Eq, Ord, Arbitrary, ToJSON)

data GqlQuery ts = GqlQuery
    { getUser :: Magic ts (Arg "id" Id -> User ts)
    , getAllUsers :: Magic ts [User ts]
    } deriving (Generic)

data User ts = User
  { userId         :: Magic ts (Arg "arg" (Maybe String) -> Id)
  , userName       :: Magic ts Name
  , userBestFriend :: Magic ts (Arg "arg" (Maybe String) -> User ts)
  , userFriends    :: Magic ts [User ts]
  } deriving (Generic)

deriving instance AllHave Show (User ts)     => Show (User ts)
deriving instance AllHave Eq (User ts)       => Eq (User ts)
deriving instance AllHave ToJSON (User ts)   => ToJSON (User ts)

data Account ts = Account
  { accountBalance :: Magic ts (Arg "num" (Maybe Int) -> Int)
  } deriving (Generic)

deriving instance AllHave Show (Account ts) => Show (Account ts)
deriving instance AllHave Eq (Account ts)   => Eq (Account ts)

jonathan :: User 'Data
jonathan = User { userId = (Id "1"), userName = ( Name "Jonathan"), userBestFriend = sandy, userFriends = [] }

sandy :: User 'Data
sandy = User { userId = (Id "2"), userName = ( Name "Sandy"), userBestFriend = jonathan, userFriends = []}

getUserResolver :: (Arg "id" Id) -> User 'Query -> IO (User 'Response)
getUserResolver a q
    | (getArg a) == (Id "1") = pure $ hydrate jonathan q
    | (getArg a) == (Id "2") = pure $ hydrate sandy q
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


deriving instance Show (GqlQuery 'Response)
deriving instance ToJSON (GqlQuery 'Response)
deriving instance Show (GqlQuery 'Query)

instance Arbitrary (Account 'Query) where
  arbitrary = recordGen

instance Arbitrary (User 'Query) where
  arbitrary = recordGen

instance Arbitrary (Account 'Data) where
  arbitrary = recordGen

instance Arbitrary (User 'Data) where
  arbitrary = recordGen

main :: IO ()
main = do
    let port = 3000
    let settings = [setPort port]
    Prelude.putStrLn $ "Listening on port " ++ show port
    server settings gqlResolver