{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE NoMonoLocalBinds            #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module TestData where

import Weft.Internal.Types
import Weft.Generics.Resolve
import Weft.Generics.Hydrate
import Data.Aeson
import GHC.Generics
import Test.QuickCheck

newtype Id = Id String deriving (Generic, Show, Eq, Ord, Read)
newtype Name = Name String deriving (Generic, Show, Eq, Ord)

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

data Account ts = Account
  { accountBalance :: Magic ts (Arg "num" (Maybe Int) -> Int)
  } deriving (Generic)

jonathan :: User 'Data
jonathan = User { userId = (Id "1"), userName = ( Name "Jonathan"), userBestFriend = sandy, userFriends = [] }

sandy :: User 'Data
sandy = User { userId = (Id "2"), userName = ( Name "Sandy"), userBestFriend = jonathan, userFriends = []}

getUserResolver :: (Arg "id" Id) -> User 'Query -> IO (User 'Response)
getUserResolver a q
    | (getArg a) == (Id "1") = pure $ hydrate jonathan q
    | (getArg a) == (Id "2") = pure $ hydrate sandy q
    | otherwise = pure $ hydrate jonathan q

getAllUsersResolver :: User 'Query -> IO ([User 'Response])
getAllUsersResolver q = pure $ (flip hydrate q) <$> [sandy, jonathan]

queryResolver :: GqlQuery 'Resolver
queryResolver = GqlQuery
            { getUser = getUserResolver
            , getAllUsers = getAllUsersResolver
            }

-- resolver = resolve (User @('Resolver)) (User @('Query))
deriving instance Show (GqlQuery 'Response)
deriving instance Show (GqlQuery 'Query)

deriving instance Show (User 'Data)
deriving instance Show (User 'Schema)
deriving instance Show (User 'Response)
deriving instance Show (User 'Query)

deriving instance Show (Account 'Data)
deriving instance Show (Account 'Schema)
deriving instance Show (Account 'Response)
deriving instance Show (Account 'Query)

deriving instance Eq (User 'Data)
deriving instance Eq (User 'Schema)
deriving instance Eq (User 'Query)

deriving instance Eq (Account 'Data)
deriving instance Eq (Account 'Schema)
deriving instance Eq (Account 'Query)

instance Arbitrary (Account 'Query) where
  arbitrary = Account <$> arbitrary

-- TODO(sandy): lets do THIS generically too!!
instance Arbitrary (User 'Query) where
  arbitrary = sized $ \case
    0 -> pure $ User Nothing Nothing Nothing Nothing
    n -> let smaller = resize (n - 1) arbitrary
          in User <$> smaller <*> smaller <*> smaller <*> smaller

