{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE NoMonoLocalBinds          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module TestData where

import Weft.Types
import GHC.Generics
import Test.QuickCheck

newtype Id = Id String deriving (Generic, Show, Eq, Ord)
newtype Name = Name String deriving (Generic, Show, Eq, Ord)

data User ts = User
  { userId         :: Magic ts (Arg "arg" (Maybe String) -> Id)
  , userName       :: Magic ts Name
  , userBestFriend :: Magic ts (Arg "arg" (Maybe String) -> User ts)
  , userFriends    :: Magic ts [User ts]
  } deriving (Generic)

deriving instance AllHave Show (User ts) => Show (User ts)
deriving instance AllHave Eq (User ts)   => Eq (User ts)

data Account ts = Account
  { accountBalance :: Magic ts (Arg "num" (Maybe Int) -> Int)
  } deriving (Generic)

deriving instance AllHave Show (Account ts) => Show (Account ts)
deriving instance AllHave Eq (Account ts)   => Eq (Account ts)


instance Arbitrary (Account 'Query) where
  arbitrary = Account <$> arbitrary

-- TODO(sandy): lets do THIS generically too!!
instance Arbitrary (User 'Query) where
  arbitrary = sized $ \case
    0 -> pure $ User Nothing Nothing Nothing Nothing
    n -> let smaller = resize (n - 1) arbitrary
          in User <$> smaller <*> smaller <*> smaller <*> smaller

