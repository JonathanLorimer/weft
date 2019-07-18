module TestData where

import GHC.Generics
import Data.Text
import SchemaGenerator
import Args
import Test.QuickCheck

newtype Id = Id String deriving (Generic, Show, Eq, Ord)
newtype Name = Name String deriving (Generic, Show, Eq, Ord)

data User ts = User
  { userId         :: Magic ts (Arg "arg" (Maybe String) -> Id)
  , userName       :: Magic ts Name
  , userBestFriend :: Magic ts (Arg "arg" (Maybe String) -> User ts)
  , userFriends    :: Magic ts [User ts]
  } deriving (Generic)

data Account ts = Account
  { accountBalance :: Magic ts (Arg "num" Int -> Int)
  } deriving (Generic)

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

instance Arbitrary (User 'Query) where
  arbitrary = User <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

userSchema :: User 'Schema
userSchema = schema

userQ :: User 'Query
userQ = User Nothing Nothing Nothing Nothing

