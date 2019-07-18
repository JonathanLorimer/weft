module TestData where

import GHC.Generics
import Data.Text
import SchemaGenerator
import Args

newtype Id = Id String deriving (Generic, Show)
newtype Name = Name String deriving (Generic, Show)

data User ts = User
  { userId         :: Magic ts (Arg "arg" (Maybe String) -> Id)
  , userName       :: Magic ts Name
  , userBestFriend :: Magic ts (Arg "arg" (Maybe String) -> User ts)
  , userFriends    :: Magic ts [User ts]
  } deriving (Generic)

data Account ts = Account
  { accountBalance :: Magic ts Int
  } deriving (Generic)

deriving instance Show (User 'Data)
deriving instance Show (User 'Schema)
deriving instance Show (User 'Response)
deriving instance Show (User 'Query)

deriving instance Show (Account 'Data)
deriving instance Show (Account 'Schema)
deriving instance Show (Account 'Response)
deriving instance Show (Account 'Query)

userSchema :: User 'Schema
userSchema = schema

userQ :: User 'Query
userQ = User Nothing Nothing Nothing Nothing

