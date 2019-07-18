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

deriving instance Show (User 'Data)
deriving instance Show (User 'Schema)
deriving instance Show (User 'Response)
deriving instance Show (User 'Query)

userSchema :: User 'Schema
userSchema = schema

userQ :: User 'Query
userQ = User Nothing Nothing Nothing Nothing

