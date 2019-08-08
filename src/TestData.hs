{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module TestData where

import Data.Aeson
import Test.QuickCheck hiding (Args)
import Weft.Internal.Types
import Weft.Types


------------------------------------------------------------------------------------------
-- | Mock Data
data GqlQuery ts = GqlQuery
    { getUser :: Magic ts (Arg "id" ID -> User ts)
    , getAllUsers :: Magic ts [User ts]
    } deriving (Generic)

deriving instance AllHave Eq (GqlQuery ts)   => Eq (GqlQuery ts)
deriving instance AllHave Show (GqlQuery ts) => Show (GqlQuery ts)

data GqlMutation ts = GqlMutation
    { mutateUser :: Magic ts (Arg "id" ID -> User ts)
    , mutateAllUsers :: Magic ts [User ts]
    } deriving (Generic)

deriving instance AllHave Eq (GqlMutation ts)   => Eq (GqlMutation ts)
deriving instance AllHave Show (GqlMutation ts) => Show (GqlMutation ts)

data User ts = User
  { userId         :: Magic ts (Arg "arg" (Maybe Int) -> ID)
  , userName       :: Magic ts String
  , userBestFriend :: Magic ts (Arg "arg" (Maybe Int) -> User ts)
  , userFriends    :: Magic ts [User ts]
  , userFingers    :: Magic ts [Finger ts]
  } deriving (Generic)

deriving instance AllHave Show (User ts) => Show (User ts)
deriving instance AllHave Eq (User ts)   => Eq (User ts)

data Account ts = Account
  { accountTitle :: Magic ts (Arg "title" (Maybe String) -> Int)
  } deriving (Generic)

deriving instance AllHave Show (Account ts)     => Show (Account ts)
deriving instance AllHave Eq (Account ts)       => Eq (Account ts)

data Finger ts = Finger
  { fingers :: Magic ts (Arg "input" (Maybe MyInputType) -> Arg "enum" (Maybe MyEnum) -> Account ts)
  } deriving (Generic)

data MyInputType = MyInputType
  { boots  :: Int
  , hearts :: Bool
  } deriving stock (Generic, Eq, Ord, Show)

instance Arbitrary MyInputType where
  arbitrary = MyInputType <$> arbitrary <*> arbitrary

data MyEnum = One | Two deriving (Generic, Eq, Ord, Show)

instance Arbitrary MyEnum where
  arbitrary = oneof [ pure One, pure Two ]


deriving instance AllHave Show (Finger ts) => Show (Finger ts)
deriving instance AllHave Eq (Finger ts)   => Eq (Finger ts)

instance Arbitrary (Account 'Query) where
  arbitrary = recordGen
  shrink = genericShrink

instance Arbitrary (User 'Query) where
  arbitrary = recordGen
  shrink = genericShrink

instance Arbitrary (Account 'Data) where
  arbitrary = recordGen
  shrink = genericShrink

instance Arbitrary (User 'Data) where
  arbitrary = recordGen
  shrink = genericShrink

instance Arbitrary (Finger 'Data) where
  arbitrary = recordGen
  shrink = genericShrink

instance Arbitrary (Finger 'Query) where
  arbitrary = recordGen
  shrink = genericShrink

jonathan :: User 'Data
jonathan =
  User
    { userId         = ID "1"
    , userName       = "Jonathan"
    , userBestFriend = sandy
    , userFriends    = []
    , userFingers    = []
    }

sandy :: User 'Data
sandy =
  User
    { userId         = ID "2"
    , userName       = "Sandy"
    , userBestFriend = jonathan
    , userFriends    = []
    , userFingers    = []
    }

