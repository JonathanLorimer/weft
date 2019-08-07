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
newtype Name = Name String
  deriving stock (Generic, Show, Eq, Ord)
  deriving newtype (Arbitrary, ToJSON)

data GqlQuery ts = GqlQuery
    { getUser :: Magic ts (Arg "id" ID -> User ts)
    , getAllUsers :: Magic ts [User ts]
    } deriving (Generic)

deriving instance AllHave Eq (GqlQuery ts)   => Eq (GqlQuery ts)
deriving instance AllHave Show (GqlQuery ts) => Show (GqlQuery ts)
deriving via (NoNothingJSON (GqlQuery 'Response))
  instance AllHave ToJSON (GqlQuery 'Response) => ToJSON (GqlQuery 'Response)

data User ts = User
  { userId         :: Magic ts (Arg "arg" (Maybe Int) -> ID)
  , userName       :: Magic ts Name
  , userBestFriend :: Magic ts (Arg "arg" (Maybe Int) -> User ts)
  , userFriends    :: Magic ts [User ts]
  , userFingers    :: Magic ts [Finger ts]
  } deriving (Generic)

deriving instance AllHave Show (User ts) => Show (User ts)
deriving instance AllHave Eq (User ts)   => Eq (User ts)
deriving via (NoNothingJSON (User 'Response))
  instance AllHave ToJSON (User 'Response) => ToJSON (User 'Response)

data Account ts = Account
  { accountTitle :: Magic ts (Arg "title" (Maybe String) -> Int)
  } deriving (Generic)

deriving instance AllHave Show (Account ts)     => Show (Account ts)
deriving instance AllHave Eq (Account ts)       => Eq (Account ts)
deriving via (NoNothingJSON (Account 'Response))
  instance AllHave ToJSON (Account 'Response) => ToJSON (Account 'Response)

data Finger ts = Finger
  { fingers :: Magic ts (Arg "input" (Maybe MyInputType) -> Account ts)
  } deriving (Generic)

data MyInputType = MyInputType
  { boots  :: Int
  , hearts :: Bool
  } deriving stock (Generic, Eq, Ord, Show)

instance Arbitrary MyInputType where
  arbitrary = MyInputType <$> arbitrary <*> arbitrary

data MyEnum = One | Two deriving (Generic, Eq, Ord, Show)

deriving instance AllHave Show (Finger ts) => Show (Finger ts)
deriving instance AllHave Eq (Finger ts)   => Eq (Finger ts)
deriving via (NoNothingJSON (Finger 'Response))
  instance AllHave ToJSON (Finger 'Response) => ToJSON (Finger 'Response)

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
    , userName       = Name "Jonathan"
    , userBestFriend = sandy
    , userFriends    = []
    , userFingers    = []
    }

sandy :: User 'Data
sandy =
  User
    { userId         = ID "2"
    , userName       = Name "Sandy"
    , userBestFriend = jonathan
    , userFriends    = []
    , userFingers    = []
    }

