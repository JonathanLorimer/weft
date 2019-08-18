{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module TestData where

import Test.QuickCheck hiding (Args)
import Weft.Internal.Types
import Weft.Types


data User = User
  { userId         :: Method '[ '("arg", Maybe Int) ] ID
  , userName       :: String
  , userBestFriend :: Method '[ '("arg", Maybe Int) ] User
  , userFriends    :: [User]
  , userFingers    :: [Finger]
  } deriving (Generic)

data Account = Account
  { accountTitle :: Method '[ '("title", Maybe String) ] Int
  } deriving (Generic)

data Finger = Finger
  { fingers :: Method '[ '("input", Maybe MyInputType)
                       , '("enum", Maybe MyEnum)
                       ] Account
  } deriving (Generic)

data MyInputType = MyInputType
  { boots  :: Int
  , hearts :: Bool
  } deriving stock (Generic, Eq, Ord, Show)

-- TODO(sandy): make args use recordgen arbitrary
instance Arbitrary MyInputType where
  arbitrary = MyInputType <$> arbitrary <*> arbitrary

data MyEnum = One | Two deriving (Generic, Eq, Ord, Show)

instance Arbitrary MyEnum where
  arbitrary = oneof [pure One, pure Two]

jonathan :: User
jonathan = User
  { userId = Method $ ID "1"
  , userName =  "Jonathan"
  , userBestFriend = Method sandy
  , userFriends = []
  , userFingers = []
  }

sandy :: User
sandy = User
  { userId = Method $ ID "2"
  , userName = "Sandy"
  , userBestFriend = Method jonathan
  , userFriends = []
  , userFingers = []
  }

