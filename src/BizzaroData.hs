{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module BizzaroData where

import Text.PrettyPrint.HughesPJ
import Data.Generic.HKD.Types
import Data.Aeson
import Test.QuickCheck hiding (Args)
import Weft.Internal.Types
import Weft.Types
import Weft.Generics.EmptyQuery
import Weft.Generics.PprQuery
import Weft.Generics.PprSchema
import Weft.Generics.Schema


data User' = User'
  { user'Id         :: Arg "id" (Maybe Int) -> ID
  , user'Name       :: String
  , user'BestFriend :: Arg "arg" (Maybe Int) -> User'
  , user'Friends    :: [User']
  , user'Fingers    :: [Finger']
  } deriving (Generic)

data Account' = Account'
  { account'Title :: Arg "title" (Maybe String) -> Int
  } deriving (Generic)

data Finger' = Finger'
  { finger's :: Arg "input" (Maybe MyInputType')
            -> Arg "enum" (Maybe MyEnum')
            -> Account'
  } deriving (Generic)

data MyInputType' = MyInputType'
  { boot's  :: Int
  , heart's :: Bool
  } deriving stock (Generic, Eq, Ord, Show)

instance Arbitrary MyInputType' where
  arbitrary = MyInputType' <$> arbitrary <*> arbitrary

data MyEnum' = One' | Two' deriving (Generic, Eq, Ord, Show)

instance Arbitrary MyEnum' where
  arbitrary = oneof [pure One', pure Two']

foo :: Doc
foo = gPprSchema $ magicSchema @MyInputType'

