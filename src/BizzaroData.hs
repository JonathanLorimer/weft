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
  { user'Id         :: Arg "arg" (Maybe Int)
                    -> ID
  , user'Name       :: String
  , user'BestFriend :: Arg "arg" (Maybe Int)
                    -> User'
  , user'Friends    :: [User']
  , user'Fingers    :: [Finger']
  } deriving (Generic)

data Account' = Account'
  { account'Title :: Arg "title" (Maybe String)
                  -> Int
  } deriving (Generic)

data Finger' = Finger'
  { fingers :: Arg "input" (Maybe MyInputType')
            -> Arg "enum" (Maybe MyEnum')
            -> Account'
  } deriving (Generic)

data MyInputType' = MyInputType'
  { boots  :: Int
  , hearts :: Bool
  } deriving stock (Generic, Eq, Ord, Show)

data MyEnum' = One | Two deriving (Generic, Eq, Ord, Show)

foo :: Doc
foo = gPprSchema $ gSchema @(HKD_ (ToMagic 'Data) MyInputType') @(HKD_ (ToMagic 'Schema) MyInputType')

