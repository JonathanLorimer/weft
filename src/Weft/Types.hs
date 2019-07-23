module Weft.Types
  ( Magic
  , Arg (..)
  , TypeState (..)
  , module Weft.Types
  ) where

import Weft.Internal.Types
import Weft.Generics.AllTypes
import Weft.Generics.EmptyQuery
import Weft.Generics.Hydrate
import Weft.Generics.PprQuery
import Weft.Generics.PprSchema
import Weft.Generics.QueryParser
import Weft.Generics.Resolve
import Weft.Generics.Schema
import GHC.Generics

type Wefty record =
  ( HasAllTypes record
  , HasEmptyQuery record
  , HasHydrate record
  , HasPprQuery record
  , HasPprSchema record
  , HasQueryParser record
  , HasResolve record
  , HasSchema record
  )

data Gql q m s (ts :: TypeState) = Gql
  { query        :: Magic ts (q ts)
  -- , mutation     :: m ts
  -- , subscription :: s ts
  }
  deriving Generic

deriving instance (Show (q 'Query)) => Show (Gql q m s 'Query)

