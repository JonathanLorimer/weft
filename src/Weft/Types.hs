module Weft.Types
  ( Magic
  , Arg (..)
  , TypeState (..)
  , module Weft.Types
  , module Weft.Generics.RecordGen
  , Generic
  ) where

import Data.Kind
import GHC.Generics
import Weft.Generics.AllTypes
import Weft.Generics.EmptyQuery
import Weft.Generics.Hydrate
import Weft.Generics.PprQuery
import Weft.Generics.PprSchema
import Weft.Generics.QueryParser
import Weft.Generics.RecordGen
import Weft.Generics.Resolve
import Weft.Generics.Schema
import Weft.Internal.Types

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


type AllHave c a = GFields c (Rep a)

type family GFields (c :: * -> Constraint) (f :: * -> *) :: Constraint
type instance GFields c (M1 i d f) = GFields c f
type instance GFields c (f :+: g)  = (GFields c f, GFields c g)
type instance GFields c (f :*: g)  = (GFields c f, GFields c g)
type instance GFields c U1         = ()
type instance GFields c (K1 i a)   = c a

