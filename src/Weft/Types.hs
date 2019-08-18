{-# LANGUAGE DeriveAnyClass #-}

module Weft.Types
  ( Magic
  , Arg (..)
  , TypeState (..)
  , module Weft.Types
  , module Weft.Generics.RecordGen
  , Generic
  , Gql (..)
  , AllHave
  , ID (..)
  , None (..)
  ) where

import GHC.Generics
import Weft.Generics.Hydrate
import Weft.Generics.PprSchema
import Weft.Generics.RecordGen
import Weft.Generics.Resolve
import Weft.Generics.Schema
import Weft.Internal.Types hiding (query)

type Wefty record =
  ( HasSchema record
  , HasHydrate record
  , HasResolve record
  , HasPprSchema record
  , HasPprSchema record
  )

