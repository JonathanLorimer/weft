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
import Weft.Generics.AllTypes
import Weft.Generics.EmptyQuery
import Weft.Generics.Hydrate
import Weft.Generics.PprQuery
import Weft.Generics.PprSchema
import Weft.Generics.QueryParser
import Weft.Generics.RecordGen
import Weft.Generics.Resolve
import Weft.Generics.Schema
import Weft.Generics.JSONResponse
import Weft.Internal.Types hiding (query)

type Wefty record =
  ( HasAllTypes record
  , HasEmptyQuery record
  , HasHydrate record
  , HasPprQuery record
  , HasPprSchema record
  , HasQueryParser record
  , HasResolve record
  , HasSchema record
  , HasJSONResponse record
  )

