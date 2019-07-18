module Weft.Generics.EmptyQuery
  ( emptyQuery
  , HasEmptyQuery
  ) where

import Weft.Internal.Types
import GHC.Generics


type HasEmptyQuery record =
  ( GEmptyQuery (Rep (record 'Query))
  , Generic (record 'Query)
  )

emptyQuery :: forall record . HasEmptyQuery record => record 'Query
emptyQuery = to $ gEmptyQuery @(Rep (record 'Query))

class GEmptyQuery (fq :: * -> *) where
    gEmptyQuery :: fq x

instance (GEmptyQuery fq, GEmptyQuery gq) => GEmptyQuery (fq :*: gq) where
    gEmptyQuery = (gEmptyQuery @fq) :*: (gEmptyQuery @gq)

instance GEmptyQuery fq => GEmptyQuery (M1 x y fq) where
    gEmptyQuery = M1 $ gEmptyQuery @fq

-- | Q3
instance GEmptyQuery (K1 x Bool) where
    gEmptyQuery = K1 False

-- | Q2
instance GEmptyQuery (K1 x (Maybe a)) where
    gEmptyQuery = K1 Nothing

