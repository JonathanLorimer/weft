module QueryParser where

import Args
import TestData
import GHC.Generics
import SchemaGenerator


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
instance GEmptyQuery (K1 x (Maybe (Args args))) where
    gEmptyQuery = K1 Nothing

-- | Q2
instance GEmptyQuery (K1 x (Maybe (Args args, record 'Query))) where
    gEmptyQuery = K1 Nothing



testEmptyQuery :: User' 'Query
testEmptyQuery = emptyQuery
